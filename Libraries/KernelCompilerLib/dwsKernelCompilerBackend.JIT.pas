{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Eric Grange.        }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsKernelCompilerBackend.JIT;

interface

{$POINTERMATH ON}

uses
   System.Classes, System.SysUtils, System.Generics.Collections, System.Math,
   Winapi.Windows,
   dwsUtils, dwsKernelCompilerCommon;

type
   TKCLMicroKernel = procedure(pIn, pOut, pWeights, pBias: PDouble; totalPixels: NativeInt);

   TKCLCompiledKernel = class
   public
      Code : Pointer;
      Size : NativeInt;
      Weights : TDoubleDynArray; // Transposed weights [tile, k, channel_in_tile]
      constructor Create(ACode : Pointer; ASize : NativeInt);
      destructor Destroy; override;
   end;

   TKCLOptimizationData = class
   public
      CompiledKernels : TDictionary<TKCLNode, TKCLCompiledKernel>;
      constructor Create;
      destructor Destroy; override;
   end;

   TKCLWin64JITBackend = class
   public
      class function Compile(AKernel : TKCLKernel; ANode : TKCLNode; inC, outC : Integer) : TKCLCompiledKernel;
      class function Execute(AKernel : TKCLKernel; ANode : TKCLNode; pIn, pOut, pWeights, pBias: PDouble; totalPixels: NativeInt; inC, outC : Integer) : Boolean;
   end;

implementation

// ------------------
// ------------------ TKCLCompiledKernel ------------------
// ------------------

constructor TKCLCompiledKernel.Create(ACode : Pointer; ASize : NativeInt);
begin
   Code := ACode;
   Size := ASize;
end;

destructor TKCLCompiledKernel.Destroy;
begin
   if Code <> nil then VirtualFree(Code, 0, MEM_RELEASE);
   inherited;
end;

// ------------------
// ------------------ TKCLOptimizationData ------------------
// ------------------

constructor TKCLOptimizationData.Create;
begin
   CompiledKernels := TDictionary<TKCLNode, TKCLCompiledKernel>.Create;
end;

destructor TKCLOptimizationData.Destroy;
var
   compiled : TKCLCompiledKernel;
begin
   for compiled in CompiledKernels.Values do
      compiled.Free;
   CompiledKernels.Free;
   inherited;
end;

// ------------------
// ------------------ TKCLWin64JITBackend ------------------
// ------------------

class function TKCLWin64JITBackend.Compile(AKernel : TKCLKernel; ANode : TKCLNode; inC, outC : Integer) : TKCLCompiledKernel;
var
   ms : TMemoryStream;
   tileCount, tile, curTileSize, i : Integer;
   ptr : Pointer;
   pixelLoopPos, kLoopPos, epiloguePos, jmpEpiloguePos : Int64;

   procedure Emit(b : Byte);
   begin
      ms.Write(b, 1);
   end;

   procedure EmitBytes(const b : array of Byte);
   begin
      ms.Write(b[0], Length(b));
   end;

   procedure EmitD(d : Integer);
   begin
      ms.Write(d, 4);
   end;

   procedure EmitModRM(mod_, reg, rm : Byte);
   begin
      Emit((mod_ shl 6) or ((reg and 7) shl 3) or (rm and 7));
   end;

begin
   ms := TMemoryStream.Create;
   try
      // Prologue
      Emit($55); EmitBytes([$48, $89, $E5]); // push rbp; mov rbp, rsp
      
      // Save shadow space
      EmitBytes([$48, $83, $EC, $20]); // sub rsp, 32

      // Use R10 for totalPixels
      EmitBytes([$4C, $8B, $55, $30]); // mov r10, [rbp+48]
      
      // Zero-pixel check
      EmitBytes([$4D, $85, $D2]); // test r10, r10
      EmitBytes([$0F, $84]); // jz (rel32)
      jmpEpiloguePos := ms.Position;
      EmitD(0);

      pixelLoopPos := ms.Position;
      tileCount := (outC + 7) div 8;
      for tile := 0 to tileCount - 1 do begin
         curTileSize := Min(8, outC - tile * 8);
         
         // Accumulators: XMM0-XMM3 (using pairs for 8 channels)
         // Load Bias
         for i := 0 to (curTileSize div 2) - 1 do begin
            Emit($66); Emit($41); Emit($0F); Emit($10); EmitModRM($02, i, 1); EmitD((tile * 8 + i * 2) * 8); // XMMi = Bias
         end;
         if (curTileSize mod 2) <> 0 then begin
            Emit($F2); Emit($41); Emit($0F); Emit($10); EmitModRM($02, 4, 1); EmitD((tile * 8 + curTileSize - 1) * 8);
         end;

         // K loop over input channels
         EmitBytes([$4D, $89, $C3]); // mov r11, r8 (Weights pointer)
         if tile > 0 then begin EmitBytes([$49, $81, $C3]); EmitD(tile * 8 * inC * 8); end;
         
         EmitBytes([$48, $31, $C0]); // xor rax, rax (k = 0)
         kLoopPos := ms.Position;
         
         // Load input value into XMM5 and broadcast: MOVSD XMM5, [RCX + RAX*8]
         EmitBytes([$F2, $0F, $10, $2C]); Emit($C1); 
         EmitBytes([$66, $0F, $14, $ED]); // UNPCKLPD XMM5, XMM5

         // Accumulate using contiguous weights
         for i := 0 to (curTileSize div 2) - 1 do begin
            Emit($66); Emit($41); Emit($0F); Emit($10); EmitModRM($02, 6, 3); EmitD(i * 2 * 8); // XMM6 = Weights from R11
            Emit($66); Emit($0F); Emit($59); EmitModRM($03, 6, 5); // XMM6 *= XMM5
            Emit($66); Emit($0F); Emit($58); EmitModRM($03, i, 6); // XMMi += XMM6
         end;
         if (curTileSize mod 2) <> 0 then begin
            Emit($F2); Emit($41); Emit($0F); Emit($10); EmitModRM($02, 6, 3); EmitD((curTileSize - 1) * 8);
            Emit($F2); Emit($0F); Emit($59); EmitModRM($03, 6, 5);
            Emit($F2); Emit($0F); Emit($58); EmitModRM($03, 4, 6);
         end;

         EmitBytes([$49, $83, $C3, $40]); // next weights
         EmitBytes([$48, $FF, $C0]); // inc rax
         EmitBytes([$48, $81, $F8]); EmitD(inC); EmitBytes([$0F, $82]); EmitD(kLoopPos - (ms.Position + 4));

         // Store results
         EmitBytes([$48, $89, $D0]); // mov rax, rdx
         if tile > 0 then begin EmitBytes([$48, $05]); EmitD(tile * 8 * 8); end;
         for i := 0 to (curTileSize div 2) - 1 do begin
            Emit($66); Emit($0F); Emit($11); EmitModRM($02, i, 0); EmitD(i * 2 * 8);
         end;
         if (curTileSize mod 2) <> 0 then begin
            Emit($F2); Emit($0F); Emit($11); EmitModRM($02, 4, 0); EmitD((curTileSize - 1) * 8);
         end;
      end;

      // Pixel loop increments
      EmitBytes([$48, $81, $C1]); EmitD(inC * 8); // rcx += inC*8
      EmitBytes([$48, $81, $C2]); EmitD(outC * 8); // rdx += outC*8
      
      EmitBytes([$4C, $8B, $55, $30]); EmitBytes([$49, $FF, $CA]); EmitBytes([$4C, $89, $55, $30]); // totalPixels -= 1
      EmitBytes([$0F, $85]); EmitD(pixelLoopPos - (ms.Position + 4));

      // Epilogue
      epiloguePos := ms.Position;
      ms.Position := jmpEpiloguePos; EmitD(epiloguePos - (jmpEpiloguePos + 4));
      ms.Position := epiloguePos;

      EmitBytes([$48, $83, $C4, $20]); // add rsp, 32
      Emit($5D); // pop rbp
      Emit($C3); // ret

      ptr := VirtualAlloc(nil, ms.Size, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
      Move(ms.Memory^, ptr^, ms.Size);
      Result := TKCLCompiledKernel.Create(ptr, ms.Size);
   finally
      ms.Free;
   end;
end;

class function TKCLWin64JITBackend.Execute(AKernel : TKCLKernel; ANode : TKCLNode; pIn, pOut, pWeights, pBias: PDouble; totalPixels: NativeInt; inC, outC : Integer) : Boolean;
begin
   Result := False;
   TMonitor.Enter(AKernel);
   try
      var optData := TKCLOptimizationData(AKernel.OptimizationData);
      if optData = nil then begin
         optData := TKCLOptimizationData.Create; AKernel.OptimizationData := optData;
      end;
      var compiled : TKCLCompiledKernel;
      if not optData.CompiledKernels.TryGetValue(ANode, compiled) then begin
         compiled := Compile(AKernel, ANode, inC, outC);
         if compiled <> nil then begin
            SetLength(compiled.Weights, ((outC + 7) div 8) * inC * 8);
            for var t := 0 to ((outC + 7) div 8) - 1 do begin
               for var k := 0 to inC - 1 do begin
                  for var c := 0 to 7 do begin
                     var outChan := t * 8 + c;
                     var val : Double := 0;
                     if outChan < outC then val := pWeights[outChan + k * outC];
                     compiled.Weights[(t * inC * 8) + (k * 8) + c] := val;
                  end;
               end;
            end;
            optData.CompiledKernels.Add(ANode, compiled);
         end;
      end;
      if compiled <> nil then begin
         var micro := TKCLMicroKernel(compiled.Code);
         micro(pIn, pOut, @compiled.Weights[0], pBias, totalPixels);
         Result := True;
      end;
   finally TMonitor.Exit(AKernel); end;
end;

end.
