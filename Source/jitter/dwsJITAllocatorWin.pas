{**************************************************************************}
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{     file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Copyright Eric Grange / Creative IT                                   }
{                                                                          }
{**************************************************************************}
unit dwsJITAllocatorWin;

{$I ../dws.inc}

interface

uses
   Windows, Classes, SysUtils,
   dwsUtils, dwsXPlatform, dwsJIT;

const
   cJITSubAllocatorSize = 64*1024; // 16 kB

type
   TdwsJITCodeSubAllocator = class (TInterfacedObject, IdwsJITCodeSubAllocator)
      private
         FBlock : Pointer;
         FCurrent : PByte;
         FBlockSize : Integer;
         FRemaining : Integer;
         FNext : IdwsJITCodeSubAllocator;
         FInitialProtect, FFinalProtect : Cardinal;
         FRuntimeFunctionTables : array of Pointer;
         {$ifdef WIN64}
         FRuntimeFunctionCallbackRegistered : Boolean;
         {$endif}
         FUseRtFnCallback : Boolean;

      protected
         function GetNext : IdwsJITCodeSubAllocator;
         procedure SetNext(const aNext : IdwsJITCodeSubAllocator);

         procedure RegisterFunctionTableCallback;
         procedure DeRegisterFunctionTableCallback;

      public
         constructor Create(aSize : Integer; protect : Boolean; useRtFnCallback : Boolean);
         destructor Destroy; override;

         function BlockSize : Integer; inline;
         property Remaining : Integer read FRemaining;

         function Allocate(aSize : Integer) : Pointer;

         procedure Protect;
         procedure Reset;

         procedure RegisterFunctionTable(rtFn : Pointer);
         procedure DeregisterFunctionTable(rtFn : Pointer);
   end;

   TdwsJITAllocatorWin = class
      private
         FSubAllocators : IdwsJITCodeSubAllocator;
         FSpareSubAllocator : IdwsJITCodeSubAllocator;
         FSubAllocatorSize : Integer;
         FSubAllocatorProtect : Boolean;
         FSubAllocatorUseRtFnCallback : Boolean;

      protected

      public
         constructor Create;
         destructor Destroy; override;

         function Allocate(const code : TBytes; var aSubAllocator : IdwsJITCodeSubAllocator) : Pointer;

         procedure DetachSubAllocators;

         procedure ReturnSpare(const spare : IdwsJITCodeSubAllocator);

         property SubAllocatorSize : Integer read FSubAllocatorSize write FSubAllocatorSize;
         property SubAllocatorProtect : Boolean read FSubAllocatorProtect write FSubAllocatorProtect;
         property SubAllocatorUseRtFnCallback : Boolean read FSubAllocatorUseRtFnCallback write FSubAllocatorUseRtFnCallback;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$ifdef WIN64}
uses dwsWin64Functiontables;

function FunctionTableCallback(ControlPC : DWORD64; Context : Pointer) : PRUNTIME_FUNCTION;
var
   subContext : TdwsJITCodeSubAllocator;
   i : Integer;
   relativePC : DWORD;
begin
   subContext := TdwsJITCodeSubAllocator(Context);
   if subContext.FBlock <> nil then begin
      relativePC := ControlPC - DWORD64(subContext.FBlock);
      for i := 0 to High(subContext.FRuntimeFunctionTables) do begin
         Result := subContext.FRuntimeFunctionTables[i];
         if (relativePC >= Result.BeginAddress) and (relativePC <= Result.EndAddress) then
            Exit
      end;
   end;
   Result := nil;
end;
{$endif}

// ------------------
// ------------------ TdwsJITCodeSubAllocator ------------------
// ------------------

// Create
//
constructor TdwsJITCodeSubAllocator.Create(aSize : Integer; protect : Boolean; useRtFnCallback : Boolean);
begin
   FBlockSize := aSize;
   FUseRtFnCallback := useRtFnCallback;
   if protect then begin
      FInitialProtect := PAGE_READWRITE;
      FFinalProtect := PAGE_EXECUTE_READ;
   end else begin
      FInitialProtect := PAGE_EXECUTE_READWRITE;
      FFinalProtect := FInitialProtect;
   end;
   FBlock := VirtualAlloc(nil, aSize, MEM_COMMIT, FInitialProtect);
   Reset;
end;

// Destroy
//
destructor TdwsJITCodeSubAllocator.Destroy;
begin
   DeRegisterFunctionTableCallback;
   VirtualFree(FBlock, 0, MEM_RELEASE);
   FBlock := nil;
   inherited;
end;

// BlockSize
//
function TdwsJITCodeSubAllocator.BlockSize : Integer;
begin
   Result := FBlockSize;
end;

// Allocate
//
function TdwsJITCodeSubAllocator.Allocate(aSize : Integer) : Pointer;
begin
   // round up to multiple of 16
   aSize := ((aSize + 15) shr 4) shl 4;

   if aSize > FRemaining then
      Exit(nil);

   Result:=FCurrent;
   Inc(FCurrent, aSize);
   Dec(FRemaining, aSize);
   if FRemaining<64 then
      Protect;
end;

// Protect
//
procedure TdwsJITCodeSubAllocator.Protect;
var
   oldProtect : Integer;
   n : Integer;
begin
   if FRemaining>16 then
      n:=16
   else n:=FRemaining;
   // pad a small range with nops after last code
   FillChar(FCurrent^, n, $90);
   FRemaining := 0;
   if FFinalProtect <> FInitialProtect then
      VirtualProtect(FBlock, FBlockSize, FFinalProtect, @oldProtect);
end;

// Reset
//
procedure TdwsJITCodeSubAllocator.Reset;
var
   oldProtect : Integer;
begin
   FRemaining := FBlockSize;
   FCurrent := FBlock;
   if FFinalProtect <> FInitialProtect then
      VirtualProtect(FBlock, FBlockSize, FInitialProtect, @oldProtect);
   SetLength(FRuntimeFunctionTables, 0);
end;

// RegisterFunctionTableCallback
//
procedure TdwsJITCodeSubAllocator.RegisterFunctionTableCallback;
begin
   {$ifdef WIN64}
   if not FRuntimeFunctionCallbackRegistered then begin
      if not RtlInstallFunctionTableCallback(
                  NativeUInt(FBlock) or 3, NativeUInt(FBlock), FBlockSize,
                  @FunctionTableCallback, Self, nil) then
         RaiseLastOSError;
      FRuntimeFunctionCallbackRegistered := True;
   end;
   {$endif}
end;

// DeRegisterFunctionTableCallback
//
procedure TdwsJITCodeSubAllocator.DeRegisterFunctionTableCallback;
begin
   {$ifdef WIN64}
   if FRuntimeFunctionCallbackRegistered then begin
      if not RtlDeleteFunctionTable(Pointer(NativeUInt(FBlock) or 3)) then
         RaiseLastOSError;
      FRuntimeFunctionCallbackRegistered := False;
   end;
   {$endif}
end;

// RegisterFunctionTable
//
procedure TdwsJITCodeSubAllocator.RegisterFunctionTable(rtFn : Pointer);
{$ifdef WIN64}
var
   n : Integer;
begin
   if not FUseRtFnCallback then begin
      if not RtlAddFunctionTable(rtFn, 1, rtFn) then
         RaiseLastOSError;
   end else begin
      RegisterFunctionTableCallback;
      n := Length(FRuntimeFunctionTables);
      SetLength(FRuntimeFunctionTables, n+1);
      FRuntimeFunctionTables[n] := rtFn;
      // make offsets relative to the block base, as that will be the BaseAddress
      var delta := NativeUInt(rtFn) - NativeUInt(FBlock);
      Inc(PRUNTIME_FUNCTION(rtFn).BeginAddress, delta);
      Inc(PRUNTIME_FUNCTION(rtFn).EndAddress, delta);
      Inc(PRUNTIME_FUNCTION(rtFn).UnwindInfoAddress, delta);
   end;
end;
{$else}
begin
end;
{$endif}

// DeregisterFunctionTable
//
procedure TdwsJITCodeSubAllocator.DeregisterFunctionTable(rtFn : Pointer);
{$ifdef WIN64}
var
   i : Integer;
begin
   if not FUseRtFnCallback then begin
      RtlDeleteFunctionTable(rtFn);
   end else begin
      for i := 0 to High(FRuntimeFunctionTables) do begin
         if FRuntimeFunctionTables[i] = rtFn then begin
            Delete(FRuntimeFunctionTables, i, 1);
            Exit;
         end;
      end;
      raise Exception.CreateFmt('FunctionTable not found ($%x)', [ NativeUInt(rtFn) ]);
   end;
end;
{$else}
begin
end;
{$endif}

// GetNext
//
function TdwsJITCodeSubAllocator.GetNext : IdwsJITCodeSubAllocator;
begin
   Result := FNext;
end;

// SetNext
//
procedure TdwsJITCodeSubAllocator.SetNext(const aNext : IdwsJITCodeSubAllocator);
begin
   FNext := aNext;
end;

// ------------------
// ------------------ TdwsJITAllocatorWin ------------------
// ------------------

// Create
//
constructor TdwsJITAllocatorWin.Create;
begin
   inherited Create;
   FSubAllocatorSize := cJITSubAllocatorSize;
   FSubAllocatorProtect := True;
   FSubAllocatorUseRtFnCallback := True;
end;

// Destroy
//
destructor TdwsJITAllocatorWin.Destroy;
begin
   inherited;
   DetachSubAllocators;
end;

// Allocate
//
function TdwsJITAllocatorWin.Allocate(const code : TBytes; var aSubAllocator : IdwsJITCodeSubAllocator) : Pointer;
var
   sub : IdwsJITCodeSubAllocator;
   n, s : Integer;
   p : Pointer;
begin
   n:=Length(code);
   Assert(n>0);
   p:=nil;
   sub:=FSubAllocators;
   while sub<>nil do begin
      p:=sub.Allocate(n);
      if p<>nil then break;
   end;
   if p = nil then begin
      if n > SubAllocatorSize then
         s := n+16
      else s := SubAllocatorSize;
      if (FSpareSubAllocator <> nil) and (FSpareSubAllocator.BlockSize >= s) then begin
         PPointer(@sub)^ := Pointer(FSpareSubAllocator);
         PPointer(@FSpareSubAllocator)^ := nil;
      end else begin
         sub := TdwsJITCodeSubAllocator.Create(s, SubAllocatorProtect, SubAllocatorUseRtFnCallback);
      end;
      sub.Next := FSubAllocators;
      FSubAllocators := sub;
      p := sub.Allocate(n);
   end;

   System.Move(code[0], p^, n);
   Result := p;
   aSubAllocator := sub;
end;

// DetachSubAllocators
//
procedure TdwsJITAllocatorWin.DetachSubAllocators;
var
   sub : IdwsJITCodeSubAllocator;
begin
   while FSubAllocators<>nil do begin
      sub := FSubAllocators;
      FSubAllocators := sub.Next;
      sub.Protect;
   end;
end;

// ReturnSpare
//
procedure TdwsJITAllocatorWin.ReturnSpare(const spare : IdwsJITCodeSubAllocator);
begin
   Assert(spare.Next = nil, 'Cannot return a chained suballocator as spare');
   if FSpareSubAllocator <> nil then Exit;
   if spare.BlockSize < SubAllocatorSize then Exit;
   FSpareSubAllocator := spare;
   spare.Reset;
end;

end.
