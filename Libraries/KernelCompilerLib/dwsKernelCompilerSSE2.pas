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
unit dwsKernelCompilerSSE2;

{$I dws.inc}

interface

uses
   dwsSymbols
{$IFDEF WIN64_ASM}
   , System.Classes, System.SysUtils,
   dwsUnitSymbols, dwsComp, dwsExprs, dwsFunctions,
   dwsKernelCompilerCommon, dwsKernelCompiler, dwsKernelCompilerBackend.SSE2
{$ENDIF}
   ;

{$IFDEF WIN64_ASM}

type
   TSSE2CompilerDispatchMethod = class(TKernelCompilerDispatchMethod)
   protected
      procedure ExecuteDispatch(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor); override;
   end;

{$ENDIF}

procedure RegisterSSE2CompilerSymbols(unitTable : TSymbolTable);

implementation

{$IFDEF WIN64_ASM}

// ------------------
// ------------------ TSSE2CompilerDispatchMethod ------------------
// ------------------

procedure TSSE2CompilerDispatchMethod.ExecuteDispatch(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
begin
   var backend := TKCLSSE2Backend.Create;
   try
      backend.Execute(AKernel, ABuffers);
   finally
      backend.Free;
   end;
end;

{$ENDIF}

// RegisterSSE2CompilerSymbols
//
procedure RegisterSSE2CompilerSymbols(unitTable : TSymbolTable);
begin
{$IFDEF WIN64_ASM}
   var clsBase := unitTable.FindSymbol(SYS_KCL_KERNELCOMPILER, cvMagic) as TClassSymbol;
   if clsBase = nil then
      raise Exception.Create('KCL: Base compiler class not found.');

   var clsSSE2 := TClassSymbol.Create('TKCLSSE2Compiler', clsBase);
   unitTable.AddSymbol(clsSSE2);

   TSSE2CompilerDispatchMethod.Create(unitTable, 'Dispatch', ['kernel', SYS_KCL_KERNEL, 'buffers', 'array of ' + SYS_KCL_STRIDEDBUFFER], '', [iffStaticMethod], clsSSE2);
{$ENDIF}
end;

end.
