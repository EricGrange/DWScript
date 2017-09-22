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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsByteBufferFunctions;

{$I dws.inc}

interface

uses
   dwsXPlatform, dwsUtils, dwsStrings, dwsCompilerContext, dwsDataContext,
   dwsSymbols, dwsFunctions, dwsUnitSymbols, dwsOperators, dwsExprs,
   dwsMagicExprs, dwsExprList,
   dwsByteBuffer;

const
   SYS_BYTEBUFFER = 'ByteBuffer';

type

   TBaseByteBufferSymbol = class (TBaseSymbol)
      public
         constructor Create;

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;
   end;

   TByteBufferUnaryOpExpr = class (TUnaryOpExpr)
      public
         constructor Create(context : TdwsCompilerContext; expr : TTypedExpr); override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TConvStringToByteBufferExpr = class(TByteBufferUnaryOpExpr)
      procedure EvalAsInterface(exec : TdwsExecution; var result : IUnknown); override;
   end;

   TByteBufferGetLengthFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetLengthFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetPositionFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetPositionFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetByteFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetByteFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetWordFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetWordFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetInt16Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetInt16Func = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetDWordFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetDWordFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetInt32Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetInt32Func = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RegisterByteBufferType
//
procedure RegisterByteBufferType(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                           unitTable : TSymbolTable);
var
   typByteBuffer : TBaseByteBufferSymbol;
begin
   if systemTable.FindLocal(SYS_BYTEBUFFER) <> nil then exit;

   typByteBuffer := TBaseByteBufferSymbol.Create;

   systemTable.AddSymbol(typByteBuffer);
end;

// RegisterByteBufferOperators
//
procedure RegisterByteBufferOperators(systemTable : TSystemSymbolTable;
                                unitTable : TSymbolTable; operators : TOperators);
var
   typByteBuffer : TBaseByteBufferSymbol;
begin
   typByteBuffer := systemTable.FindTypeSymbol(SYS_BYTEBUFFER, cvMagic) as TBaseByteBufferSymbol;

   if operators.FindCaster(typByteBuffer, systemTable.TypString) <> nil then Exit;

   operators.RegisterCaster(typByteBuffer, systemTable.TypString, TConvStringToByteBufferExpr);
end;

type
   TExprBaseListExecHelper = record helper for TExprBaseListExec
      procedure GetBuffer(var buffer : IdwsByteBuffer); inline;
   end;

// ------------------
// ------------------ TBaseBigIntegerSymbol ------------------
// ------------------

// Create
//
constructor TBaseByteBufferSymbol.Create;
begin
   inherited Create(SYS_BYTEBUFFER);
end;

// IsCompatible
//
function TBaseByteBufferSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym<>nil) and (typSym.UnAliasedType.ClassType=TBaseByteBufferSymbol);
end;

// InitData
//
procedure TBaseByteBufferSymbol.InitData(const data : TData; offset : Integer);
var
   p : PVariant;
begin
   p := @data[offset];
   VarCopySafe(p^, IdwsByteBuffer(TdwsByteBuffer.Create));
end;

// ------------------
// ------------------ TByteBufferUnaryOpExpr ------------------
// ------------------

// Create
//
constructor TByteBufferUnaryOpExpr.Create(context : TdwsCompilerContext; expr : TTypedExpr);
begin
   inherited Create(context, expr);
   Typ := context.SystemTable.FindTypeSymbol(SYS_BYTEBUFFER, cvMagic);
end;

// EvalAsVariant
//
procedure TByteBufferUnaryOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   intf : IUnknown;
begin
   EvalAsInterface(exec, intf);
   result := intf;
end;

// ------------------
// ------------------ TConvStringToByteBufferExpr ------------------
// ------------------

// EvalAsInterface
//
procedure TConvStringToByteBufferExpr.EvalAsInterface(exec : TdwsExecution; var result : IUnknown);
var
   ByteBuffer : TdwsByteBuffer;
   s : UnicodeString;
begin
   Expr.EvalAsString(exec, s);
   ByteBuffer := TdwsByteBuffer.Create;
   ByteBuffer.AssignString(s);
   result := IdwsByteBuffer(ByteBuffer);
end;

// ------------------
// ------------------ TExprBaseListExecHelper ------------------
// ------------------

// GetBuffer
//
procedure TExprBaseListExecHelper.GetBuffer(var buffer : IdwsByteBuffer);
begin
   ExprBase[0].EvalAsInterface(Exec, IInterface(buffer));
end;

// ------------------
// ------------------ TByteBufferGetLengthFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetLengthFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   Result := buffer.GetLength;
end;

// ------------------
// ------------------ TByteBufferSetLengthFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetLengthFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.SetLength(args.AsInteger[1]);
end;

// ------------------
// ------------------ TByteBufferGetPositionFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetPositionFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   Result := buffer.GetPosition;
end;

// ------------------
// ------------------ TByteBufferSetPositionFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetPositionFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.SetPosition(args.AsInteger[1]);
end;

// ------------------
// ------------------ TByteBufferGetByteFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetByteFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetByteA(args.AsInteger[1])
   else Result := buffer.GetByteP;
end;

// ------------------
// ------------------ TByteBufferSetByteFunc ------------------
// ------------------

// DoEvalAsInteger
//
procedure TByteBufferSetByteFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetByteP(args.AsInteger[1])
   else buffer.SetByteA(args.AsInteger[1], args.AsInteger[2])
end;

// ------------------
// ------------------ TByteBufferGetWordFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetWordFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetWordA(args.AsInteger[1])
   else Result := buffer.GetWordP;
end;

// ------------------
// ------------------ TByteBufferSetWordFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetWordFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetWordP(args.AsInteger[1])
   else buffer.SetWordA(args.AsInteger[1], args.AsInteger[2])
end;

// ------------------
// ------------------ TByteBufferGetInt16Func ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetInt16Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetInt16A(args.AsInteger[1])
   else Result := buffer.GetInt16P;
end;

// ------------------
// ------------------ TByteBufferSetInt16Func ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetInt16Func.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetInt16P(args.AsInteger[1])
   else buffer.SetInt16A(args.AsInteger[1], args.AsInteger[2])
end;

// ------------------
// ------------------ TByteBufferGetDWordFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetDWordFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetDWordA(args.AsInteger[1])
   else Result := buffer.GetDWordP;
end;

// ------------------
// ------------------ TByteBufferSetDWordFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetDWordFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetDWordP(args.AsInteger[1])
   else buffer.SetDWordA(args.AsInteger[1], args.AsInteger[2])
end;

// ------------------
// ------------------ TByteBufferGetInt32Func ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetInt32Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetInt32A(args.AsInteger[1])
   else Result := buffer.GetInt32P;
end;

// ------------------
// ------------------ TByteBufferSetInt32Func ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetInt32Func.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetInt32P(args.AsInteger[1])
   else buffer.SetInt32A(args.AsInteger[1], args.AsInteger[2])
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterByteBufferType);

   RegisterInternalIntFunction(TByteBufferGetLengthFunc,  '', ['buffer', SYS_BYTEBUFFER], [], 'Length');
   RegisterInternalProcedure(TByteBufferSetLengthFunc,  '', ['buffer', SYS_BYTEBUFFER, 'nbBytes', SYS_INTEGER], 'SetLength');

   RegisterInternalIntFunction(TByteBufferGetPositionFunc,  '', ['buffer', SYS_BYTEBUFFER], [], 'Position');
   RegisterInternalProcedure(TByteBufferSetPositionFunc,  '', ['buffer', SYS_BYTEBUFFER, 'indexInBytes', SYS_INTEGER], 'SetPosition');

   RegisterInternalIntFunction(TByteBufferGetByteFunc,  '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetByte');
   RegisterInternalIntFunction(TByteBufferGetByteFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetByte');
   RegisterInternalIntFunction(TByteBufferGetWordFunc,  '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetWord');
   RegisterInternalIntFunction(TByteBufferGetWordFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetWord');
   RegisterInternalIntFunction(TByteBufferGetInt16Func,  '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetInt16');
   RegisterInternalIntFunction(TByteBufferGetInt16Func,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetInt16');
   RegisterInternalIntFunction(TByteBufferGetDWordFunc,  '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetDWord');
   RegisterInternalIntFunction(TByteBufferGetDWordFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetDWord');
   RegisterInternalIntFunction(TByteBufferGetInt32Func,  '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetInt32');
   RegisterInternalIntFunction(TByteBufferGetInt32Func,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetInt32');

   RegisterInternalProcedure(TByteBufferSetByteFunc,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetByte', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetByteFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetByte', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetWordFunc,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetWord', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetWordFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetWord', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt16Func,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetInt16', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt16Func,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetInt16', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetDWordFunc,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetDWord', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetDWordFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetDWord', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt32Func,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetInt32', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt32Func,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetInt32', [iffOverloaded]);
end.
