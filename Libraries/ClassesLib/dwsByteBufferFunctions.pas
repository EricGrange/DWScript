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

{$define BYTEBUFFER_FILE_FUNCTIONS}

interface

uses
   Types,
   dwsXPlatform, dwsUtils, dwsStrings, dwsCompilerContext, dwsDataContext,
   dwsSymbols, dwsFunctions, dwsUnitSymbols, dwsOperators, dwsExprs,
   dwsMagicExprs, dwsExprList, dwsTokenTypes, dwsScriptSource,
   dwsByteBuffer, dwsDynamicArrays;

const
   SYS_BYTEBUFFER = 'ByteBuffer';

type

   TBaseByteBufferSymbol = class (TTypeSymbol)
      public
         constructor Create(const aName : String);
         function DynamicInitialization : Boolean; override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;
         procedure InitVariant(var v : Variant); override;
   end;

   TByteBufferUnaryOpExpr = class (TUnaryOpExpr)
      public
         constructor Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr); override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TConvStringToByteBufferExpr = class(TByteBufferUnaryOpExpr)
      public
         procedure EvalAsInterface(exec : TdwsExecution; var result : IUnknown); override;
   end;

   TByteBufferNewOpExpr = class (TByteBufferUnaryOpExpr)
      public
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

   TByteBufferCopyFunc = class(TInternalMagicInterfaceFunction)
      procedure DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown); override;
   end;

   TByteBufferAssignFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferAssignDataFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferAssignJSONFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferAssignBase64Func = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferAssignHexStringFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferToDataStringFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   TByteBufferToJSONFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   TByteBufferToBase64Func = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   TByteBufferToHexStringFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
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

   TByteBufferGetInt8Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetInt8Func = class(TInternalMagicProcedure)
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

   TByteBufferGetInt64Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TByteBufferSetInt64Func = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetSingleFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TByteBufferSetSingleFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetDoubleFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TByteBufferSetDoubleFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetExtendedFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TByteBufferSetExtendedFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetDataFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   TByteBufferSetDataFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TByteBufferGetIntegersFunc = class(TInternalMagicDynArrayFunction)
      procedure DoEvalAsDynArray(const args : TExprBaseListExec; var Result : IScriptDynArray); override;
   end;

{$ifdef BYTEBUFFER_FILE_FUNCTIONS}
   TFileWriteByteBuffer1Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;
{$endif}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$ifdef BYTEBUFFER_FILE_FUNCTIONS}
uses dwsFileFunctions;
{$endif}

// RegisterByteBufferType
//
procedure RegisterByteBufferType(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                 unitTable : TSymbolTable);
var
   typByteBuffer : TBaseByteBufferSymbol;
begin
   if systemTable.FindLocal(SYS_BYTEBUFFER) <> nil then Exit;

   typByteBuffer := TBaseByteBufferSymbol.Create(SYS_BYTEBUFFER);
   systemTable.AddSymbol(typByteBuffer);
end;

// RegisterByteBufferOperators
//
procedure RegisterByteBufferOperators(systemTable : TSystemSymbolTable; unitTable : TSymbolTable;
                                      operators : TOperators);
var
   typByteBuffer : TBaseByteBufferSymbol;
begin
   typByteBuffer := systemTable.FindTypeSymbol(SYS_BYTEBUFFER, cvMagic) as TBaseByteBufferSymbol;

  if operators.FindCaster(typByteBuffer, systemTable.TypString) <> nil then Exit;

   operators.RegisterCaster(typByteBuffer, systemTable.TypString, TConvStringToByteBufferExpr);

   operators.RegisterUnaryOperator(ttNEW, TByteBufferNewOpExpr, typByteBuffer);
end;

type
   TExprBaseListExecHelper = record helper for TExprBaseListExec
      procedure GetBuffer(var buffer : IdwsByteBuffer); inline;
   end;

// ------------------
// ------------------ TBaseByteBufferSymbol ------------------
// ------------------

// Create
//
constructor TBaseByteBufferSymbol.Create(const aName : String);
begin
   inherited Create(aName, nil);
   FSize := 1;
end;

// DynamicInitialization
//
function TBaseByteBufferSymbol.DynamicInitialization : Boolean;
begin
   Result := True;
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
begin
   InitVariant(data[offset]);
end;

// InitVariant
//
procedure TBaseByteBufferSymbol.InitVariant(var v : Variant);
begin
   VarCopySafe(v, IdwsByteBuffer(TdwsByteBuffer.Create));
end;

// ------------------
// ------------------ TByteBufferUnaryOpExpr ------------------
// ------------------

// Create
//
constructor TByteBufferUnaryOpExpr.Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr);
begin
   inherited Create(context, aScriptPos, expr);
   Typ := context.FindType(SYS_BYTEBUFFER);
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
   bb : TdwsByteBuffer;
   s : String;
begin
   Expr.EvalAsString(exec, s);
   bb := TdwsByteBuffer.Create;
   result := IdwsByteBuffer(bb);
   bb.AssignDataString(s);
end;

// ------------------
// ------------------ TByteBufferNewOpExpr ------------------
// ------------------

// EvalAsInterface
//
procedure TByteBufferNewOpExpr.EvalAsInterface(exec : TdwsExecution; var result : IUnknown);
begin
   result := IdwsByteBuffer(TdwsByteBuffer.Create);
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
   Result := buffer.GetCount;
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
   buffer.SetCount(args.AsInteger[1]);
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
// ------------------ TByteBufferCopyFunc ------------------
// ------------------

// DoEvalAsInterface
//
procedure TByteBufferCopyFunc.DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   result := buffer.Copy(args.AsInteger[1], args.AsInteger[2]);
end;

// ------------------
// ------------------ TByteBufferAssignFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferAssignFunc.DoEvalProc(const args : TExprBaseListExec);
var
   dest, src : IdwsByteBuffer;
begin
   args.GetBuffer(dest);
   args.ExprBase[1].EvalAsInterface(args.Exec, IInterface(src));
   dest.Assign(src);
end;

// ------------------
// ------------------ TByteBufferAssignDataFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferAssignDataFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.AssignDataString(args.AsString[1]);
end;

// ------------------
// ------------------ TByteBufferAssignJSONFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferAssignJSONFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.AssignJSON(args.AsString[1]);
end;

// ------------------
// ------------------ TByteBufferAssignBase64Func ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferAssignBase64Func.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.AssignBase64(args.AsString[1]);
end;

// ------------------
// ------------------ TByteBufferAssignHexStringFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferAssignHexStringFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.AssignHexString(args.AsString[1]);
end;

// ------------------
// ------------------ TByteBufferToDataStringFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TByteBufferToDataStringFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.ToDataString(Result);
end;

// ------------------
// ------------------ TByteBufferToJSONFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TByteBufferToJSONFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.ToJSON(Result);
end;

// ------------------
// ------------------ TByteBufferToBase64Func ------------------
// ------------------

// DoEvalAsString
//
procedure TByteBufferToBase64Func.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.ToBase64(Result);
end;

// ------------------
// ------------------ TByteBufferToHexStringFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TByteBufferToHexStringFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   buffer.ToHexString(Result);
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
      buffer.SetByteP(Byte(args.AsInteger[1]))
   else buffer.SetByteA(args.AsInteger[1], Byte(args.AsInteger[2]))
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
      buffer.SetWordP(Word(args.AsInteger[1]))
   else buffer.SetWordA(args.AsInteger[1], Word(args.AsInteger[2]))
end;

// ------------------
// ------------------ TByteBufferGetInt8Func ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetInt8Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetInt8A(args.AsInteger[1])
   else Result := buffer.GetInt8P;
end;

// ------------------
// ------------------ TByteBufferSetInt8Func ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetInt8Func.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetInt8P(args.AsInteger[1])
   else buffer.SetInt8A(args.AsInteger[1], args.AsInteger[2])
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
      buffer.SetDWordP(DWord(args.AsInteger[1]))
   else buffer.SetDWordA(args.AsInteger[1], DWord(args.AsInteger[2]))
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

// ------------------
// ------------------ TByteBufferGetInt64Func ------------------
// ------------------

// DoEvalAsInteger
//
function TByteBufferGetInt64Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetInt64A(args.AsInteger[1])
   else Result := buffer.GetInt64P;
end;

// ------------------
// ------------------ TByteBufferSetInt64Func ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetInt64Func.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetInt64P(args.AsInteger[1])
   else buffer.SetInt64A(args.AsInteger[1], args.AsInteger[2])
end;

// ------------------
// ------------------ TByteBufferGetSingleFunc ------------------
// ------------------

// DoEvalAsFloat
//
procedure TByteBufferGetSingleFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetSingleA(args.AsInteger[1])
   else Result := buffer.GetSingleP;
end;

// ------------------
// ------------------ TByteBufferSetSingleFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetSingleFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetSingleP(args.AsFloat[1])
   else buffer.SetSingleA(args.AsInteger[1], args.AsFloat[2])
end;

// ------------------
// ------------------ TByteBufferGetDoubleFunc ------------------
// ------------------

// DoEvalAsFloat
//
procedure TByteBufferGetDoubleFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetDoubleA(args.AsInteger[1])
   else Result := buffer.GetDoubleP;
end;

// ------------------
// ------------------ TByteBufferSetDoubleFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetDoubleFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetDoubleP(args.AsFloat[1])
   else buffer.SetDoubleA(args.AsInteger[1], args.AsFloat[2])
end;

// ------------------
// ------------------ TByteBufferGetExtendedFunc ------------------
// ------------------

// DoEvalAsFloat
//
procedure TByteBufferGetExtendedFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      Result := buffer.GetExtendedA(args.AsInteger[1])
   else Result := buffer.GetExtendedP;
end;

// ------------------
// ------------------ TByteBufferSetExtendedFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetExtendedFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetExtendedP(args.AsFloat[1])
   else buffer.SetExtendedA(args.AsInteger[1], args.AsFloat[2])
end;

// ------------------
// ------------------ TByteBufferGetDataFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TByteBufferGetDataFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 3 then
      buffer.GetDataStringA(args.AsInteger[1], args.AsInteger[2], Result)
   else buffer.GetDataStringP(args.AsInteger[1], Result);
end;

// ------------------
// ------------------ TByteBufferSetDataFunc ------------------
// ------------------

// DoEvalProc
//
procedure TByteBufferSetDataFunc.DoEvalProc(const args : TExprBaseListExec);
var
   buffer : IdwsByteBuffer;
begin
   args.GetBuffer(buffer);
   if args.Count = 2 then
      buffer.SetDataStringP(args.AsString[1])
   else buffer.SetDataStringA(args.AsInteger[1], args.AsString[2])
end;

// ------------------
// ------------------ TByteBufferGetIntegersFunc ------------------
// ------------------

// DoEvalAsDynArray
//
procedure TByteBufferGetIntegersFunc.DoEvalAsDynArray(const args : TExprBaseListExec; var Result : IScriptDynArray);
var
   dyn : TScriptDynamicNativeIntegerArray;
   buffer : IdwsByteBuffer;
   index, count, elemSize : Integer;
   i : NativeInt;
   signed : Boolean;
   pSrcInt64 : PInt64Array;
   pSrcInt32 : PInt32Array;
   pSrcUInt32 : PUInt32Array;
   pSrcInt16 : PInt16Array;
   pSrcUInt16 : PUInt16Array;
   pSrcInt8 : PInt8Array;
   pSrcUInt8 : PUInt8Array;
begin
   dyn := TScriptDynamicNativeIntegerArray.Create(TTypedExpr(args.Expr).Typ.Typ);
   Result := dyn;
   args.GetBuffer(buffer);
   index := args.AsInteger[1];
   count := args.AsInteger[2];
   elemSize := args.AsInteger[3];
   signed := args.AsBoolean[4];
   buffer.RangeCheck(index, count*elemSize);
   dyn.SetArrayLength(count);
   case elemSize of
      1 :  if signed then begin
         pSrcInt8 := buffer.DataPtr;
         for i := 0 to count-1 do
            dyn.SetAsInteger(i, pSrcInt8[i]);
      end else begin
         pSrcUInt8 := buffer.DataPtr;
         for i := 0 to count-1 do
            dyn.SetAsInteger(i, pSrcUInt8[i]);
      end;
      2 :  if signed then begin
         pSrcInt16 := buffer.DataPtr;
         for i := 0 to count-1 do
            dyn.SetAsInteger(i, pSrcInt16[i]);
      end else begin
         pSrcUInt16 := buffer.DataPtr;
         for i := 0 to count-1 do
            dyn.SetAsInteger(i, pSrcUInt16[i]);
      end;
      4 : if signed then begin
         pSrcInt32 := buffer.DataPtr;
         for i := 0 to count-1 do
            dyn.SetAsInteger(i, pSrcInt32[i]);
      end else begin
         pSrcUInt32 := buffer.DataPtr;
         for i := 0 to count-1 do
            dyn.SetAsInteger(i, pSrcUInt32[i]);
      end;
      8 : begin
         if not signed then
            raise EdwsByteBuffer.Create('UInt64 is not supported');
         pSrcInt64 := buffer.DataPtr;
         for i := 0 to count-1 do
            dyn.SetAsInteger(i, pSrcInt64[i]);
      end;
   else
      raise EdwsByteBuffer.CreateFmt('Unsupported element size (%d)', [ elemSize ]);
   end;
end;

{$ifdef BYTEBUFFER_FILE_FUNCTIONS}

// ------------------
// ------------------ TFileWriteByteBuffer1Func ------------------
// ------------------

// DoEvalAsInteger
//
function TFileWriteByteBuffer1Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buf : RawByteString;
begin
   buf := args.AsDataString[1];
   Result := dwsXPlatform.FileWrite(GetIdwsFileHandle(args, 0), Pointer(buf), Length(buf));
end;

{$endif}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterByteBufferType);
   dwsInternalUnit.AddOperatorsRegistrationProc(RegisterByteBufferOperators);

   RegisterInternalIntFunction(TByteBufferGetLengthFunc,  '', ['buffer', SYS_BYTEBUFFER], [], 'Length');
   RegisterInternalProcedure(TByteBufferSetLengthFunc,  '', ['buffer', SYS_BYTEBUFFER, 'nbBytes', SYS_INTEGER], 'SetLength');

   RegisterInternalIntFunction(TByteBufferGetPositionFunc,  '', ['buffer', SYS_BYTEBUFFER], [], 'Position');
   RegisterInternalProcedure(TByteBufferSetPositionFunc,  '', ['buffer', SYS_BYTEBUFFER, 'indexInBytes', SYS_INTEGER], 'SetPosition');

   RegisterInternalFunction(TByteBufferCopyFunc, '', ['buffer', SYS_BYTEBUFFER, 'offset=0', SYS_INTEGER, 'size=MaxInt', SYS_INTEGER],
                                                 SYS_BYTEBUFFER, [], 'Copy');

   RegisterInternalProcedure(TByteBufferAssignFunc, '', ['buffer', SYS_BYTEBUFFER, 'src', SYS_BYTEBUFFER], 'Assign');
   RegisterInternalProcedure(TByteBufferAssignDataFunc, '', ['buffer', SYS_BYTEBUFFER, 'data', SYS_STRING], 'AssignDataString');
   RegisterInternalProcedure(TByteBufferAssignJSONFunc, '', ['buffer', SYS_BYTEBUFFER, 'data', SYS_STRING], 'AssignJSON');
   RegisterInternalProcedure(TByteBufferAssignBase64Func, '', ['buffer', SYS_BYTEBUFFER, 'data', SYS_STRING], 'AssignBase64');
   RegisterInternalProcedure(TByteBufferAssignHexStringFunc, '', ['buffer', SYS_BYTEBUFFER, 'hexData', SYS_STRING], 'AssignHexString');

   RegisterInternalStringFunction(TByteBufferToDataStringFunc, '', ['buffer', SYS_BYTEBUFFER], [], 'ToDataString');
   RegisterInternalStringFunction(TByteBufferToJSONFunc, '', ['buffer', SYS_BYTEBUFFER], [], 'ToJSON');
   RegisterInternalStringFunction(TByteBufferToBase64Func, '', ['buffer', SYS_BYTEBUFFER], [], 'ToBase64');
   RegisterInternalStringFunction(TByteBufferToHexStringFunc, '', ['buffer', SYS_BYTEBUFFER], [], 'ToHexString');

   RegisterInternalIntFunction(TByteBufferGetByteFunc,     '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetByte');
   RegisterInternalIntFunction(TByteBufferGetByteFunc,     '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetByte');
   RegisterInternalIntFunction(TByteBufferGetInt8Func,     '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetInt8');
   RegisterInternalIntFunction(TByteBufferGetInt8Func,     '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetInt8');
   RegisterInternalIntFunction(TByteBufferGetWordFunc,     '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetWord');
   RegisterInternalIntFunction(TByteBufferGetWordFunc,     '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetWord');
   RegisterInternalIntFunction(TByteBufferGetInt16Func,    '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetInt16');
   RegisterInternalIntFunction(TByteBufferGetInt16Func,    '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetInt16');
   RegisterInternalIntFunction(TByteBufferGetDWordFunc,    '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetDWord');
   RegisterInternalIntFunction(TByteBufferGetDWordFunc,    '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetDWord');
   RegisterInternalIntFunction(TByteBufferGetInt32Func,    '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetInt32');
   RegisterInternalIntFunction(TByteBufferGetInt32Func,    '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetInt32');
   RegisterInternalIntFunction(TByteBufferGetInt64Func,    '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetInt64');
   RegisterInternalIntFunction(TByteBufferGetInt64Func,    '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetInt64');
   RegisterInternalFloatFunction(TByteBufferGetSingleFunc, '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetSingle');
   RegisterInternalFloatFunction(TByteBufferGetSingleFunc, '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetSingle');
   RegisterInternalFloatFunction(TByteBufferGetDoubleFunc, '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetDouble');
   RegisterInternalFloatFunction(TByteBufferGetDoubleFunc, '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetDouble');
   RegisterInternalFloatFunction(TByteBufferGetExtendedFunc, '', ['buffer', SYS_BYTEBUFFER], [iffOverloaded], 'GetExtended');
   RegisterInternalFloatFunction(TByteBufferGetExtendedFunc, '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER], [iffOverloaded], 'GetExtended');
   RegisterInternalStringFunction(TByteBufferGetDataFunc,  '', ['buffer', SYS_BYTEBUFFER, 'size', SYS_INTEGER], [iffOverloaded], 'GetData');
   RegisterInternalStringFunction(TByteBufferGetDataFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'size', SYS_INTEGER], [iffOverloaded], 'GetData');
   RegisterInternalFunction(TByteBufferGetIntegersFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'count', SYS_INTEGER, 'elemSize', SYS_INTEGER, 'signed', SYS_BOOLEAN], SYS_ARRAY_OF_INTEGER, [], 'GetIntegers');

   RegisterInternalProcedure(TByteBufferSetByteFunc,   '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetByte', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetByteFunc,   '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetByte', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetWordFunc,   '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetWord', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetWordFunc,   '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetWord', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt8Func,   '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetInt8', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt8Func,   '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetInt8', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt16Func,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetInt16', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt16Func,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetInt16', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetDWordFunc,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetDWord', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetDWordFunc,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetDWord', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt32Func,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetInt32', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt32Func,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetInt32', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt64Func,  '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_INTEGER], 'SetInt64', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetInt64Func,  '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_INTEGER], 'SetInt64', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetSingleFunc, '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_FLOAT], 'SetSingle', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetSingleFunc, '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_FLOAT], 'SetSingle', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetDoubleFunc, '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_FLOAT], 'SetDouble', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetDoubleFunc, '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_FLOAT], 'SetDouble', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetExtendedFunc, '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_FLOAT], 'SetExtended', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetExtendedFunc, '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_FLOAT], 'SetExtended', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetDataFunc,   '', ['buffer', SYS_BYTEBUFFER, 'v', SYS_STRING], 'SetData', [iffOverloaded]);
   RegisterInternalProcedure(TByteBufferSetDataFunc,   '', ['buffer', SYS_BYTEBUFFER, 'index', SYS_INTEGER, 'v', SYS_STRING], 'SetData', [iffOverloaded]);

   {$ifdef BYTEBUFFER_FILE_FUNCTIONS}
   RegisterInternalIntFunction(TFileWriteByteBuffer1Func, 'FileWrite', ['f', SYS_FILE, 'buf', SYS_BYTEBUFFER], [iffOverloaded], 'Write');
   {$endif}

end.
