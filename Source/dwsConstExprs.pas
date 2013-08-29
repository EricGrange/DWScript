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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsConstExprs;

{$I dws.inc}

interface

uses
   Variants, SysUtils,
   dwsUtils, dwsDataContext, dwsStack, dwsXPlatform, dwsErrors, dwsStrings,
   dwsExprs, dwsExprList, dwsSymbols, dwsUnitSymbols;

type

   TConstIntExpr = class;

   // A constant value (like 0, 3.14159, 'Hello' or true)
   TConstExpr = class(TDataExpr)
      protected
         FData : TData;

         function GetIsConstant : Boolean; override;

      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); overload; virtual;
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer); overload;
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol); overload;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;

         function IsWritable : Boolean; override;
         function SameValueAs(otherConst : TConstExpr) : Boolean;
         function SameDataExpr(expr : TTypedExpr) : Boolean; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
         property Data : TData read FData;

         class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer = 0) : TConstExpr; overload; static;
         class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr; overload; static;

         class function CreateTypedDefault(prog : TdwsProgram; typ : TTypeSymbol) : TConstExpr;

         class function CreateTypedVariantValue(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant) : TConstExpr; overload; static;

         class function CreateIntegerValue(prog : TdwsProgram; const value : Int64) : TConstExpr; overload; static;
         class function CreateIntegerValue(prog : TdwsProgram; typ : TTypeSymbol; const value : Int64) : TConstExpr; overload; static;

         class function CreateFloatValue(prog : TdwsProgram; const value : Int64) : TConstExpr; overload; static;
         class function CreateFloatValue(prog : TdwsProgram; const value : Double) : TConstExpr; overload; static;

         class function CreateStringValue(prog : TdwsProgram; const value : String) : TConstExpr; overload; static;

         class function CreateBooleanValue(prog : TdwsProgram; const value : Boolean) : TConstExpr; overload; static;

         class function CreateDynamicArrayValue(prog : TdwsProgram; typ : TTypeSymbol) : TConstExpr; overload; static;
   end;

   TUnifiedConstExprClass = class of TUnifiedConstExpr;

   // TUnifiedConstExpr
   //
   {: Unified constants go into a program root unified const list. }
   TUnifiedConstExpr = class (TConstExpr)
      public
         class function CreateUnified(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant) : TUnifiedConstExpr;
   end;

   // TConstBooleanExpr
   //
   TConstBooleanExpr = class(TUnifiedConstExpr)
      protected
         FValue : Boolean;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;

         property Value : Boolean read FValue;
   end;

   // TConstIntExpr
   //
   TConstIntExpr = class sealed (TUnifiedConstExpr)
      private
         FValue : Int64;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         property Value : Int64 read FValue write FValue;
   end;

   // TConstFloatExpr
   //
   TConstFloatExpr = class sealed (TUnifiedConstExpr)
      private
         FValue : Double;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         property Value : Double read FValue;
   end;

   // TConstStringExpr
   //
   TConstStringExpr = class(TUnifiedConstExpr)
      private
         FValue : UnicodeString;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString); override;
         property Value : UnicodeString read FValue write FValue;
   end;

   TStandardIntegersConstIntExprArray = array [-1..2] of TUnifiedConstExpr;

   // TUnifiedConstList
   //
   TUnifiedConstList = class (TSortedExprBaseList)
      private
         FEmptyString : TUnifiedConstExpr;
         FIntegers : TStandardIntegersConstIntExprArray;
         FZeroFloat : TUnifiedConstExpr;
         FTrue, FFalse : TUnifiedConstExpr;
         FNil : TUnifiedConstExpr;

      protected
         function Compare(const item1, item2 : TExprBase) : Integer; override;

      public
         destructor Destroy; override;

         procedure Precharge(prog : TdwsMainProgram; systemTable : TSystemSymbolTable);

         property EmptyString : TUnifiedConstExpr read FEmptyString;
         property Integers : TStandardIntegersConstIntExprArray read FIntegers;
         property ZeroFloat : TUnifiedConstExpr read FZeroFloat;
         property TrueConst : TUnifiedConstExpr read FTrue;
         property FalseConst : TUnifiedConstExpr read FFalse;
         property NilConst : TUnifiedConstExpr read FNil;
   end;

   TArrayConstantExpr = class sealed (TPosDataExpr)
      protected
         FArrayAddr : Integer;
         FElementExprs : TTightList;
         FArrayEvaled : Boolean;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetElement(idx : Integer) : TTypedExpr; inline;
         function GetElementCount : Integer; inline;

         function GetIsConstant : Boolean; override;

      public
         constructor Create(Prog: TdwsProgram; const aScriptPos: TScriptPos);
         destructor Destroy; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property Elements[idx : Integer] : TTypedExpr read GetElement;
         property ElementCount : Integer read GetElementCount;
         procedure AddElementExpr(Prog: TdwsProgram; ElementExpr: TTypedExpr);
         procedure Prepare(Prog: TdwsProgram; ElementTyp : TTypeSymbol);
         procedure TypeCheckElements(prog : TdwsProgram);
         procedure ElementsFromIntegerToFloat(prog : TdwsProgram);

         function Size : Integer; inline;

         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsTData(exec : TdwsExecution) : TData; overload; inline;
         procedure EvalAsTData(exec : TdwsExecution; var result : TData); overload;
         procedure EvalToTData(exec : TdwsExecution; var result : TData; offset : Integer);
         function EvalAsVarRecArray(exec : TdwsExecution) : TVarRecArrayContainer;

         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
         function IsWritable : Boolean; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsConvExprs;

// ------------------
// ------------------ TConstExpr ------------------
// ------------------

// Create
//
constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   inherited Create(Prog, Typ);
   SetLength(FData, Typ.Size);
   case Typ.Size of
      0 : ;
      1 : FData[0] := Value;
   else
      Assert(False);
   end;
end;

// Create
//
constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer);
begin
   Create(Prog, Typ);
   DWSCopyData(Data, addr, FData, 0, Typ.Size);
end;

// Create
//
constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol);
begin
   inherited Create(Prog, Typ);
   SetLength(FData, Typ.Size);
end;

// Eval
//
function TConstExpr.Eval(exec : TdwsExecution) : Variant;
begin
   EvalAsVariant(exec, Result);
end;

// EvalAsString
//
procedure TConstExpr.EvalAsString(exec : TdwsExecution; var Result : UnicodeString);
begin
   VariantToString(FData[0], Result);
end;

// EvalAsVariant
//
procedure TConstExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Result := FData[0];
end;

// GetIsConstant
//
function TConstExpr.GetIsConstant : Boolean;
begin
   Result:=True;
end;

// IsWritable
//
function TConstExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// SameValueAs
//
function TConstExpr.SameValueAs(otherConst : TConstExpr) : Boolean;
begin
   Result:=   (Length(FData)=Length(otherConst.FData))
           and DWSSameData(FData, otherConst.FData, 0, 0, Length(FData));
end;

// SameDataExpr
//
function TConstExpr.SameDataExpr(expr : TTypedExpr) : Boolean;
begin
   Result:=(ClassType=expr.ClassType) and SameValueAs(TConstExpr(expr));
end;

// GetDataPtr
//
procedure TConstExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   exec.DataContext_Create(FData, 0, Result);
end;

// CreateTypedVariantValue
//
class function TConstExpr.CreateTypedVariantValue(
   prog : TdwsProgram; typ : TTypeSymbol; const value : Variant) : TConstExpr;
begin
   if typ=prog.TypString then
      Result:=TConstStringExpr.CreateUnified(prog, typ, value)
   else if typ.ClassType=TDynamicArraySymbol then
      Result:=CreateDynamicArrayValue(prog, typ)
   else if (typ=prog.TypInteger) or (typ.typ=prog.TypInteger) then
      Result:=CreateIntegerValue(prog, typ, value)
   else if typ=prog.TypBoolean then
      Result:=CreateBooleanValue(prog, value)
   else if typ=prog.TypFloat then
      Result:=TConstFloatExpr.CreateUnified(prog, typ, value)
   else Result:=TConstExpr.Create(prog, typ, value);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer = 0) : TConstExpr;
begin
   case Length(Data) of
      0 : Result:=TConstExpr.Create(Prog, Typ, Null);
      1 : Result:=TConstExpr.CreateTypedVariantValue(Prog, Typ, Data[addr]);
   else
      Result:=TConstExpr.Create(Prog, Typ, Data, addr);
   end;
end;

// CreateTypedDefault
//
class function TConstExpr.CreateTypedDefault(prog : TdwsProgram; typ : TTypeSymbol) : TConstExpr;
var
   data : TData;
begin
   if typ=nil then
      Result:=TConstExpr.CreateTypedVariantValue(prog, prog.TypNil, IUnknown(nil))
   else begin
      SetLength(data, typ.Size);
      typ.InitData(data, 0);
      Result:=TConstExpr.CreateTyped(prog, typ, data);
   end;
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr;
begin
   Assert(constSymbol<>nil);
   Result:=CreateTyped(Prog, Typ, constSymbol.Data);
end;

// CreateIntegerValue
//
class function TConstExpr.CreateIntegerValue(prog : TdwsProgram; const value : Int64) : TConstExpr;
begin
   Result:=TConstIntExpr.CreateUnified(prog, prog.TypInteger, value);
end;

// CreateIntegerValue
//
class function TConstExpr.CreateIntegerValue(prog : TdwsProgram; typ : TTypeSymbol; const value : Int64) : TConstExpr;
begin
   Result:=TConstIntExpr.CreateUnified(prog, typ, value);
end;

// CreateFloatValue
//
class function TConstExpr.CreateFloatValue(prog : TdwsProgram; const value : Int64) : TConstExpr;
begin
   Result:=TConstFloatExpr.CreateUnified(prog, prog.TypFloat, value);
end;

// CreateFloatValue
//
class function TConstExpr.CreateFloatValue(prog : TdwsProgram; const value : Double) : TConstExpr;
begin
   Result:=TConstFloatExpr.CreateUnified(prog, prog.TypFloat, value);
end;

// CreateStringValue
//
class function TConstExpr.CreateStringValue(prog : TdwsProgram; const value : String) : TConstExpr;
begin
   Result:=TConstStringExpr.CreateUnified(prog, prog.TypString, value);
end;

// CreateBooleanValue
//
class function TConstExpr.CreateBooleanValue(prog : TdwsProgram; const value : Boolean) : TConstExpr;
begin
   Result:=TConstBooleanExpr.CreateUnified(prog, prog.TypBoolean, value);
end;

// CreateDynamicArrayValue
//
class function TConstExpr.CreateDynamicArrayValue(prog : TdwsProgram; typ : TTypeSymbol) : TConstExpr;
begin
   Result:=TConstExpr.Create(prog, typ, TScriptDynamicArray.CreateNew(typ.Typ) as IScriptObj);
end;

// ------------------
// ------------------ TUnifiedConstExpr ------------------
// ------------------

// CreateUnified
//
class function TUnifiedConstExpr.CreateUnified(Prog: TdwsProgram; Typ: TTypeSymbol;
                                               const Value: Variant) : TUnifiedConstExpr;
var
   i : Integer;
   added : Boolean;
begin
   Result:=Self.Create(Prog, Typ, Value);

   i:=Prog.Root.UnifiedConstList.AddOrFind(Result, added);
   if not added then begin
      Result.Free;
      Result:=TUnifiedConstExpr(Prog.Root.UnifiedConstList[i]);
   end;
   Result.IncRefCount;
end;

// ------------------
// ------------------ TConstBooleanExpr ------------------
// ------------------

// Create
//
constructor TConstBooleanExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypBoolean;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsInteger
//
function TConstBooleanExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Integer(FValue);
end;

// EvalAsBoolean
//
function TConstBooleanExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=FValue;
end;

// ------------------
// ------------------ TConstIntExpr ------------------
// ------------------

// Create
//
constructor TConstIntExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypInteger;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsInteger
//
function TConstIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
{$if Defined(WIN32_ASM)}
asm
   mov   edx, [eax + OFFSET FValue + 4]
   mov   eax, [eax + OFFSET FValue]
{$else}
begin
   Result:=FValue;
{$ifend}
end;

// EvalAsFloat
//
function TConstIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$if Defined(WIN32_ASM)}
asm
   fild  qword [eax + OFFSET FValue]
{$else}
begin
   Result:=FValue;
{$ifend}
end;

// ------------------
// ------------------ TConstFloatExpr ------------------
// ------------------

// Create
//
constructor TConstFloatExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypFloat;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsFloat
//
function TConstFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$if Defined(WIN32_ASM)}
asm
   fld qword [eax].FValue
{$else}
begin
   Result:=FValue;
{$ifend}
end;

// ------------------
// ------------------ TConstStringExpr ------------------
// ------------------

// Create
//
constructor TConstStringExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypString;
   UnifyAssignString(Value, FValue);
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsString
//
procedure TConstStringExpr.EvalAsString(exec : TdwsExecution; var Result : UnicodeString);
{$ifdef PUREPASCAL}
begin
   Result:=FValue;
{$else}
asm
   mov   edx, [eax + OFFSET FVALUE]
   mov   eax, ecx
   call  System.@UStrAsg;
{$endif}
end;

// ------------------
// ------------------ TUnifiedConstList<TExprBase> ------------------
// ------------------

// Destroy
//
destructor TUnifiedConstList.Destroy;
var
   i : Integer;
begin
   Clean;
   FEmptyString.Free;
   for i:=Low(FIntegers) to High(FIntegers) do begin
      Assert(FIntegers[i].RefCount=0);
      FIntegers[i].Free;
   end;
   FZeroFloat.Free;
   FTrue.Free;
   FFalse.Free;
   FNil.Free;
   inherited;
end;

// Precharge
//
procedure TUnifiedConstList.Precharge(prog : TdwsMainProgram; systemTable : TSystemSymbolTable);
const
   cEmptyString : UnicodeString = '';
   cZeroFloat : Double = 0;
   cNilIntf : IUnknown = nil;
var
   i : Integer;
begin
   inherited Create;
   FEmptyString:=TConstStringExpr.CreateUnified(prog, systemTable.TypString, cEmptyString);
   for i:=Low(FIntegers) to High(FIntegers) do
      FIntegers[i]:=TConstIntExpr.CreateUnified(prog, systemTable.TypInteger, Int64(i));
   FZeroFloat:=TConstFloatExpr.CreateUnified(prog, systemTable.TypFloat, cZeroFloat);
   FTrue:=TConstBooleanExpr.CreateUnified(prog, systemTable.TypBoolean, True);
   FFalse:=TConstBooleanExpr.CreateUnified(prog, systemTable.TypBoolean, False);
   FNil:=TUnifiedConstExpr.CreateUnified(prog, prog.TypNil, cNilIntf);
end;

// Compare
//
function TUnifiedConstList.Compare(const item1, item2 : TExprBase) : Integer;
var
   unified1, unified2 : TUnifiedConstExpr;
   vd1, vd2 : PVarData;
   rawResult : Int64;
begin
   unified1:=TUnifiedConstExpr(item1);
   unified2:=TUnifiedConstExpr(item2);
   if unified1.ClassType=unified2.ClassType then begin
      if unified1.Typ=unified2.Typ then begin
         vd1:=@unified1.FData[0];
         vd2:=@unified2.FData[0];
         rawResult:=Integer(vd1.VType)-Integer(vd2.VType);
         if rawResult=0 then begin
            case vd1.VType of
               varUString : rawResult:=UnicodeCompareStr(UnicodeString(vd1.VString), UnicodeString(vd2.VString));
               varInt64 : rawResult:=vd1.VInt64-vd2.VInt64;
               varBoolean : rawResult:=Integer(vd1.VBoolean)-Integer(vd2.VBoolean);
            else
               case VarCompareValue(unified1.FData[0], unified2.FData[0]) of
                  vrEqual : rawResult:=0;
                  vrLessThan : rawResult:=-1;
                  vrGreaterThan : rawResult:=1;
               else
                  rawResult:=0;
                  Assert(False);
               end;
            end;
         end;
      end else rawResult:=NativeInt(unified1.Typ)-NativeInt(unified2.Typ);
   end else rawResult:=NativeInt(unified1.ClassType)-NativeInt(unified2.ClassType);

   if rawResult=0 then
      Result:=0
   else if rawResult>0 then
      Result:=1
   else Result:=-1;
end;

// ------------------
// ------------------ TArrayConstantExpr ------------------
// ------------------

// Create
//
constructor TArrayConstantExpr.Create(Prog: TdwsProgram; const aScriptPos: TScriptPos);
begin
   inherited Create(prog, aScriptPos,
      TStaticArraySymbol.Create('', prog.TypNil, prog.TypInteger, 0, -1));
end;

// Destroy
//
destructor TArrayConstantExpr.Destroy;
begin
   FElementExprs.Clean;
   FTyp.Free;
   inherited;
end;

// AddElementExpr
//
procedure TArrayConstantExpr.AddElementExpr(Prog: TdwsProgram; ElementExpr: TTypedExpr);
var
   arraySymbol : TStaticArraySymbol;
begin
   arraySymbol:=(FTyp as TStaticArraySymbol);
   if arraySymbol.Typ<>Prog.TypVariant then begin
      if arraySymbol.Typ=Prog.TypNil then
         arraySymbol.Typ:=ElementExpr.Typ
      else if arraySymbol.Typ<>ElementExpr.Typ then begin
         if arraySymbol.Typ=Prog.TypNil then
            arraySymbol.Typ:=ElementExpr.Typ
         else if (arraySymbol.Typ=Prog.TypInteger) and (ElementExpr.Typ=Prog.TypFloat) then
            arraySymbol.Typ:=Prog.TypFloat
         else if not ((arraySymbol.Typ=Prog.TypFloat) and (ElementExpr.Typ=Prog.TypInteger)) then
            arraySymbol.Typ:=Prog.TypVariant;
      end;
   end;
   FElementExprs.Add(ElementExpr);
   arraySymbol.AddElement;
end;

// Prepare
//
procedure TArrayConstantExpr.Prepare(prog: TdwsProgram; elementTyp : TTypeSymbol);
var
   x, n : Integer;
   elemExpr : TTypedExpr;
begin
   if (elementTyp<>nil) and (FTyp.Typ<>elementTyp) then begin
      if  (elementTyp.IsOfType(prog.TypFloat) and FTyp.Typ.IsOfType(prog.TypInteger)) then begin
         ElementsFromIntegerToFloat(prog);
      end else if elementTyp.IsCompatible(FTyp.Typ) then begin
         (FTyp as TStaticArraySymbol).Typ:=elementTyp;
      end;
   end;

   for x := 0 to FElementExprs.Count - 1 do begin
      elemExpr:=TTypedExpr(FElementExprs.List[x]);
      if elemExpr is TArrayConstantExpr then
         TArrayConstantExpr(elemExpr).Prepare(prog, FTyp.Typ);
   end;

   if FTyp.Typ<>nil then
      n:=FElementExprs.Count * FTyp.Typ.Size
   else n:=0;
   FArrayAddr := prog.GetGlobalAddr(n);
end;

// GetDataPtr
//
procedure TArrayConstantExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   Eval(exec);
   exec.DataContext_Create(exec.Stack.Data, FArrayAddr, Result);
end;

// GetSubExpr
//
function TArrayConstantExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=TExprBase(FElementExprs.List[i]);
end;

// GetSubExprCount
//
function TArrayConstantExpr.GetSubExprCount : Integer;
begin
   Result:=FElementExprs.Count;
end;

// GetElement
//
function TArrayConstantExpr.GetElement(idx : Integer) : TTypedExpr;
begin
   Result:=TTypedExpr(FElementExprs.List[idx]);
end;

// GetElementCount
//
function TArrayConstantExpr.GetElementCount : Integer;
begin
   Result:=FElementExprs.Count;
end;

// Size
//
function TArrayConstantExpr.Size : Integer;
begin
   Result:=ElementCount*Typ.Typ.Size;
end;

// Eval
//
function TArrayConstantExpr.Eval(exec : TdwsExecution) : Variant;

   procedure DoEval;
   var
      x, addr : Integer;
      elemSize : Integer;
      elemExpr : TTypedExpr;
      dataExpr : TDataExpr;
   begin
      exec.Stack.WriteValue(FArrayAddr, FElementExprs.Count);

      elemSize:=Typ.Typ.Size;
      addr:=FArrayAddr;
      for x:=0 to FElementExprs.Count-1 do begin
         elemExpr:=TTypedExpr(FElementExprs.List[x]);
         if elemSize=1 then begin
            exec.Stack.WriteValue(addr, elemExpr.Eval(exec));
         end else begin
            dataExpr:=elemExpr as TDataExpr;
            dataExpr.DataPtr[exec].CopyData(exec.Stack.Data, addr, elemSize);
         end;
         Inc(addr, elemSize);
      end;
   end;

begin
   if not FArrayEvaled then begin
      FArrayEvaled:=True;
      DoEval;
   end;

   Result:=FArrayAddr;
end;

// EvalAsTData
//
function TArrayConstantExpr.EvalAsTData(exec : TdwsExecution) : TData;
begin
   EvalAsTData(exec, Result);
end;

// EvalAsTData
//
procedure TArrayConstantExpr.EvalAsTData(exec : TdwsExecution; var result : TData);
begin
   SetLength(result, Size);
   Typ.InitData(result, 0);
   EvalToTData(exec, result, 0);
end;

// EvalToTData
//
procedure TArrayConstantExpr.EvalToTData(exec : TdwsExecution; var result : TData; offset : Integer);
var
   i, p : Integer;
   expr : TTypedExpr;
begin
   p:=offset;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      if expr is TArrayConstantExpr then
         TArrayConstantExpr(expr).EvalToTData(exec, result, p)
      else expr.EvalAsVariant(exec, result[p]);
      Inc(p, expr.Typ.Size);
   end;
end;

// EvalAsVarRecArray
//
function TArrayConstantExpr.EvalAsVarRecArray(exec : TdwsExecution) : TVarRecArrayContainer;
var
   i : Integer;
   expr : TTypedExpr;
   buf : Variant;
begin
   Result:=TVarRecArrayContainer.Create;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      expr.EvalAsVariant(exec, buf);
      Result.Add(buf);
   end;
   Result.Initialize;
end;

// Optimize
//
function TArrayConstantExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   i : Integer;
   expr : TTypedExpr;
begin
   Result:=Self;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      FElementExprs.List[i]:=expr.Optimize(prog, exec);
   end;
end;

// GetIsConstant
//
function TArrayConstantExpr.GetIsConstant : Boolean;
var
   i : Integer;
begin
   for i:=0 to FElementExprs.Count-1 do
      if not TTypedExpr(FElementExprs.List[i]).IsConstant then
         Exit(False);
   Result:=True;
end;

// IsWritable
//
function TArrayConstantExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// TypeCheckElements
//
procedure TArrayConstantExpr.TypeCheckElements(prog : TdwsProgram);
var
   x : Integer;
   expr : TTypedExpr;
   elemTyp : TTypeSymbol;
begin
   if Typ.Typ=nil then
      prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_InvalidConstType, [SYS_VOID])
   else if FElementExprs.Count>0 then begin
      elemTyp:=Elements[0].Typ;
      for x:=1 to FElementExprs.Count-1 do begin
         expr:=Elements[x];
         if not elemTyp.IsCompatible(expr.Typ) then begin
            if elemTyp.IsOfType(prog.TypInteger) and expr.Typ.IsOfType(prog.TypFloat) then
               elemTyp:=prog.TypFloat
            else if elemTyp.IsOfType(prog.TypFloat) and expr.Typ.IsOfType(prog.TypInteger) then
               // handled below
            else if expr.Typ.IsCompatible(elemTyp) then
               elemTyp:=expr.Typ
            else if (expr.Typ is TStructuredTypeSymbol) and (elemTyp is TStructuredTypeSymbol) then begin
               repeat
                  elemTyp:=TStructuredTypeSymbol(elemTyp).Parent;
                  if elemTyp=nil then begin
                     prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_AssignIncompatibleTypes,
                                                          [expr.Typ.Caption, Elements[0].Typ.Caption]);
                     Exit;
                  end;
               until elemTyp.IsCompatible(expr.Typ);
            end else if prog.TypVariant.IsCompatible(expr.Typ) and prog.TypVariant.IsCompatible(elemTyp) then
               elemTyp:=prog.TypVariant
            else begin
               prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_AssignIncompatibleTypes,
                                                    [expr.Typ.Caption, elemTyp.Caption]);
               Exit;
            end;
         end;
      end;

      // implicit cast integer to float
      if elemTyp.IsOfType(prog.TypFloat) then
         ElementsFromIntegerToFloat(prog);

      Typ.Typ:=elemTyp;
   end;
end;

// ElementsFromIntegerToFloat
//
procedure TArrayConstantExpr.ElementsFromIntegerToFloat(prog : TdwsProgram);
var
   x : Integer;
   expr : TTypedExpr;
begin
   for x:=0 to FElementExprs.Count-1 do begin
      expr:=Elements[x];
      if expr.Typ.IsOfType(prog.TypInteger) then begin
         expr:=TConvIntToFloatExpr.Create(prog, expr);
         FElementExprs.List[x]:=expr;
      end;
   end;
   Typ.Typ:=prog.TypFloat;
end;

end.
