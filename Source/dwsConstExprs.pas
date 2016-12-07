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
   dwsExprs, dwsExprList, dwsSymbols, dwsUnitSymbols, dwsScriptSource;

type

   TConstIntExpr = class;

   // A constant value (like 0, 3.14159, 'Hello' or true)
   TConstExpr = class(TDataExpr)
      protected
         FData : TData;

         function GetIsConstant : Boolean; override;

      public
         constructor Create(Prog: TdwsProgram; aTyp: TTypeSymbol; const Value: Variant); overload; virtual;
         constructor Create(aTyp: TTypeSymbol; const Data: TData; addr : Integer); overload;
         constructor Create(aTyp: TTypeSymbol); overload;
         constructor CreateRef(aTyp: TTypeSymbol; const Data: TData);
         constructor CreateNull(aTyp: TTypeSymbol);
         procedure Orphan(prog : TdwsProgram); override;

         procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj); override;
         procedure EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface); override;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         function IsWritable : Boolean; override;
         function SameValueAs(otherConst : TConstExpr) : Boolean;
         function SameDataExpr(expr : TTypedExpr) : Boolean; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
         property Data : TData read FData;

         class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer = 0) : TConstExpr; overload; static;
         class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr; overload; static;

         class function CreateTypedDefault(prog : TdwsProgram; typ : TTypeSymbol) : TConstExpr;

         class function CreateTypedVariantValue(prog : TdwsProgram; typ : TTypeSymbol; const value : Variant) : TConstExpr; overload; static;

         class function CreateIntegerValue(prog : TdwsProgram; const value : Int64) : TConstExpr; overload; static;
         class function CreateIntegerValue(prog : TdwsProgram; typ : TTypeSymbol; const value : Int64) : TConstExpr; overload; static;

         class function CreateFloatValue(prog : TdwsProgram; const value : Int64) : TConstExpr; overload; static;
         class function CreateFloatValue(prog : TdwsProgram; const value : Double) : TConstExpr; overload; static;

         class function CreateStringValue(prog : TdwsProgram; const value : UnicodeString) : TConstExpr; overload; static;

         class function CreateBooleanValue(prog : TdwsProgram; const value : Boolean) : TConstExpr; overload; static;

         class function CreateDynamicArrayValue(prog : TdwsProgram; typ : TTypeSymbol; const val : IScriptDynArray) : TConstExpr; static;
         class function CreateAssociativeArrayValue(prog : TdwsProgram; typ : TAssociativeArraySymbol; const val : IScriptAssociativeArray) : TConstExpr; static;
   end;

   // TConstNilExpr
   //
   TConstNilExpr = class(TConstExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj); override;
         procedure EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface); override;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;
   end;

   // TConstBooleanExpr
   //
   TConstBooleanExpr = class(TConstExpr)
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
   TConstIntExpr = class sealed (TConstExpr)
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
   TConstFloatExpr = class sealed (TConstExpr)
      private
         FValue : Double;

      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;

         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         property Value : Double read FValue;
   end;

   // TConstStringExpr
   //
   TConstStringExpr = class(TConstExpr)
      private
         FValue : UnicodeString;

         procedure SetValue(const v : UnicodeString);

      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;

         procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString); override;
         property Value : UnicodeString read FValue write SetValue;
   end;

   // TConstArrayExpr
   //
   TConstArrayExpr = class (TConstExpr)
      private
         FSymbol : TConstSymbol;

      public
         constructor Create(prog : TdwsProgram; symbol : TConstSymbol);

         property Symbol : TConstSymbol read FSymbol;
   end;

   TStandardIntegersConstIntExprArray = array [-1..2] of TConstExpr;

   // TUnifiedConstants
   //
   TUnifiedConstants = class
      private
         FEmptyString : TConstExpr;
         FIntegers : TStandardIntegersConstIntExprArray;
         FZeroFloat : TConstExpr;
         FTrue, FFalse : TConstExpr;
         FNil : TConstNilExpr;

      public
         constructor Create(prog : TdwsMainProgram; systemTable : TSystemSymbolTable);
         destructor Destroy; override;

         property EmptyString : TConstExpr read FEmptyString;
         property Integers : TStandardIntegersConstIntExprArray read FIntegers;
         property ZeroFloat : TConstExpr read FZeroFloat;
         property TrueConst : TConstExpr read FTrue;
         property FalseConst : TConstExpr read FFalse;
         property NilConst : TConstNilExpr read FNil;
   end;

   // TArrayConstantExpr
   //
   TArrayConstantExpr = class sealed (TPosDataExpr)
      protected
         FElementExprs : TTightList;
         FArrayData : IDataContext;

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
         procedure AddElementExpr(const scriptPos : TScriptPos; prog: TdwsProgram; ElementExpr: TTypedExpr);
         procedure AddElementRange(prog : TdwsProgram; const range1, range2 : Int64; typ : TTypeSymbol);
         procedure Prepare(prog : TdwsProgram; elementTyp : TTypeSymbol);
         procedure TypeCheckElements(prog : TdwsProgram);
         procedure ElementsFromIntegerToFloat(prog : TdwsProgram);

         function Size : Integer; inline;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
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
constructor TConstExpr.Create(Prog: TdwsProgram; aTyp: TTypeSymbol; const Value: Variant);
begin
   inherited Create(aTyp);
   SetLength(FData, aTyp.Size);
   case aTyp.Size of
      0 : ;
      1 : VarCopySafe(FData[0], Value);
   else
      Assert(False);
   end;
end;

// Create
//
constructor TConstExpr.Create(aTyp: TTypeSymbol; const Data: TData; addr : Integer);
begin
   Create(aTyp);
   DWSCopyData(Data, addr, FData, 0, aTyp.Size);
end;

// Create
//
constructor TConstExpr.Create(aTyp: TTypeSymbol);
begin
   inherited Create(aTyp);
   SetLength(FData, aTyp.Size);
end;

// CreateRef
//
constructor TConstExpr.CreateRef(aTyp: TTypeSymbol; const Data: TData);
begin
   inherited Create(aTyp);
   FData:=Data;
end;

// CreateNull
//
constructor TConstExpr.CreateNull(aTyp: TTypeSymbol);
var
   i : Integer;
begin
   inherited Create(aTyp);
   SetLength(FData, aTyp.Size);
   for i:=0 to aTyp.Size-1 do
      FData[i]:=Null;
end;

// Orphan
//
procedure TConstExpr.Orphan(prog : TdwsProgram);
begin
   DecRefCount;
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
   VarCopySafe(Result, FData[0]);
end;

// EvalAsScriptObj
//
procedure TConstExpr.EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj);
begin
   result := IScriptObj(IUnknown(FData[0]));
end;

// EvalAsScriptObjInterface
//
procedure TConstExpr.EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface);
begin
   result := IScriptObjInterface(IUnknown(FData[0]));
end;

// EvalAsScriptDynArray
//
procedure TConstExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
begin
   result := IScriptDynArray(IUnknown(FData[0]));
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
   if typ = prog.TypString then
      Result := TConstStringExpr.Create(prog, typ, value)
   else if typ.ClassType = TDynamicArraySymbol then
      Result:=CreateDynamicArrayValue(prog, typ, IUnknown(value) as IScriptDynArray)
   else if typ.ClassType=TAssociativeArraySymbol then
      Result:=CreateAssociativeArrayValue(prog, TAssociativeArraySymbol(typ),
                                          IUnknown(value) as IScriptAssociativeArray)
   else if (typ=prog.TypInteger) or (typ.typ=prog.TypInteger) then
      Result:=CreateIntegerValue(prog, typ, value)
   else if typ=prog.TypBoolean then
      Result:=CreateBooleanValue(prog, value)
   else if typ = prog.TypFloat then
      Result := TConstFloatExpr.Create(prog, typ, value)
   else Result := TConstExpr.Create(prog, typ, value);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer = 0) : TConstExpr;
begin
   case Length(Data) of
      0 : Result:=TConstExpr.Create(Prog, Typ, Null);
      1 : Result:=TConstExpr.CreateTypedVariantValue(Prog, Typ, Data[addr]);
   else
      Result:=TConstExpr.Create(Typ, Data, addr);
   end;
end;

// CreateTypedDefault
//
class function TConstExpr.CreateTypedDefault(prog : TdwsProgram; typ : TTypeSymbol) : TConstExpr;
var
   data : TData;
begin
   SetLength(data, typ.Size);
   typ.InitData(data, 0);
   Result:=TConstExpr.CreateTyped(prog, typ, data);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr;
begin
   Assert(constSymbol<>nil);
   if constSymbol.Typ is TArraySymbol then
      Result:=TConstArrayExpr.Create(Prog, constSymbol)
   else Result:=CreateTyped(Prog, Typ, constSymbol.Data);
end;

// CreateIntegerValue
//
class function TConstExpr.CreateIntegerValue(prog : TdwsProgram; const value : Int64) : TConstExpr;
begin
   Result := TConstIntExpr.Create(prog, prog.TypInteger, value);
end;

// CreateIntegerValue
//
class function TConstExpr.CreateIntegerValue(prog : TdwsProgram; typ : TTypeSymbol; const value : Int64) : TConstExpr;
begin
   Result := TConstIntExpr.Create(prog, typ, value);
end;

// CreateFloatValue
//
class function TConstExpr.CreateFloatValue(prog : TdwsProgram; const value : Int64) : TConstExpr;
begin
   Result := TConstFloatExpr.Create(prog, prog.TypFloat, value);
end;

// CreateFloatValue
//
class function TConstExpr.CreateFloatValue(prog : TdwsProgram; const value : Double) : TConstExpr;
begin
   Result := TConstFloatExpr.Create(prog, prog.TypFloat, value);
end;

// CreateStringValue
//
class function TConstExpr.CreateStringValue(prog : TdwsProgram; const value : UnicodeString) : TConstExpr;
begin
   Result := TConstStringExpr.Create(prog, prog.TypString, value);
end;

// CreateBooleanValue
//
class function TConstExpr.CreateBooleanValue(prog : TdwsProgram; const value : Boolean) : TConstExpr;
var
   unified : TUnifiedConstants;
begin
   unified := TUnifiedConstants(prog.Root.UnifiedConstants);
   if value then
      Result := unified.TrueConst
   else Result := unified.FalseConst;
   Result.IncRefCount;
end;

// CreateDynamicArrayValue
//
class function TConstExpr.CreateDynamicArrayValue(prog : TdwsProgram;
      typ : TTypeSymbol; const val : IScriptDynArray) : TConstExpr;
begin
   if val<>nil then
      Result:=TConstExpr.Create(prog, typ, val)
   else Result:=TConstExpr.Create(prog, typ, TScriptDynamicArray.CreateNew(typ.Typ) as IScriptDynArray);
end;

// CreateAssociativeArrayValue
//
class function TConstExpr.CreateAssociativeArrayValue(prog : TdwsProgram; typ : TAssociativeArraySymbol; const val : IScriptAssociativeArray) : TConstExpr;
begin
   if val<>nil then
      Result:=TConstExpr.Create(prog, typ, val)
   else Result:=TConstExpr.Create(prog, typ, TScriptAssociativeArray.CreateNew(typ.KeyType, typ.Typ) as IScriptAssociativeArray);
end;

// ------------------
// ------------------ TConstNilExpr ------------------
// ------------------

// EvalAsInteger
//
function TConstNilExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := 0;
end;

// EvalAsVariant
//
procedure TConstNilExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   VarCopySafe(Result, FData[0]);
end;

// EvalAsScriptObj
//
procedure TConstNilExpr.EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj);
begin
   result := nil;
end;

// EvalAsScriptObjInterface
//
procedure TConstNilExpr.EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface);
begin
   result := nil;
end;

// EvalAsScriptDynArray
//
procedure TConstNilExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
begin
   result := nil;
end;

// ------------------
// ------------------ TConstBooleanExpr ------------------
// ------------------

// Create
//
constructor TConstBooleanExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   Assert(TVarData(value).VType = varBoolean);
   if typ = nil then
      typ := prog.TypBoolean;
   FValue := value;
   inherited Create(prog, typ, value);
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
   Assert(TVarData(value).VType = varInt64);
   if typ = nil then
      typ := prog.TypInteger;
   FValue := value;
   inherited Create(prog, typ, value);
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
constructor TConstFloatExpr.Create(prog : TdwsProgram; typ : TTypeSymbol; const value : Variant);
begin
   if typ = nil then
      typ := prog.TypFloat;
   FValue := VariantToFloat(value);
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
   Result := FValue;
{$ifend}
end;

// ------------------
// ------------------ TConstStringExpr ------------------
// ------------------

// Create
//
constructor TConstStringExpr.Create(prog: TdwsProgram; typ: TTypeSymbol; const value: Variant);
begin
   if typ = nil then
      FTyp := prog.TypString
   else FTyp := typ;
   VariantToUnifiedString(value, FValue);
   SetLength(FData, 1);
   FData[0] := FValue;
end;

// EvalAsString
//
procedure TConstStringExpr.EvalAsString(exec : TdwsExecution; var Result : UnicodeString);
{$ifndef WIN32_ASM}
begin
   Result:=FValue;
{$else}
asm
   mov   edx, [eax + OFFSET FVALUE]
   mov   eax, ecx
   call  System.@UStrAsg;
{$endif}
end;

// SetValue
//
procedure TConstStringExpr.SetValue(const v : UnicodeString);
begin
   FValue := v;
   FData[0] := FValue;
end;

// ------------------
// ------------------ TConstArrayExpr ------------------
// ------------------

// Create
//
constructor TConstArrayExpr.Create(prog : TdwsProgram; symbol : TConstSymbol);
begin
   inherited CreateRef(symbol.Typ, symbol.Data);
   FSymbol:=symbol;
end;

// ------------------
// ------------------ TUnifiedConstants ------------------
// ------------------

// Precharge
//
constructor TUnifiedConstants.Create(prog : TdwsMainProgram; systemTable : TSystemSymbolTable);
const
   cZeroFloat : Double = 0;
   cNilIntf : IUnknown = nil;
var
   i : Integer;
begin
   inherited Create;
   // no lock is required here
   FEmptyString:=TConstStringExpr.Create(prog, systemTable.TypString, '');
   for i:=Low(FIntegers) to High(FIntegers) do
      FIntegers[i] := TConstIntExpr.Create(prog, systemTable.TypInteger, Int64(i));
   FZeroFloat := TConstFloatExpr.Create(prog, systemTable.TypFloat, cZeroFloat);
   FTrue := TConstBooleanExpr.Create(prog, systemTable.TypBoolean, True);
   FFalse := TConstBooleanExpr.Create(prog, systemTable.TypBoolean, False);
   FNil := TConstNilExpr.Create(prog, prog.TypNil, cNilIntf);
end;

// Destroy
//
destructor TUnifiedConstants.Destroy;
var
   i : Integer;
begin
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

// ------------------
// ------------------ TArrayConstantExpr ------------------
// ------------------

// Create
//
constructor TArrayConstantExpr.Create(Prog: TdwsProgram; const aScriptPos: TScriptPos);
var
   sas : TStaticArraySymbol;
begin
   sas := TStaticArraySymbol.Create('', prog.TypNil, prog.TypInteger, 0, -1);
   prog.Table.AddSymbol(sas);
   inherited Create(aScriptPos, sas);
end;

// Destroy
//
destructor TArrayConstantExpr.Destroy;
begin
   FElementExprs.Clean;
   inherited;
end;

// AddElementExpr
//
procedure TArrayConstantExpr.AddElementExpr(const scriptPos : TScriptPos; prog: TdwsProgram; ElementExpr: TTypedExpr);
var
   arraySymbol : TStaticArraySymbol;
begin
   FElementExprs.Add(ElementExpr);
   if ElementExpr.Typ=nil then
      prog.Root.CompileMsgs.AddCompilerStopFmt(scriptPos, CPE_IncompatibleTypes, [SYS_VOID, Typ.Typ.Caption]);

   arraySymbol:=(FTyp as TStaticArraySymbol);
   if arraySymbol.Typ<>Prog.TypVariant then begin
      if arraySymbol.Typ=Prog.TypNil then
         arraySymbol.Typ:=ElementExpr.Typ
      else if arraySymbol.Typ<>ElementExpr.Typ then begin
         if arraySymbol.Typ=Prog.TypNil then
            arraySymbol.Typ:=ElementExpr.Typ
         else if (arraySymbol.Typ=Prog.TypInteger) and (ElementExpr.Typ=Prog.TypFloat) then
            arraySymbol.Typ:=Prog.TypFloat
         else if ElementExpr.Typ.Size=1 then begin
            if not ((arraySymbol.Typ=Prog.TypFloat) and (ElementExpr.Typ=Prog.TypInteger)) then
               arraySymbol.Typ:=Prog.TypVariant;
         end else if not ElementExpr.Typ.IsCompatible(Typ.Typ) then begin
            prog.Root.CompileMsgs.AddCompilerStopFmt(scriptPos, CPE_IncompatibleTypes,
                                                     [ElementExpr.Typ.Caption, Typ.Typ.Caption]);
         end;
      end;
   end;
   arraySymbol.AddElement;
end;

// AddElementRange
//
procedure TArrayConstantExpr.AddElementRange(prog : TdwsProgram; const range1, range2 : Int64; typ : TTypeSymbol);
var
   i : Int64;
   d : Integer;
begin
   if range1<range2 then
      d:=1
   else d:=-1;
   i:=range1;
   repeat
      AddElementExpr(cNullPos, prog, TConstIntExpr.Create(prog, typ, i));
      if i=range2 then break;
      Inc(i, d);
   until False;
end;

// Prepare
//
procedure TArrayConstantExpr.Prepare(prog : TdwsProgram; elementTyp : TTypeSymbol);
var
   x : Integer;
   elemExpr : TTypedExpr;
begin
   if (elementTyp<>nil) and (FTyp.Typ<>elementTyp) then begin
      if  (elementTyp.UnAliasedTypeIs(TBaseFloatSymbol) and FTyp.Typ.UnAliasedTypeIs(TBaseIntegerSymbol)) then begin
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
end;

// GetDataPtr
//
procedure TArrayConstantExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   EvalNoResult(exec);
   result := FArrayData;
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

// EvalNoResult
//
procedure TArrayConstantExpr.EvalNoResult(exec : TdwsExecution);

   procedure DoEval;
   var
      x, n, addr : Integer;
      elemSize : Integer;
      elemExpr : TTypedExpr;
      dataExpr : TDataExpr;
   begin
      if FArrayData=nil then begin
         if FTyp.Typ<>nil then
            n:=FElementExprs.Count * FTyp.Typ.Size
         else n:=0;
         FArrayData := TDataContext.CreateStandalone(n);
      end;

      elemSize:=Typ.Typ.Size;
      if elemSize=1 then begin
         for x:=0 to FElementExprs.Count-1 do begin
            elemExpr:=TTypedExpr(FElementExprs.List[x]);
            elemExpr.EvalAsVariant(exec, FArrayData.AsPVariant(x)^);
         end;
      end else begin
         addr:=0;
         for x:=0 to FElementExprs.Count-1 do begin
            dataExpr:=FElementExprs.List[x] as TDataExpr;
            dataExpr.DataPtr[exec].CopyData(FArrayData.AsPData^, addr, elemSize);
            Inc(addr, elemSize);
         end;
      end;
   end;

begin
   if FArrayData=nil then
      DoEval;
end;

// EvalAsVariant
//
procedure TArrayConstantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   data : TData;
begin
   EvalAsTData(exec, data);
   if Length(data)>0 then
      VarCopySafe(Result, data[0])
   else VarClearSafe(Result);
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
      if expr.ClassType=TArrayConstantExpr then
         TArrayConstantExpr(expr).EvalToTData(exec, result, p)
      else if expr is TConstExpr then
         DWSCopyData(TConstExpr(expr).Data, 0, result, p, expr.Typ.Size)
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
      elemTyp := Typ.Typ;
      if elemTyp is TBaseVariantSymbol then
         elemTyp:=Elements[0].Typ;
      for x:=0 to FElementExprs.Count-1 do begin
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
            end else if (expr.Typ is TStructuredTypeMetaSymbol) and (elemTyp is TStructuredTypeMetaSymbol) then begin
               repeat
                  elemTyp:=TStructuredTypeMetaSymbol(elemTyp).Parent;
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
