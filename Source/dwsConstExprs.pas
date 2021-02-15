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
   SysUtils,
   dwsUtils, dwsDataContext, dwsStack, dwsXPlatform, dwsErrors, dwsStrings,
   dwsExprs, dwsExprList, dwsSymbols, dwsUnitSymbols, dwsScriptSource,
   dwsCompilerContext;

type

   TConstIntExpr = class;

   // A constant value (like 0, 3.14159, 'Hello' or true)
   TConstExpr = class(TDataExpr)
      protected
         FData : TData;

         function GetIsConstant : Boolean; override;

      public
         constructor Create(aTyp: TTypeSymbol; const Value: Variant); overload;
         constructor Create(aTyp: TTypeSymbol; const initData : TData; addr : Integer); overload;
         constructor Create(aTyp: TTypeSymbol); overload;
         constructor CreateRef(aTyp: TTypeSymbol; const Data: TData);
         constructor CreateNull(aTyp: TTypeSymbol);
         constructor CreateDefault(aTyp: TTypeSymbol);
         procedure Orphan(context : TdwsCompilerContext); override;

         procedure EvalAsString(exec : TdwsExecution; var result : String); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj); override;
         procedure EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface); override;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         function IsWritable : Boolean; override;
         function SameValueAs(otherConst : TConstExpr) : Boolean;
         function SameDataExpr(expr : TTypedExpr) : Boolean; override;

         function  SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
         property Data : TData read FData;

         class function CreateTyped(context : TdwsCompilerContext; Typ: TTypeSymbol; const Data: TData; addr : Integer = 0) : TConstExpr; overload; static;
         class function CreateTyped(context : TdwsCompilerContext; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr; overload; static;
   end;

   // TConstNilExpr
   //
   TConstNilExpr = class(TConstExpr)
      public
         constructor Create(aTyp : TTypeSymbol);

         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj); override;
         procedure EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface); override;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;
   end;

   // TConstBooleanExpr
   //
   TConstBooleanExpr = class sealed(TConstExpr)
      protected
         FValue : Boolean;

      public
         constructor Create(aTyp : TTypeSymbol; const aValue : Boolean);

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
         constructor Create(typ : TTypeSymbol; const aValue : Int64);

         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         property Value : Int64 read FValue write FValue;

         function ValueIsInt32 : Boolean; inline;
   end;

   // TConstFloatExpr
   //
   TConstFloatExpr = class sealed (TConstExpr)
      private
         FValue : Double;

      public
         constructor Create(typ : TTypeSymbol; const aValue : Double);

         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         property Value : Double read FValue;
   end;

   // TConstStringExpr
   //
   TConstStringExpr = class(TConstExpr)
      private
         FValue : String;

         procedure SetValue(const v : String);

      public
         constructor Create(typ : TTypeSymbol; const aValue : String);

         procedure EvalAsString(exec : TdwsExecution; var result : String); override;
         property Value : String read FValue write SetValue;
   end;

   // TConstArrayExpr
   //
   TConstArrayExpr = class (TConstExpr)
      private
         FSymbol : TConstSymbol;

      public
         constructor Create(context : TdwsCompilerContext; symbol : TConstSymbol);

         property Symbol : TConstSymbol read FSymbol;
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
         constructor Create(context : TdwsCompilerContext; const aScriptPos: TScriptPos);
         destructor Destroy; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property Elements[idx : Integer] : TTypedExpr read GetElement;
         property ElementCount : Integer read GetElementCount;
         procedure AddElementExpr(const scriptPos : TScriptPos; context : TdwsCompilerContext; ElementExpr: TTypedExpr);
         procedure AddElementRange(context : TdwsCompilerContext; const range1, range2 : Int64; typ : TTypeSymbol);
         procedure Prepare(context : TdwsCompilerContext; elementTyp : TTypeSymbol);
         procedure TypeCheckElements(context : TdwsCompilerContext);
         procedure ElementsFromIntegerToFloat(context : TdwsCompilerContext);

         function Size : Integer; inline;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsTData(exec : TdwsExecution; var result : TData);
         procedure EvalToTData(exec : TdwsExecution; var result : TData; offset : Integer);
         function  EvalAsVarRecArray(exec : TdwsExecution) : TVarRecArrayContainer;

         function Optimize(context : TdwsCompilerContext) : TProgramExpr; override;
         function IsWritable : Boolean; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsConvExprs, dwsSpecializationContext, dwsDynamicArrays;

// ------------------
// ------------------ TConstExpr ------------------
// ------------------

// Create
//
constructor TConstExpr.Create(aTyp: TTypeSymbol; const Value: Variant);
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
constructor TConstExpr.Create(aTyp: TTypeSymbol; const initData: TData; addr : Integer);
begin
   Create(aTyp);
   if initData <> nil then
      DWSCopyData(initData, addr, FData, 0, aTyp.Size);
end;

// Create
//
constructor TConstExpr.Create(aTyp: TTypeSymbol);
begin
   inherited Create(aTyp);
   if aTyp <> nil then
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
      VarSetNull(FData[i]);
end;

// CreateDefault
//
constructor TConstExpr.CreateDefault(aTyp: TTypeSymbol);
begin
   Create(aTyp);
   aTyp.InitData(FData, 0);
end;

// Orphan
//
procedure TConstExpr.Orphan(context : TdwsCompilerContext);
begin
   DecRefCount;
end;

// EvalAsString
//
procedure TConstExpr.EvalAsString(exec : TdwsExecution; var result : String);
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

// SpecializeDataExpr
//
function TConstExpr.SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr;
begin
   Result := CreateTyped(CompilerContextFromSpecialization(context),
                         context.SpecializeType(Typ), Data, 0);
end;

// GetDataPtr
//
procedure TConstExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   exec.DataContext_Create(FData, 0, Result);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(context : TdwsCompilerContext; Typ: TTypeSymbol; const Data: TData; addr : Integer = 0) : TConstExpr;
begin
   case Length(Data) of
      0 : Result := TConstExpr.CreateNull(Typ);
      1 : Result := (context.CreateConstExpr(typ, data[addr]) as TConstExpr);
   else
      Result := TConstExpr.Create(Typ, Data, addr);
   end;
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(context : TdwsCompilerContext; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr;
begin
   Assert(constSymbol<>nil);
   if constSymbol.Typ is TArraySymbol then
      Result:=TConstArrayExpr.Create(context, constSymbol)
   else Result:=CreateTyped(context, Typ, constSymbol.Data);
end;

// ------------------
// ------------------ TConstNilExpr ------------------
// ------------------

// Create
//
constructor TConstNilExpr.Create(aTyp : TTypeSymbol);
begin
   inherited Create(typ);
   FTyp := aTyp;
   SetLength(FData, 1);
   TVarData(FData[0]).VType := varUnknown;
   TVarData(FData[0]).VUnknown := nil;
end;

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
constructor TConstBooleanExpr.Create(aTyp : TTypeSymbol; const aValue : Boolean);
begin
   inherited Create(typ);
   FTyp := aTyp;
   FValue := aValue;
   SetLength(FData, 1);
   TVarData(FData[0]).VType := varBoolean;
   TVarData(FData[0]).VBoolean := aValue;
end;

// EvalAsInteger
//
function TConstBooleanExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := Integer(FValue);
end;

// EvalAsBoolean
//
function TConstBooleanExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := FValue;
end;

// ------------------
// ------------------ TConstIntExpr ------------------
// ------------------

// Create
//
constructor TConstIntExpr.Create(typ : TTypeSymbol; const aValue : Int64);
begin
   inherited Create(typ);
   FTyp := typ;
   FValue := aValue;
   SetLength(FData, 1);
   TVarData(FData[0]).VType := varInt64;
   TVarData(FData[0]).VInt64 := aValue;
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

// ValueIsInt32
//
function TConstIntExpr.ValueIsInt32 : Boolean;
begin
   Result := Int32(FValue) = FValue;
end;

// ------------------
// ------------------ TConstFloatExpr ------------------
// ------------------

// Create
//
constructor TConstFloatExpr.Create(typ : TTypeSymbol; const aValue : Double);
begin
   inherited Create(typ);
   FTyp := typ;
   FValue := aValue;
   SetLength(FData, 1);
   TVarData(FData[0]).VType := varDouble;
   TVarData(FData[0]).VDouble := aValue;
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
constructor TConstStringExpr.Create(typ : TTypeSymbol; const aValue : String);
begin
   inherited Create(typ);
   FTyp := typ;
   FValue := aValue;
   SetLength(FData, 1);
   {$ifdef FPC}
   TVarData(FData[0]).VType := varString;
   String(TVarData(FData[0]).VString) := aValue;
   {$else}
   TVarData(FData[0]).VType := varUString;
   String(TVarData(FData[0]).VUString) := aValue;
   {$endif}
end;

// EvalAsString
//
procedure TConstStringExpr.EvalAsString(exec : TdwsExecution; var result : String);
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
procedure TConstStringExpr.SetValue(const v : String);
begin
   FValue := v;
   FData[0] := FValue;
end;

// ------------------
// ------------------ TConstArrayExpr ------------------
// ------------------

// Create
//
constructor TConstArrayExpr.Create(context : TdwsCompilerContext; symbol : TConstSymbol);
begin
   inherited CreateRef(symbol.Typ, symbol.Data);
   FSymbol:=symbol;
end;

// ------------------
// ------------------ TArrayConstantExpr ------------------
// ------------------

// Create
//
constructor TArrayConstantExpr.Create(context : TdwsCompilerContext; const aScriptPos: TScriptPos);
var
   sas : TStaticArraySymbol;
begin
   sas := TStaticArraySymbol.Create('', context.TypNil, context.TypInteger, 0, -1);
   context.Table.AddSymbol(sas);
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
procedure TArrayConstantExpr.AddElementExpr(const scriptPos : TScriptPos; context: TdwsCompilerContext; ElementExpr: TTypedExpr);
var
   arraySymbol : TStaticArraySymbol;
begin
   FElementExprs.Add(ElementExpr);
   if ElementExpr.Typ=nil then
      context.Msgs.AddCompilerStopFmt(scriptPos, CPE_IncompatibleTypes, [SYS_VOID, Typ.Typ.Caption]);

   arraySymbol:=(FTyp as TStaticArraySymbol);
   if arraySymbol.Typ<>context.TypVariant then begin
      if arraySymbol.Typ=context.TypNil then
         arraySymbol.Typ:=ElementExpr.Typ
      else if arraySymbol.Typ<>ElementExpr.Typ then begin
         if arraySymbol.Typ=context.TypNil then
            arraySymbol.Typ:=ElementExpr.Typ
         else if (arraySymbol.Typ=context.TypInteger) and (ElementExpr.Typ=context.TypFloat) then
            arraySymbol.Typ:=context.TypFloat
         else if ElementExpr.Typ.Size=1 then begin
            if not ((arraySymbol.Typ=context.TypFloat) and (ElementExpr.Typ=context.TypInteger)) then
               arraySymbol.Typ:=context.TypVariant;
         end else if not ElementExpr.Typ.IsCompatible(Typ.Typ) then begin
            context.Msgs.AddCompilerStopFmt(scriptPos, CPE_IncompatibleTypes,
                                            [ElementExpr.Typ.Caption, Typ.Typ.Caption]);
         end;
      end;
   end;
   arraySymbol.AddElement;
end;

// AddElementRange
//
procedure TArrayConstantExpr.AddElementRange(context : TdwsCompilerContext; const range1, range2 : Int64; typ : TTypeSymbol);
var
   i : Int64;
   d : Integer;
begin
   if typ = nil then
      typ := context.TypInteger;
   if range1<range2 then
      d:=1
   else d:=-1;
   i:=range1;
   repeat
      AddElementExpr(cNullPos, context, TConstIntExpr.Create(typ, i));
      if i=range2 then break;
      Inc(i, d);
   until False;
end;

// Prepare
//
procedure TArrayConstantExpr.Prepare(context : TdwsCompilerContext; elementTyp : TTypeSymbol);
var
   elemExpr : TRefCountedObject;
begin
   if (elementTyp<>nil) and (FTyp.Typ<>elementTyp) then begin
      if  (elementTyp.UnAliasedTypeIs(TBaseFloatSymbol) and FTyp.Typ.UnAliasedTypeIs(TBaseIntegerSymbol)) then begin
         ElementsFromIntegerToFloat(context);
      end else if elementTyp.IsCompatible(FTyp.Typ) then begin
         (FTyp as TStaticArraySymbol).Typ:=elementTyp;
      end;
   end;

   for elemExpr in FElementExprs do
      if elemExpr.ClassType = TArrayConstantExpr then
         TArrayConstantExpr(elemExpr).Prepare(context, FTyp.Typ);
end;

// GetDataPtr
//
procedure TArrayConstantExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   result := TDataContext.CreateStandalone(Size);
   EvalToTData(exec, result.AsPData^, 0);
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
var
   buf : TData;
begin
   EvalAsTData(exec, buf);
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
   else VarCopySafe(Result, CreateNewDynamicArray(Typ));
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
   p, s : Integer;
   expr : TRefCountedObject;
begin
   p := offset;
   for expr in FElementExprs do begin
      s := TTypedExpr(expr).Typ.Size;
      if expr.ClassType=TArrayConstantExpr then
         TArrayConstantExpr(expr).EvalToTData(exec, result, p)
      else if expr is TConstExpr then
         DWSCopyData(TConstExpr(expr).Data, 0, result, p, s)
      else if s = 1 then
         TTypedExpr(expr).EvalAsVariant(exec, result[p])
      else (expr as TDataExpr).DataPtr[exec].CopyData(result, p, s);
      Inc(p, s);
   end;
end;

// EvalAsVarRecArray
//
function TArrayConstantExpr.EvalAsVarRecArray(exec : TdwsExecution) : TVarRecArrayContainer;
var
   element : TRefCountedObject;
   buf : Variant;
begin
   Result := TVarRecArrayContainer.Create;
   for element in FElementExprs do begin
      TTypedExpr(element).EvalAsVariant(exec, buf);
      Result.Add(buf);
   end;
   Result.Initialize;
end;

// Optimize
//
function TArrayConstantExpr.Optimize(context : TdwsCompilerContext) : TProgramExpr;
var
   i : Integer;
   expr : TTypedExpr;
begin
   Result:=Self;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      FElementExprs.List[i]:=expr.Optimize(context);
   end;
end;

// GetIsConstant
//
function TArrayConstantExpr.GetIsConstant : Boolean;
var
   elem : TRefCountedObject;
begin
   for elem in FElementExprs do
      if not TTypedExpr(elem).IsConstant then
         Exit(False);
   Result := True;
end;

// IsWritable
//
function TArrayConstantExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// TypeCheckElements
//
procedure TArrayConstantExpr.TypeCheckElements(context : TdwsCompilerContext);
var
   x : Integer;
   expr : TTypedExpr;
   elemTyp : TTypeSymbol;
begin
   if Typ.Typ=nil then
      context.Msgs.AddCompilerErrorFmt(ScriptPos, CPE_InvalidConstType, [SYS_VOID])
   else if FElementExprs.Count>0 then begin
      elemTyp := Typ.Typ;
      if elemTyp is TBaseVariantSymbol then
         elemTyp:=Elements[0].Typ;
      for x:=0 to FElementExprs.Count-1 do begin
         expr:=Elements[x];
         if not elemTyp.IsCompatible(expr.Typ) then begin
            if elemTyp.IsOfType(context.TypInteger) and expr.Typ.IsOfType(context.TypFloat) then
               elemTyp:=context.TypFloat
            else if elemTyp.IsOfType(context.TypFloat) and expr.Typ.IsOfType(context.TypInteger) then
               // handled below
            else if expr.Typ.IsCompatible(elemTyp) then
               elemTyp:=expr.Typ
            else if (expr.Typ is TStructuredTypeSymbol) and (elemTyp is TStructuredTypeSymbol) then begin
               repeat
                  elemTyp:=TStructuredTypeSymbol(elemTyp).Parent;
                  if elemTyp=nil then begin
                     context.Msgs.AddCompilerErrorFmt(ScriptPos, CPE_AssignIncompatibleTypes,
                                                      [expr.Typ.Caption, Elements[0].Typ.Caption]);
                     Exit;
                  end;
               until elemTyp.IsCompatible(expr.Typ);
            end else if (expr.Typ is TStructuredTypeMetaSymbol) and (elemTyp is TStructuredTypeMetaSymbol) then begin
               repeat
                  elemTyp:=TStructuredTypeMetaSymbol(elemTyp).Parent;
                  if elemTyp=nil then begin
                     context.Msgs.AddCompilerErrorFmt(ScriptPos, CPE_AssignIncompatibleTypes,
                                                      [expr.Typ.Caption, Elements[0].Typ.Caption]);
                     Exit;
                  end;
               until elemTyp.IsCompatible(expr.Typ);
            end else if context.TypVariant.IsCompatible(expr.Typ) and context.TypVariant.IsCompatible(elemTyp) then
               elemTyp:=context.TypVariant
            else begin
               context.Msgs.AddCompilerErrorFmt(ScriptPos, CPE_AssignIncompatibleTypes,
                                                [expr.Typ.Caption, elemTyp.Caption]);
               Exit;
            end;
         end;
      end;

      // implicit cast integer to float
      if elemTyp.IsOfType(context.TypFloat) then
         ElementsFromIntegerToFloat(context);

      Typ.Typ:=elemTyp;
   end;
end;

// ElementsFromIntegerToFloat
//
procedure TArrayConstantExpr.ElementsFromIntegerToFloat(context : TdwsCompilerContext);
var
   x : Integer;
   expr : TTypedExpr;
begin
   for x:=0 to FElementExprs.Count-1 do begin
      expr:=Elements[x];
      if expr.Typ.IsOfType(context.TypInteger) then begin
         expr:=TConvIntToFloatExpr.Create(context, ScriptPos, expr);
         FElementExprs.List[x]:=expr;
      end;
   end;
   Typ.Typ:=context.TypFloat;
end;

end.
