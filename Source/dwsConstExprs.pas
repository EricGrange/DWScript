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
         FDataContext : IDataContext;

         function GetIsConstant : Boolean; override;

      public
         constructor Create(const scriptPos : TScriptPos; aTyp: TTypeSymbol);
         constructor CreateValue(const scriptPos : TScriptPos; aTyp: TTypeSymbol; const value: Variant);
         constructor CreateData(const scriptPos : TScriptPos; aTyp: TTypeSymbol; const initData : IDataContext);
         constructor CreateNull(const scriptPos : TScriptPos; aTyp: TTypeSymbol);

         procedure Orphan(context : TdwsCompilerContext); override;

         procedure EvalAsString(exec : TdwsExecution; var result : String); override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj); override;
         procedure EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface); override;

         function IsWritable : Boolean; override;
         function SameValueAs(otherConst : TConstExpr) : Boolean;
         function SameDataExpr(expr : TTypedExpr) : Boolean; override;

         function  SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
         property DataContext : IDataContext read FDataContext;

         class function CreateTyped(context : TdwsCompilerContext; Typ: TTypeSymbol; const initData : IDataContext) : TConstExpr; overload; static;
         class function CreateTyped(context : TdwsCompilerContext; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr; overload; static;
   end;

   // TConstNilExpr
   //
   TConstNilExpr = class sealed (TConstExpr)
      public
         constructor Create(const scriptPos : TScriptPos; aTyp : TTypeSymbol);

         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj); override;
         procedure EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface); override;
   end;

   // TConstBooleanExpr
   //
   TConstBooleanExpr = class sealed(TConstExpr)
      protected
         FValue : Boolean;

      public
         constructor Create(const scriptPos : TScriptPos; aTyp : TTypeSymbol; const aValue : Boolean);

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
         constructor Create(const scriptPos : TScriptPos; typ : TTypeSymbol; const aValue : Int64);

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
         constructor Create(const scriptPos : TScriptPos; typ : TTypeSymbol; const aValue : Double);

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
         constructor Create(const scriptPos : TScriptPos; typ : TTypeSymbol; const aValue : String);

         procedure EvalAsString(exec : TdwsExecution; var result : String); override;
         property Value : String read FValue write SetValue;
   end;

   // TConstArrayExpr
   //
   TConstArrayExpr = class (TConstExpr)
      private
         FSymbol : TConstSymbol;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos; symbol : TConstSymbol);

         property Symbol : TConstSymbol read FSymbol;
   end;

   // TArrayConstantExpr
   //
   TArrayConstantExpr = class sealed (TDataExpr)
      protected
         FElementExprs : TTightList;

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
         procedure EvalToDataContext(exec : TdwsExecution; const destDC : IDataContext; offset : Integer);
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
constructor TConstExpr.Create(const scriptPos : TScriptPos; aTyp: TTypeSymbol);
begin
   inherited Create(scriptPos, aTyp);
   if aTyp <> nil then
      FDataContext := TDataContext.CreateStandalone(aTyp.Size);
end;

// CreateValue
//
constructor TConstExpr.CreateValue(const scriptPos : TScriptPos; aTyp: TTypeSymbol; const value: Variant);
begin
   Create(scriptPos, aTyp);
   case aTyp.Size of
      0 : ;
      1 : FDataContext.AsVariant[0] := value;
   else
      Assert(False);
   end;
end;

// CreateDC
//
constructor TConstExpr.CreateData(const scriptPos : TScriptPos; aTyp: TTypeSymbol; const initData: IDataContext);
begin
   Create(scriptPos, aTyp);
   if initData <> nil then
      FDataContext.WriteData(0, initData, 0, aTyp.Size);
end;

// CreateNull
//
constructor TConstExpr.CreateNull(const scriptPos : TScriptPos; aTyp: TTypeSymbol);
var
   i : Integer;
begin
   Create(scriptPos, aTyp);
   for i := 0 to aTyp.Size-1 do
      FDataContext.SetNullVariant(i);
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
   FDataContext.EvalAsString(0, result);
end;

// EvalAsVariant
//
procedure TConstExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   FDataContext.EvalAsVariant(0, result);
end;

// EvalAsScriptObj
//
procedure TConstExpr.EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj);
begin
   FDataContext.EvalAsInterface(0, IInterface(result));
end;

// EvalAsScriptObjInterface
//
procedure TConstExpr.EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface);
begin
   FDataContext.EvalAsInterface(0, IInterface(result));
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
   Result := FDataContext.SameData(otherConst.FDataContext);
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
                         context.SpecializeType(Typ), DataContext);
end;

// GetDataPtr
//
procedure TConstExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   result := FDataContext;
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(context : TdwsCompilerContext; Typ: TTypeSymbol;
                                      const initData : IDataContext) : TConstExpr;
begin
   case Typ.Size of
      0 : Result := TConstExpr.CreateNull(cNullPos, Typ);
      1 : Result := (context.CreateConstExpr(typ, initData.AsVariant[0]) as TConstExpr);
   else
      Result := TConstExpr.CreateData(cNullPos, Typ, initData);
   end;
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(context : TdwsCompilerContext; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr;
begin
   Assert(constSymbol<>nil);
   if constSymbol.Typ is TArraySymbol then
      Result := TConstArrayExpr.Create(context, cNullPos, constSymbol)
   else Result := CreateTyped(context, Typ, constSymbol.DataContext);
end;

// ------------------
// ------------------ TConstNilExpr ------------------
// ------------------

// Create
//
constructor TConstNilExpr.Create(const scriptPos : TScriptPos; aTyp : TTypeSymbol);
begin
   inherited Create(scriptPos, aTyp);
   FDataContext.AsInterface[0] := nil;
end;

// EvalAsInteger
//
function TConstNilExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := 0;
end;

// EvalAsVariant
//
procedure TConstNilExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   FDataContext.EvalAsVariant(0, result);
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

// ------------------
// ------------------ TConstBooleanExpr ------------------
// ------------------

// Create
//
constructor TConstBooleanExpr.Create(const scriptPos : TScriptPos; aTyp : TTypeSymbol; const aValue : Boolean);
begin
   inherited Create(scriptPos, aTyp);
   FValue := aValue;
   FDataContext.AsBoolean[0] := aValue;
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
constructor TConstIntExpr.Create(const scriptPos : TScriptPos; typ : TTypeSymbol; const aValue : Int64);
begin
   inherited Create(scriptPos, typ);
   FTyp := typ;
   FValue := aValue;
   FDataContext.AsInteger[0] := aValue;
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
constructor TConstFloatExpr.Create(const scriptPos : TScriptPos; typ : TTypeSymbol; const aValue : Double);
begin
   inherited Create(scriptPos, typ);
   FTyp := typ;
   FValue := aValue;
   FDataContext.AsFloat[0] := aValue;
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
constructor TConstStringExpr.Create(const scriptPos : TScriptPos; typ : TTypeSymbol; const aValue : String);
begin
   inherited Create(scriptPos, typ);
   FTyp := typ;
   FValue := aValue;
   FDataContext.AsString[0] := aValue;
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
   FDataContext.AsString[0] := v;
end;

// ------------------
// ------------------ TConstArrayExpr ------------------
// ------------------

// Create
//
constructor TConstArrayExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos; symbol : TConstSymbol);
begin
   inherited CreateData(scriptPos, symbol.Typ, symbol.DataContext);
   FSymbol := symbol;
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
         if (arraySymbol.Typ=context.TypInteger) and (ElementExpr.Typ=context.TypFloat) then
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
   Assert(typ <> nil);
   if range1<range2 then
      d:=1
   else d:=-1;
   i:=range1;
   repeat
      AddElementExpr(cNullPos, context, TConstIntExpr.Create(ScriptPos, typ, i));
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
   EvalToDataContext(exec, result, 0);
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
begin
   // nothing
end;

// EvalAsVariant
//
procedure TArrayConstantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   if FElementExprs.Count = 0 then begin
      if TVarData(result).VType <> varUnknown then begin
         VarClearSafe(result);
         TVarData(result).VType := varUnknown;
      end;
      CreateNewDynamicArray(Typ, IScriptDynArray(TVarData(result).VUnknown));
   end else TTypedExpr(FElementExprs.List[0]).EvalAsVariant(exec, Result);
end;

// EvalToDataContext
//
procedure TArrayConstantExpr.EvalToDataContext(exec : TdwsExecution; const destDC : IDataContext; offset : Integer);
var
   p, s : Integer;
   expr : TRefCountedObject;
   v : Variant;
begin
   p := offset;
   for expr in FElementExprs do begin
      s := TTypedExpr(expr).Typ.Size;
      if expr.ClassType = TArrayConstantExpr then
         TArrayConstantExpr(expr).EvalToDataContext(exec, destDC, p)
      else if expr is TConstExpr then
         destDC.WriteData(p, TConstExpr(expr).DataContext, 0, s)
      else if s = 1 then begin
         TTypedExpr(expr).EvalAsVariant(exec, v);
         destDC.AsVariant[p] := v;
      end else destDC.WriteData(p, (expr as TDataExpr).DataPtr[exec], 0, s);
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
