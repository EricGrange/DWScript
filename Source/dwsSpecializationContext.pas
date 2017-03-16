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
unit dwsSpecializationContext;

interface

uses
   dwsUtils, dwsSymbols, dwsScriptSource, dwsErrors, dwsStrings,
   dwsCompilerContext, dwsOperators;

type

   TSpecializedSymbol = record
      Generic : TSymbol;
      Specialized : TSymbol;
   end;

   // TSpecializationContext
   //
   TSpecializationContext = class (TInterfacedSelfObject, ISpecializationContext)
      private
         FName : String;
         FParameters, FValues : TUnSortedSymbolTable; // referred
         FValuesPosList : TScriptPosArray;
         FUnitSymbol : TSymbol;
         FScriptPos : TScriptPos;
         FCompositeSymbol : TCompositeTypeSymbol;
         FCompositeStack : TTightList;
         FFuncSymbol : TFuncSymbol;
         FFuncSymbolStack : TTightList;
         FSpecializedSymbols : array of TSpecializedSymbol;
         FOptimize : Boolean;
         FCompilerContext : TdwsCompilerContext;
         FOperators : TOperators;

      protected
         function Name : String;
         function Parameters : TUnSortedSymbolTable;
         function Values : TUnSortedSymbolTable;
         function UnitSymbol : TSymbol;
         function Msgs : TdwsCompileMessageList;
         function Optimize : Boolean;

      public
         constructor Create(const aName : String; aParams, aValues : TUnSortedSymbolTable;
                            const aScriptPos : TScriptPos; aUnit : TSymbol;
                            const aValuesPosList : TScriptPosArray;
                            const aCompilerContext : TdwsCompilerContext;
                            const aOperators : TOperators;
                            allowOptimization : Boolean);
         destructor Destroy; override;

         function Specialize(sym : TSymbol) : TSymbol;
         function SpecializeType(typ : TTypeSymbol) : TTypeSymbol;
         function SpecializeDataSymbol(ds : TDataSymbol) : TDataSymbol;
         function SpecializeField(fld : TFieldSymbol) : TFieldSymbol;
         function SpecializeExecutable(const exec : IExecutable) : IExecutable;

         procedure RegisterSpecialization(generic, specialized : TSymbol);

         procedure AddCompilerHint(const msg : String);
         procedure AddCompilerError(const msg : String);
         procedure AddCompilerErrorFmt(const msgFmt : String; const params : array of const); overload;
         procedure AddCompilerErrorFmt(const aScriptPos : TScriptPos; const msgFmt : String;
                                       const params : array of const); overload;

         property CompilerContext : TdwsCompilerContext read FCompilerContext;
         property Operators : TOperators read FOperators;
         property ScriptPos : TScriptPos read FScriptPos;
         function ParameterValuePos(parameter : TSymbol) : TScriptPos;

         procedure EnterComposite(sym : TCompositeTypeSymbol);
         procedure LeaveComposite;
         function  CompositeSymbol : TCompositeTypeSymbol;

         procedure EnterFunction(funcSym : TFuncSymbol);
         procedure LeaveFunction;
         function  FuncSymbol : TFuncSymbol;
   end;

   TSpecializationMethod = function (const context : ISpecializationContext) : TTypeSymbol of object;

function CompilerContextFromSpecialization(const specializationContext : ISpecializationContext) : TdwsCompilerContext;
function OperatorsFromSpecialization(const specializationContext : ISpecializationContext) : TOperators;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// CompilerContextFromSpecialization
//
function CompilerContextFromSpecialization(const specializationContext : ISpecializationContext) : TdwsCompilerContext;
begin
   Result := (specializationContext.GetSelf as TSpecializationContext).CompilerContext;
end;

// OperatorsFromSpecialization
//
function OperatorsFromSpecialization(const specializationContext : ISpecializationContext) : TOperators;
begin
   Result := (specializationContext.GetSelf as TSpecializationContext).Operators;
end;

// ------------------
// ------------------ TSpecializationContext ------------------
// ------------------

// Create
//
constructor TSpecializationContext.Create(
      const aName : String; aParams, aValues : TUnSortedSymbolTable;
      const aScriptPos : TScriptPos; aUnit : TSymbol;
      const aValuesPosList : TScriptPosArray;
      const aCompilerContext : TdwsCompilerContext;
      const aOperators : TOperators;
      allowOptimization : Boolean);
begin
   inherited Create;
   FName := aName;
   Assert(aParams.Count = aValues.Count);
   FParameters := aParams;
   FValues := aValues;
   FValuesPosList := aValuesPosList;
   FUnitSymbol := aUnit;
   FCompilerContext := aCompilerContext;
   FOperators := aOperators;
   FOptimize := allowOptimization;
   FScriptPos := aScriptPos;
end;

// Destroy
//
destructor TSpecializationContext.Destroy;
begin
   Assert(FCompositeStack.Count=0);
   inherited;
end;

// Specialize
//
function TSpecializationContext.Specialize(sym : TSymbol) : TSymbol;
var
   i : Integer;
begin
   if sym = nil then Exit(nil);
   for i := 0 to FParameters.Count-1 do
      if FParameters.Symbols[i] = sym then
         Exit(FValues.Symbols[i] as TTypeSymbol);
   for i := 0 to High(FSpecializedSymbols) do
      if FSpecializedSymbols[i].Generic = sym then
         Exit(FSpecializedSymbols[i].Specialized);
   if sym.IsGeneric then
      AddCompilerErrorFmt(CPE_SpecializationNotSupportedYet, [sym.ClassName]);
   Result := sym;
end;

// SpecializeType
//
function TSpecializationContext.SpecializeType(typ : TTypeSymbol) : TTypeSymbol;
var
   sym : TSymbol;
begin
   sym := Specialize(typ);
   if sym <> nil then
      Result := sym as TTypeSymbol
   else Result := nil;
end;

// SpecializeDataSymbol
//
function TSpecializationContext.SpecializeDataSymbol(ds : TDataSymbol) : TDataSymbol;
var
   sym : TSymbol;
begin
   sym := Specialize(ds);
   if sym <> nil then
      Result := sym as TDataSymbol
   else Result := nil;
end;

// SpecializeField
//
function TSpecializationContext.SpecializeField(fld : TFieldSymbol) : TFieldSymbol;
var
   sym : TSymbol;
begin
   sym := SpecializeType(fld.StructSymbol);
   if sym = nil then Exit(nil);

   sym := Specialize(fld);
   if sym <> nil then
      Result := sym as TFieldSymbol
   else Result := nil;
end;

// SpecializeExecutable
//
function TSpecializationContext.SpecializeExecutable(const exec : IExecutable) : IExecutable;
begin
   if exec <> nil then
      Result := exec.Specialize(Self)
   else Result := nil;
end;

// RegisterSpecialization
//
procedure TSpecializationContext.RegisterSpecialization(generic, specialized : TSymbol);
var
   n : Integer;
begin
   n := Length(FSpecializedSymbols);
   SetLength(FSpecializedSymbols, n+1);
   FSpecializedSymbols[n].Generic := generic;
   FSpecializedSymbols[n].Specialized := specialized;
end;

// AddCompilerHint
//
procedure TSpecializationContext.AddCompilerHint(const msg : String);
begin
   FCompilerContext.Msgs.AddCompilerHint(ScriptPos, msg);
end;

// AddCompilerError
//
procedure TSpecializationContext.AddCompilerError(const msg : String);
begin
   FOptimize := False;
   FCompilerContext.Msgs.AddCompilerError(ScriptPos, msg);
end;

// AddCompilerErrorFmt
//
procedure TSpecializationContext.AddCompilerErrorFmt(const msgFmt : String; const params : array of const);
begin
   FOptimize := False;
   FCompilerContext.Msgs.AddCompilerErrorFmt(ScriptPos, msgFmt, params);
end;

// ParameterValuePos
//
function TSpecializationContext.ParameterValuePos(parameter : TSymbol) : TScriptPos;
var
   i : Integer;
begin
   i := FParameters.IndexOf(parameter);
   if (i >= 0) and (i < Length(FValuesPosList)) then
      Result := FValuesPosList[i]
   else Result := FScriptPos;
end;

// AddCompilerErrorFmt
//
procedure TSpecializationContext.AddCompilerErrorFmt(
      const aScriptPos : TScriptPos; const msgFmt : String;
      const params : array of const);
begin
   FOptimize := False;
   FCompilerContext.Msgs.AddCompilerErrorFmt(aScriptPos, msgFmt, params);
end;

// EnterComposite
//
procedure TSpecializationContext.EnterComposite(sym : TCompositeTypeSymbol);
begin
   FCompositeStack.Add(FCompositeSymbol);
   FCompositeSymbol := sym;
end;

// LeaveComposite
//
procedure TSpecializationContext.LeaveComposite;
var
   n : Integer;
begin
   n := FCompositeStack.Count-1;
   Assert(n >= 0);
   FCompositeSymbol := TCompositeTypeSymbol(FCompositeStack.List[n]);
   FCompositeStack.Delete(n);
end;

// CompositeSymbol
//
function TSpecializationContext.CompositeSymbol : TCompositeTypeSymbol;
begin
   Result := FCompositeSymbol;
end;

// EnterFunction
//
procedure TSpecializationContext.EnterFunction(funcSym : TFuncSymbol);
begin
   FFuncSymbolStack.Add(FFuncSymbol);
   FFuncSymbol := funcSym;
end;

// LeaveFunction
//
procedure TSpecializationContext.LeaveFunction;
var
   n : Integer;
begin
   n := FFuncSymbolStack.Count-1;
   Assert(n >= 0);
   FFuncSymbol := TFuncSymbol(FFuncSymbolStack.List[n]);
   FFuncSymbolStack.Delete(n);
end;

// FuncSymbol
//
function TSpecializationContext.FuncSymbol : TFuncSymbol;
begin
   Result := FFuncSymbol;
end;

// Name
//
function TSpecializationContext.Name : String;
begin
   Result := FName;
end;

// Parameters
//
function TSpecializationContext.Parameters : TUnSortedSymbolTable;
begin
   Result := FParameters;
end;

// Values
//
function TSpecializationContext.Values : TUnSortedSymbolTable;
begin
   Result := FValues;
end;

// UnitSymbol
//
function TSpecializationContext.UnitSymbol : TSymbol;
begin
   Result := FUnitSymbol;
end;

// Msgs
//
function TSpecializationContext.Msgs : TdwsCompileMessageList;
begin
   Result := CompilerContext.Msgs;
end;

// Optimize
//
function TSpecializationContext.Optimize : Boolean;
begin
   Result := FOptimize;
end;

end.
