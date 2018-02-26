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
unit dwsCompilerContext;

interface

uses
   dwsUtils, dwsSymbols, dwsErrors, dwsScriptSource,
   dwsUnitSymbols, dwsStrings, dwsTokenizer;

type
   TCompilerOption = (
      coOptimize,          // enable compiler optimizations
      coSymbolDictionary,  // fillup symbol dictionary
      coContextMap,        // fillup context map
      coAssertions,        // compile assertions (if absent, ignores assertions)
      coHintsDisabled,     // don't generate hints messages
      coWarningsDisabled,  // don't generate warnings messages
      coExplicitUnitUses,  // unit dependencies must be explicit via a "uses" clause
      coVariablesAsVarOnly,// only variable can be passed as "var" parameters
                           // (for CodeGen that does not support passing record fields or array elements)
      coAllowClosures      // allow closures, ie. capture of local procedures as function pointers
                           // (not suppported yet by script engine, may be supported by CodeGen)
      );
   TCompilerOptions = set of TCompilerOption;

   TdwsCompilerContext = class
      private
         FMsgs : TdwsCompileMessageList;
         FSystemTable : TSystemSymbolTable;
         FProg : TObject;
         FUnifiedConstants : TObject;
         FBaseTypes : TdwsBaseSymbolTypes;
         FOrphanedObjects : TSimpleStack<TRefCountedObject>;
         FUnitList : TIdwsUnitList;
         FHelperMemberNames : TSimpleStringHash;

         FTypDefaultConstructor : TMethodSymbol;
         FTypDefaultDestructor : TMethodSymbol;

         FStringsUnifier : TStringUnifier;

         FExecution : TdwsExecution;
         FOptions : TCompilerOptions;

      protected
         procedure SetSystemTable(const val : TSystemSymbolTable);

      public
         constructor Create;
         destructor Destroy; override;

         procedure OrphanObject(obj : TRefCountedObject);

         function GetTempAddr(DataSize: Integer = -1): Integer;
         function Level : Integer;
         function Table : TSymbolTable;

         function CreateConstExpr(typ : TTypeSymbol; const value : Variant) : TExprBase;

         function WrapWithImplicitCast(toTyp : TTypeSymbol; const scriptPos : TScriptPos; var expr) : Boolean;

         function Optimize : Boolean;

         property StringsUnifier : TStringUnifier read FStringsUnifier;

         property Msgs : TdwsCompileMessageList read FMsgs write FMsgs;
         property SystemTable : TSystemSymbolTable read FSystemTable write SetSystemTable;
         property Prog : TObject read FProg write FProg;
         property UnifiedConstants : TObject read FUnifiedConstants write FUnifiedConstants;
         property UnitList : TIdwsUnitList read FUnitList write FUnitList;
         property HelperMemberNames : TSimpleStringHash read FHelperMemberNames;

         property Execution : TdwsExecution read FExecution write FExecution;
         property Options : TCompilerOptions read FOptions write FOptions;

         property TypBoolean: TBaseBooleanSymbol read FBaseTypes.TypBoolean;
         property TypFloat: TBaseFloatSymbol read FBaseTypes.TypFloat;
         property TypInteger: TBaseIntegerSymbol read FBaseTypes.TypInteger;
         property TypNil: TNilSymbol read FBaseTypes.TypNil;
         property TypObject: TClassSymbol read FBaseTypes.TypObject;
         property TypTObject: TClassSymbol read FBaseTypes.TypTObject;
         property TypString: TBaseStringSymbol read FBaseTypes.TypString;
         property TypVariant: TBaseVariantSymbol read FBaseTypes.TypVariant;
         property TypException: TClassSymbol read FBaseTypes.TypException;
         property TypInterface : TInterfaceSymbol read FBaseTypes.TypInterface;
         property TypAnyType: TAnyTypeSymbol read FBaseTypes.TypAnyType;

         property TypDefaultConstructor : TMethodSymbol read FTypDefaultConstructor;
         property TypDefaultDestructor : TMethodSymbol read FTypDefaultDestructor;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsExprs, dwsUnifiedConstants, dwsConstExprs, dwsOperators, dwsCompilerUtils;

// ------------------
// ------------------ TdwsCompilerContext ------------------
// ------------------

// Create
//
constructor TdwsCompilerContext.Create;
begin
   inherited;
   FOrphanedObjects := TSimpleStack<TRefCountedObject>.Create;
   FStringsUnifier := TStringUnifier.Create;
   FHelperMemberNames := TSimpleStringHash.Create;
end;

// Destroy
//
destructor TdwsCompilerContext.Destroy;
var
   obj : TRefCountedObject;
begin
   // stack behavior is required to allow objects to orphan others while being cleaned up
   while FOrphanedObjects.Count > 0 do begin
      obj := FOrphanedObjects.Peek;
      obj.DecRefCount;
      FOrphanedObjects.Pop;
   end;
   FOrphanedObjects.Free;

   FStringsUnifier.Free;

   FHelperMemberNames.Free;

   inherited;
end;

// OrphanObject
//
procedure TdwsCompilerContext.OrphanObject(obj : TRefCountedObject);
begin
   if obj <> nil then
      FOrphanedObjects.Push(obj);
end;

// GetTempAddr
//
function TdwsCompilerContext.GetTempAddr(DataSize: Integer = -1): Integer;
begin
   Result := (FProg as TdwsProgram).GetTempAddr(DataSize);
end;

// Level
//
function TdwsCompilerContext.Level : Integer;
begin
   Result := (FProg as TdwsProgram).Level;
end;

// Table
//
function TdwsCompilerContext.Table : TSymbolTable;
begin
   Result := (FProg as TdwsProgram).Table;
end;

// CreateConstExpr
//
function TdwsCompilerContext.CreateConstExpr(typ : TTypeSymbol; const value : Variant) : TExprBase;

   function CreateDynamicArrayValue(typ : TTypeSymbol) : TConstExpr;
   var
      val : IScriptDynArray;
   begin
      val := IUnknown(value) as IScriptDynArray;
      if val <> nil then
         Result := TConstExpr.Create(typ, val)
      else Result := TConstExpr.Create(typ, TScriptDynamicArray.CreateNew(typ.Typ) as IScriptDynArray);
   end;

   function CreateAssociativeArrayValue(typ : TAssociativeArraySymbol) : TConstExpr;
   var
      val : IScriptAssociativeArray;
   begin
      val := IUnknown(value) as IScriptAssociativeArray;
      if val<>nil then
         Result:=TConstExpr.Create(typ, val)
      else Result:=TConstExpr.Create(typ, TScriptAssociativeArray.CreateNew(typ.KeyType, typ.Typ) as IScriptAssociativeArray);
   end;

begin
   if typ = TypString then
      Result := TUnifiedConstants(FUnifiedConstants).CreateString(value)
   else if typ.ClassType = TDynamicArraySymbol then
      Result := CreateDynamicArrayValue(typ)
   else if typ.ClassType = TAssociativeArraySymbol then
      Result := CreateAssociativeArrayValue(TAssociativeArraySymbol(typ))
   else if typ = TypBoolean then
      Result := TUnifiedConstants(FUnifiedConstants).CreateBoolean(value)
   else if typ = TypFloat then
      Result := TUnifiedConstants(FUnifiedConstants).CreateFloat(VariantToFloat(value))
   else if typ = TypInteger then
      Result := TUnifiedConstants(FUnifiedConstants).CreateInteger(value)
   else if typ.typ = TypInteger then
      Result := TConstIntExpr.Create(typ, value)
   else Result := TConstExpr.Create(typ, value);
end;

// WrapWithImplicitCast
//
function TdwsCompilerContext.WrapWithImplicitCast(
      toTyp : TTypeSymbol; const scriptPos : TScriptPos; var expr) : Boolean;
var
   casterClass : TTypedExprClass;
   typedExpr : TTypedExpr;
   prog : TdwsProgram;
   opSym : TOperatorSymbol;
   funcExpr : TFuncExprBase;
   posArray : TScriptPosArray;
begin
   typedExpr := TObject(expr) as TTypedExpr;
   if typedExpr.Typ = nil then Exit(False);

   prog := FProg as TdwsProgram;
   opSym := prog.Table.FindImplicitCastOperatorFor(typedExpr.Typ, toTyp);
   if opSym <> nil then begin
      funcExpr := CreateSimpleFuncExpr(Self, scriptPos, opSym.UsesSym);
      funcExpr.AddArg(typedExpr);
      TObject(expr) := funcExpr;
      if opSym.UsesSym.Typ.Size>1 then
         funcExpr.SetResultAddr(prog, nil);
      SetLength(posArray, 1);
      posArray[0] := scriptPos;
      TypeCheckArguments(Self, funcExpr, posArray);
      if Optimize then
         TObject(expr) := funcExpr.OptimizeToTypedExpr(Self, scriptPos);
      Result := True;
   end else begin
      casterClass := (prog.Root.Operators as TOperators).FindImplicitCaster(toTyp, typedExpr.Typ);
      Result := (casterClass <> nil);
      if Result then begin
         if casterClass.InheritsFrom(TUnaryOpExpr) then
            typedExpr := TUnaryOpExprClass(casterClass).Create(Self, scriptPos, typedExpr)
         else begin
            Assert(casterClass.InheritsFrom(TUnaryOpDataExpr));
            typedExpr := TUnaryOpDataExprClass(casterClass).Create(Self, scriptPos, typedExpr);
         end;
         TObject(expr) := typedExpr;
         if Optimize then
            TObject(expr) := typedExpr.OptimizeToTypedExpr(Self, scriptPos);
      end;
   end;
end;

// Optimize
//
function TdwsCompilerContext.Optimize : Boolean;
begin
   Result := (coOptimize in FOptions) and (not FMsgs.HasErrors);
end;

// SetSystemTable
//
procedure TdwsCompilerContext.SetSystemTable(const val : TSystemSymbolTable);
begin
   FSystemTable := val;
   FBaseTypes := FSystemTable.BaseSymbolTypes;

   FTypDefaultConstructor := TypTObject.Members.FindSymbol(SYS_TOBJECT_CREATE, cvPublic) as TMethodSymbol;
   FTypDefaultDestructor := TypTObject.Members.FindSymbol(SYS_TOBJECT_DESTROY, cvPublic) as TMethodSymbol;
end;

end.
