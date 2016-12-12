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

uses dwsUtils, dwsSymbols, dwsErrors, dwsUnitSymbols, dwsStrings;

type

   TdwsCompilerContext = class
      private
         FMsgs : TdwsCompileMessageList;
         FSystemTable : TSystemSymbolTable;
         FProg : TObject;
         FUnifiedConstants : TObject;
         FBaseTypes : TdwsBaseSymbolTypes;
         FOrphanedObjects : TSimpleStack<TRefCountedObject>;

         FTypDefaultConstructor : TMethodSymbol;
         FTypDefaultDestructor : TMethodSymbol;

         FStringsUnifier : TStringUnifier;

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

         property StringsUnifier : TStringUnifier read FStringsUnifier;

         property Msgs : TdwsCompileMessageList read FMsgs write FMsgs;
         property SystemTable : TSystemSymbolTable read FSystemTable write SetSystemTable;
         property Prog : TObject read FProg write FProg;
         property UnifiedConstants : TObject read FUnifiedConstants write FUnifiedConstants;

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

uses dwsExprs, dwsUnifiedConstants, dwsConstExprs;

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
