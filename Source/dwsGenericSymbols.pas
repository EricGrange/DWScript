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
unit dwsGenericSymbols;

interface

uses
   SysUtils,
   dwsXPlatform, dwsUtils,
   dwsSymbols, dwsDataContext, dwsScriptSource, dwsErrors, dwsStrings,
   dwsSpecializationContext, dwsCompilerContext, dwsTokenTypes, dwsOperators,
   dwsCompilerUtils, dwsExprs, dwsSpecializationMap;

type
   TGenericSymbol = class;

   TGenericConstraint = class (TRefCountedObject)
      public
         function Check(context : TSpecializationContext) : Boolean; virtual; abstract;
   end;
   TGenericConstraints = array of TGenericConstraint;

   TGenericConstraintBinaryOp = class;

   TGenericTypeSymbol = class (TTypeSymbol)
      protected
         FGenericSymbol : TGenericSymbol;

         function GetIsGeneric : Boolean; override;

      public
         procedure InitData(const data : TData; offset : Integer); override;
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;

         property GenericSymbol : TGenericSymbol read FGenericSymbol;
   end;

   TGenericTypeParameterSymbol = class (TGenericTypeSymbol)
      private
         FConstraints : TTightList;  // owned

      public
         constructor Create(aGeneric : TGenericSymbol; const name : String);
         destructor Destroy; override;

         procedure AddConstraint(aConstraint : TGenericConstraint);

         function CheckConstraints(context : TSpecializationContext) : Boolean;
   end;

   IGenericParameters = interface (IGetSelf)
      function  List : TUnSortedSymbolTable;
      procedure Add(param : TGenericTypeParameterSymbol);
      function  GetParameter(index : Integer) : TGenericTypeParameterSymbol;
      property  Parameters[index : Integer] : TGenericTypeParameterSymbol read GetParameter; default;
      function  Count : Integer;
      function  Find(const name : String) : TGenericTypeParameterSymbol;
      function  Caption : String;
   end;

   TGenericParameters = class (TInterfacedObject, IGenericParameters)
      private
         FList : TUnSortedSymbolTable;

      protected
         function GetSelf : TObject;

         function ToUnicodeString : String;

      public
         constructor Create;
         destructor Destroy; override;

         function  List : TUnSortedSymbolTable;

         procedure Add(param : TGenericTypeParameterSymbol);
         function  GetParameter(index : Integer) : TGenericTypeParameterSymbol;
         function  Find(const name : String) : TGenericTypeParameterSymbol;

         function Count : Integer;

         function  Caption : String;
   end;

   TGenericSymbolSpecialization = record
      Signature : String;
      Specialization : TTypeSymbol;
      SpecializedObjects : TSpecializationMap;
   end;

   TGenericSymbol = class sealed (TGenericTypeSymbol)
      private
         FParameters : IGenericParameters;
         FGenericType : TTypeSymbol;
         FSpecializations : array of TGenericSymbolSpecialization;
         FInternalTypes : TSymbolTable;
         FBinaryOps : TTightList;

      protected
         class function Signature(params : TUnSortedSymbolTable) : String; static;

         function GetCaption : String; override;
         procedure AcquireSymbol(sym : TSymbol);

      public
         constructor Create(const name : String; const params : IGenericParameters);
         destructor Destroy; override;

         function SpecializeType(const context : ISpecializationContext) : TTypeSymbol; override;

         function SpecializationFor(const aScriptPos : TScriptPos; aUnit : TSymbol;
                                    values : TUnSortedSymbolTable;
                                    const valuesPos : TScriptPosArray;
                                    aContext : TdwsCompilerContext;
                                    const aOperators : TOperators) : TTypeSymbol;

         function ConstraintForBinaryOp(anOp : TTokenType; aLeft, aRight : TTypeSymbol) : TGenericConstraintBinaryOp;

         class function GenericFor(typ1, typ2 : TTypeSymbol) : TGenericSymbol; static;

         property GenericType : TTypeSymbol read FGenericType write FGenericType;
         property Parameters : IGenericParameters read FParameters;
   end;

   TGenericParameterConstraint = class (TGenericConstraint)
      private
         FParameter : TGenericTypeParameterSymbol;

      public
         constructor Create(aParameter : TGenericTypeParameterSymbol);

         property Parameter : TGenericTypeParameterSymbol read FParameter;
   end;

   TGenericConstraintRecord = class (TGenericParameterConstraint)
      public
         function Check(context : TSpecializationContext) : Boolean; override;
   end;

   TGenericConstraintType = class (TGenericParameterConstraint)
      private
         FConstraintType : TTypeSymbol;

      public
         constructor Create(aParameter : TGenericTypeParameterSymbol; aConstraintType : TTypeSymbol);

         function Check(context : TSpecializationContext) : Boolean; override;
   end;

   TGenericConstraintBinaryOp = class (TGenericConstraint)
      private
         FOp : TTokenType;
         FLeftType : TTypeSymbol;
         FRightType : TTypeSymbol;
         FResultType : TTypeSymbol;

      public
         constructor Create(anOp : TTokenType; aLeft, aRight : TTypeSymbol);

         function Check(context : TSpecializationContext) : Boolean; override;

         property Op : TTokenType read FOp;
         property LeftType : TTypeSymbol read FLeftType;
         property RightType : TTypeSymbol read FRightType;
         property ResultType : TTypeSymbol read FResultType write FResultType;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGenericSymbol ------------------
// ------------------

// Create
//
constructor TGenericSymbol.Create(const name : String; const params : IGenericParameters);
var
   i : Integer;
begin
   inherited Create(name, nil);
   FParameters := params;
   for i := 0 to params.Count-1 do
      params[i].FGenericSymbol := Self;
end;

// Destroy
//
destructor TGenericSymbol.Destroy;
var
   i : Integer;
begin
   for i := 0 to High(FSpecializations) do begin
      FSpecializations[i].Specialization.Free;
      FSpecializations[i].SpecializedObjects.Free;
   end;
   FGenericType.Free;
   FBinaryOps.Clean;
   FInternalTypes.Free;
   inherited;
end;

// SpecializeType
//
function TGenericSymbol.SpecializeType(const context : ISpecializationContext) : TTypeSymbol;
begin
   context.AddCompilerError('TGenericSymbol.SpecializeType should never be invoked...');
   Result := nil;
end;

// SpecializationFor
//
function TGenericSymbol.SpecializationFor(const aScriptPos : TScriptPos; aUnit : TSymbol;
                                          values : TUnSortedSymbolTable;
                                          const valuesPos : TScriptPosArray;
                                          aContext : TdwsCompilerContext;
                                          const aOperators : TOperators) : TTypeSymbol;
var
   context : TSpecializationContext;
   sig, n : String;
   i, k : Integer;
begin
   sig := Signature(values);
   for i := 0 to High(FSpecializations) do
      if FSpecializations[i].Signature = sig then
         Exit(FSpecializations[i].Specialization);

   n := Name + '<';
   for i := 0 to values.Count-1 do begin
      if i > 0 then
         n := n + ',';
      n := n + values.Symbols[i].Name;
   end;
   n := n + '>';

   // TODO support self-references

   k := Length(FSpecializations);
   SetLength(FSpecializations, k+1);
   FSpecializations[k].Signature := sig;
   FSpecializations[k].Specialization := nil;
   FSpecializations[k].SpecializedObjects := TSpecializationMap.Create;

   context := TSpecializationContext.Create(
      n, Parameters.List, values, aScriptPos, aUnit, valuesPos,
      aContext, aOperators,
      (coOptimize in aContext.Options) and not aContext.Msgs.HasErrors,
      FSpecializations[k].SpecializedObjects
   );
   try
      context.AcquireSymbol := AcquireSymbol;
      for i := 0 to FParameters.Count-1 do
         FParameters.Parameters[i].CheckConstraints(context);

      if FGenericType <> nil then begin
         Result := FGenericType.SpecializeType(context);
         if Result <> nil then begin
            Result.SetName(n, True);
            if FInternalTypes <> nil then
               FInternalTypes.Remove(Result);
            FSpecializations[k].Specialization := Result;
         end;
      end else Result := nil;
   finally
      context.Free;
   end;
end;

// ConstraintForBinaryOp
//
function TGenericSymbol.ConstraintForBinaryOp(anOp : TTokenType; aLeft, aRight : TTypeSymbol) : TGenericConstraintBinaryOp;
var
   i : Integer;
   resultType : TGenericTypeParameterSymbol;
begin
   for i := 0 to FBinaryOps.Count-1 do begin
      Result := TGenericConstraintBinaryOp(FBinaryOps.List[i]);
      if (Result.Op = anOp) and (Result.LeftType = aLeft) and (Result.RightType = aRight) then
         Exit;
   end;
   Result := TGenericConstraintBinaryOp.Create(anOp, aLeft, aRight);
   resultType := TGenericTypeParameterSymbol.Create(Self, aLeft.Caption + ' ' + cTokenStrings[anOp] + ' ' + aRight.Caption);
   Result.ResultType := resultType;
   resultType.AddConstraint(Result);
   Result.IncRefCount;
   FBinaryOps.Add(Result);
   AcquireSymbol(resultType);
end;

// GenericFor
//
class function TGenericSymbol.GenericFor(typ1, typ2 : TTypeSymbol) : TGenericSymbol;
var
   ts1, ts2 : TGenericTypeSymbol;
begin
   typ1 := typ1.UnAliasedType;
   typ2 := typ2.UnAliasedType;
   if typ1 is TGenericTypeSymbol then begin
      ts1 := TGenericTypeSymbol(typ1);
      if typ2 is TGenericTypeSymbol then begin
         ts2 := TGenericTypeSymbol(typ2);
         // both are generic
         if ts1.GenericSymbol = ts2.GenericSymbol then
            Result := ts1.GenericSymbol
         else begin
            Assert(False, 'not supported yet');
            Result := nil;
         end;
      end else Result := ts1.GenericSymbol;
   end else if typ2 is TGenericTypeSymbol then
      Result := TGenericTypeSymbol(typ2).GenericSymbol
   else Result := nil;
end;

// GetCaption
//
function TGenericSymbol.GetCaption : String;
begin
   Result := Name + FParameters.Caption;
end;

// AcquireSymbol
//
procedure TGenericSymbol.AcquireSymbol(sym : TSymbol);
begin
   if FInternalTypes = nil then
      FInternalTypes := TSymbolTable.Create;
   FInternalTypes.AddSymbolDirect(sym);
end;

// Signature
//
class function TGenericSymbol.Signature(params : TUnSortedSymbolTable) : String;
var
   i : Integer;
   sym : TSymbol;
begin
   for i := 0 to params.Count-1 do begin
      sym := params.Symbols[i];
      Result := Result + sym.Name + ':' + FastInt64ToStr(NativeUInt(sym)) + ';';
   end;
end;

// ------------------
// ------------------ TGenericTypeParameterSymbol ------------------
// ------------------

// Create
//
constructor TGenericTypeParameterSymbol.Create(aGeneric : TGenericSymbol; const name : String);
begin
   inherited Create(name, nil);
   FGenericSymbol := aGeneric;
   FSize := 1; // fake size
end;

// Destroy
//
destructor TGenericTypeParameterSymbol.Destroy;
begin
   FConstraints.Clean;
   inherited;
end;

// AddConstraint
//
procedure TGenericTypeParameterSymbol.AddConstraint(aConstraint : TGenericConstraint);
begin
   FConstraints.Add(aConstraint);
end;

// CheckConstraints
//
function TGenericTypeParameterSymbol.CheckConstraints(context : TSpecializationContext) : Boolean;
var
   i : Integer;
begin
   Result := True;
   for i := 0 to FConstraints.Count-1 do
      if not TGenericConstraint(FConstraints.List[i]).Check(context) then
         Result := False;
end;

// ------------------
// ------------------ TGenericParameters ------------------
// ------------------

// Create
//
constructor TGenericParameters.Create;
begin
   FList := TUnSortedSymbolTable.Create;
end;

// Destroy
//
destructor TGenericParameters.Destroy;
begin
   FList.Free;
   inherited;
end;

// List
//
function TGenericParameters.List : TUnSortedSymbolTable;
begin
   Result := FList;
end;

// GetSelf
//
function TGenericParameters.GetSelf : TObject;
begin
   Result := Self;
end;

// ToUnicodeString
//
function TGenericParameters.ToUnicodeString : String;
begin
   Result := String(ClassName);
end;

// Add
//
procedure TGenericParameters.Add(param : TGenericTypeParameterSymbol);
begin
   FList.AddSymbol(param);
end;

// GetParameter
//
function TGenericParameters.GetParameter(index : Integer) : TGenericTypeParameterSymbol;
begin
   Result := TGenericTypeParameterSymbol(FList.Symbols[index]);
end;

// Find
//
function TGenericParameters.Find(const name : String) : TGenericTypeParameterSymbol;
begin
   Result := TGenericTypeParameterSymbol(FList.FindLocal(name));
end;

// Count
//
function TGenericParameters.Count : Integer;
begin
   Result := FList.Count;
end;

// Caption
//
function TGenericParameters.Caption : String;
var
   i : Integer;
   p : TGenericTypeParameterSymbol;
begin
   Result := '<';
   for i := 0 to FList.Count-1 do begin
      if i > 0 then
         Result := Result + ',';
      p := TGenericTypeParameterSymbol(FList.Symbols[i]);
      if p.Name = '' then
         Result := Result + p.Caption
      else Result := Result + p.Name;
   end;
   Result := Result + '>';
end;

// ------------------
// ------------------ TGenericTypeSymbol ------------------
// ------------------

// InitData
//
procedure TGenericTypeSymbol.InitData(const data : TData; offset : Integer);
begin
   // nothing
end;

// DoIsOfType
//
function TGenericTypeSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result := (typSym = Self); // TODO check vs constraints
end;

// IsCompatible
//
function TGenericTypeSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result := (typSym = Self); // TODO check vs constraints
end;

// GetIsGeneric
//
function TGenericTypeSymbol.GetIsGeneric : Boolean;
begin
   Result := True;
end;

// ------------------
// ------------------ TGenericParameterConstraint ------------------
// ------------------

// Create
//
constructor TGenericParameterConstraint.Create(aParameter : TGenericTypeParameterSymbol);
begin
   inherited Create;
   FParameter := aParameter;
end;

// ------------------
// ------------------ TGenericConstraintRecord ------------------
// ------------------

// Check
//
function TGenericConstraintRecord.Check(context : TSpecializationContext) : Boolean;
var
   specialized : TTypeSymbol;
   scriptPos : TScriptPos;
begin
   specialized := context.SpecializeType(Parameter);
   if not specialized.UnAliasedTypeIs(TRecordSymbol) then begin
      scriptPos := context.ParameterValuePos(Parameter);
      context.AddCompilerErrorFmt(scriptPos, CPE_IncompatibleParameterTypes,
                                  ['record', specialized.Caption]);
      Result := False;
   end else Result := True;
end;

// ------------------
// ------------------ TGenericConstraintType ------------------
// ------------------

// Create
//
constructor TGenericConstraintType.Create(aParameter : TGenericTypeParameterSymbol; aConstraintType : TTypeSymbol);
begin
   inherited Create(aParameter);
   FConstraintType := aConstraintType;
end;

// Check
//
function TGenericConstraintType.Check(context : TSpecializationContext) : Boolean;
var
   specialized : TTypeSymbol;
   scriptPos : TScriptPos;
begin
   specialized := context.SpecializeType(Parameter);
   if not specialized.IsOfType(FConstraintType) then begin
      scriptPos := context.ParameterValuePos(Parameter);
      context.AddCompilerErrorFmt(scriptPos, CPE_IncompatibleParameterTypes,
                                  [FConstraintType.Caption, specialized.Caption]);
      Result := False;
   end else Result := True;
end;

// ------------------
// ------------------ TGenericConstraintBinaryOp ------------------
// ------------------

// Create
//
constructor TGenericConstraintBinaryOp.Create(anOp : TTokenType; aLeft, aRight : TTypeSymbol);
begin
   inherited Create;
   FOp := anOp;
   FLeftType := aLeft;
   FRightType := aRight;
end;

// Check
//
function TGenericConstraintBinaryOp.Check(context : TSpecializationContext) : Boolean;
begin
   // nothing yet TODO
   Result := True;
end;

end.
