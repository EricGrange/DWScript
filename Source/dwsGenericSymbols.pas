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
   dwsSpecializationContext, dwsCompilerContext, dwsTokenizer, dwsOperators,
   dwsCompilerUtils, dwsExprs;

type
   TGenericSymbol = class;

   TGenericConstraint = class (TRefCountedObject)
      public
         function Check(context : TSpecializationContext) : Boolean; virtual; abstract;
   end;
   TGenericConstraints = array of TGenericConstraint;

   TGenericConstraintBinaryOp = class;

   TGenericTypeSymbol = class (TTypeSymbol)
      public
         procedure InitData(const data : TData; offset : Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function IsGeneric : Boolean; override;
   end;

   TGenericTypeParameterSymbol = class (TGenericTypeSymbol)
      private
         FConstraints : TTightList;  // owned

      protected

      public
         constructor Create(const name : String);
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

         function GetCaption : UnicodeString; override;

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

         property GenericType : TTypeSymbol read FGenericType write FGenericType;
         property Parameters : IGenericParameters read FParameters;
   end;

   TGenericConstraintType = class (TGenericConstraint)
      private
         FParameter : TGenericTypeParameterSymbol;
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
begin
   inherited Create(name, nil);
   FParameters := params;
end;

// Destroy
//
destructor TGenericSymbol.Destroy;
var
   i : Integer;
begin
   for i := 0 to High(FSpecializations) do
      FSpecializations[i].Specialization.Free;
   FGenericType.Free;
   FBinaryOps.Clean;
   FInternalTypes.Free;
   inherited;
end;

// SpecializeType
//
function TGenericSymbol.SpecializeType(const context : ISpecializationContext) : TTypeSymbol;
var
   sig : String;
   i : Integer;
begin
   sig := Signature(context.Values);
   for i := 0 to High(FSpecializations) do begin
      if FSpecializations[i].Signature = sig then begin
         Result := FSpecializations[i].Specialization;
         Exit(Result);
      end;
   end;

   if FGenericType <> nil then
      Result := FGenericType.SpecializeType(context)
   else Result := nil;

   i := Length(FSpecializations);
   SetLength(FSpecializations, i+1);
   FSpecializations[i].Signature := sig;
   FSpecializations[i].Specialization := Result;
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
   i : Integer;
begin
   sig := Signature(values);
   for i := 0 to High(FSpecializations) do begin
      if FSpecializations[i].Signature = sig then begin
         Exit(FSpecializations[i].Specialization);
      end;
   end;

   n := Name + '<';
   for i := 0 to values.Count-1 do begin
      if i > 0 then
         n := n + ',';
      n := n + values.Symbols[i].Name;
   end;
   n := n + '>';

   context := TSpecializationContext.Create(
      n, Parameters.List, values, aScriptPos, aUnit, valuesPos,
      aContext, aOperators,
      (coOptimize in aContext.Options) and not aContext.Msgs.HasErrors
   );
   try
      for i := 0 to FParameters.Count-1 do
         FParameters.Parameters[i].CheckConstraints(context);
      Result := SpecializeType(context);
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
   resultType := TGenericTypeParameterSymbol.Create(aLeft.Caption + ' ' + cTokenStrings[anOp] + ' ' + aRight.Caption);
   Result.ResultType := resultType;
   resultType.AddConstraint(Result);
   Result.IncRefCount;
   FBinaryOps.Add(Result);
   if FInternalTypes = nil then
      FInternalTypes := TSymbolTable.Create;
   FInternalTypes.AddSymbolDirect(resultType);
end;

// GetCaption
//
function TGenericSymbol.GetCaption : UnicodeString;
begin
   Result := Name + FParameters.Caption;
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
      Result := Result + sym.Name + ':' + IntToStr(NativeUInt(sym)) + ';';
   end;
end;

// ------------------
// ------------------ TGenericTypeParameterSymbol ------------------
// ------------------

// Create
//
constructor TGenericTypeParameterSymbol.Create(const name : String);
begin
   inherited Create(name, nil);
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

// IsCompatible
//
function TGenericTypeSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result := True;
end;

// IsGeneric
//
function TGenericTypeSymbol.IsGeneric : Boolean;
begin
   Result := True;
end;

// ------------------
// ------------------ TGenericConstraintType ------------------
// ------------------

// Create
//
constructor TGenericConstraintType.Create(aParameter : TGenericTypeParameterSymbol; aConstraintType : TTypeSymbol);
begin
   inherited Create;
   FParameter := aParameter;
   FConstraintType := aConstraintType;
end;

// Check
//
function TGenericConstraintType.Check(context : TSpecializationContext) : Boolean;
var
   specialized : TTypeSymbol;
   scriptPos : TScriptPos;
begin
   specialized := context.SpecializeType(FParameter);
   if not specialized.IsOfType(FConstraintType) then begin
      scriptPos := context.ParameterValuePos(FParameter);
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
