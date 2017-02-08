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
   dwsUtils, dwsSymbols, dwsDataContext, dwsScriptSource, dwsErrors, dwsStrings;

type
   TGenericSymbol = class;

   TGenericConstraint = class (TRefCountedObject)
      public
         function Check(const scriptPos : TScriptPos; valueType : TTypeSymbol;
                        msgs : TdwsCompileMessageList) : Boolean; virtual; abstract;
   end;
   TGenericConstraints = array of TGenericConstraint;

   TGenericTypeSymbol = class (TTypeSymbol)
      public
         procedure InitData(const data : TData; offset : Integer); override;
   end;

   TGenericTypeParameterSymbol = class (TGenericTypeSymbol)
      private
         FConstraints : TTightList;

      protected

      public
         constructor Create(const name : String);
         destructor Destroy; override;

         procedure AddConstraint(aConstraint : TGenericConstraint);

         function CheckConstraints(const scriptPos : TScriptPos; valueType : TTypeSymbol;
                                   msgs : TdwsCompileMessageList) : Boolean;
   end;

   IGenericParameters = interface (IGetSelf)
      function  List : TUnSortedSymbolTable;
      procedure Add(param : TGenericTypeParameterSymbol);
      function  GetParameter(index : Integer) : TGenericTypeParameterSymbol;
      property  Parameters[index : Integer] : TGenericTypeParameterSymbol read GetParameter; default;
      function  Count : Integer;
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

         function Count : Integer;
   end;

   TGenericSymbolSpecialization = record
      Signature : String;
      Specialization : TTypeSymbol;
   end;

   TGenericSymbol = class (TGenericTypeSymbol)
      private
         FParameters : IGenericParameters;
         FGenericType : TTypeSymbol;
         FSpecializations : array of TGenericSymbolSpecialization;

      protected
         class function Signature(params : TUnSortedSymbolTable) : String; static;

         function CreateSpecializationContext(values : TUnSortedSymbolTable) : TSpecializationContext;

      public
         constructor Create(const name : String; const params : IGenericParameters);
         destructor Destroy; override;

         function Specialize(context : TSpecializationContext) : TTypeSymbol; override;

         function SpecializationFor(values : TUnSortedSymbolTable) : TTypeSymbol;

         property Parameters : IGenericParameters read FParameters;
         property GenericType : TTypeSymbol read FGenericType write FGenericType;
   end;

   TGenericConstraintType = class (TGenericConstraint)
      private
         FType : TTypeSymbol;

      public
         constructor Create(aType : TTypeSymbol);

         function Check(const scriptPos : TScriptPos; valueType : TTypeSymbol;
                        msgs : TdwsCompileMessageList) : Boolean; override;
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
   for i := 0 to High(FSpecializations) do begin
      FSpecializations[i].Specialization.Free;
   end;
   FGenericType.Free;
   inherited;
end;

// Specialize
//
function TGenericSymbol.Specialize(context : TSpecializationContext) : TTypeSymbol;
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

   Result := FGenericType.Specialize(context);

   i := Length(FSpecializations);
   SetLength(FSpecializations, i+1);
   FSpecializations[i].Signature := sig;
   FSpecializations[i].Specialization := Result;
end;

// SpecializationFor
//
function TGenericSymbol.SpecializationFor(values : TUnSortedSymbolTable) : TTypeSymbol;
var
   context : TSpecializationContext;
   sig : String;
   i : Integer;
begin
   sig := Signature(values);
   for i := 0 to High(FSpecializations) do begin
      if FSpecializations[i].Signature = sig then begin
         Exit(FSpecializations[i].Specialization);
      end;
   end;

   context := CreateSpecializationContext(values);
   try
      Result := Specialize(context);
   finally
      context.Free;
   end;
end;

// CreateSpecializationContext
//
function TGenericSymbol.CreateSpecializationContext(values : TUnSortedSymbolTable) : TSpecializationContext;
var
   i : Integer;
   n : String;
begin
   n := Name + '<';
   for i := 0 to values.Count-1 do begin
      if i > 0 then
         n := n + ',';
      n := n + values.Symbols[i].Name;
   end;
   n := n + '>';
   Result := TSpecializationContext.Create(n, Parameters.List, values);
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
function TGenericTypeParameterSymbol.CheckConstraints(
      const scriptPos : TScriptPos; valueType : TTypeSymbol; msgs : TdwsCompileMessageList) : Boolean;
var
   i : Integer;
begin
   Result := True;
   for i := 0 to FConstraints.Count-1 do
      if not TGenericConstraint(FConstraints.List[i]).Check(scriptPos, valueType, msgs) then
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

// Count
//
function TGenericParameters.Count : Integer;
begin
   Result := FList.Count;
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

// ------------------
// ------------------ TGenericConstraintType ------------------
// ------------------

// Create
//
constructor TGenericConstraintType.Create(aType : TTypeSymbol);
begin
   inherited Create;
   FType := aType;
end;

// Check
//
function TGenericConstraintType.Check(
      const scriptPos : TScriptPos; valueType : TTypeSymbol;
      msgs : TdwsCompileMessageList) : Boolean;
begin
   if not valueType.IsOfType(FType) then begin
      msgs.AddCompilerErrorFmt(scriptPos, CPE_IncompatibleParameterTypes,
                               [FType.Caption, valueType.Caption]);
      Result := False;
   end else Result := True;
end;

end.
