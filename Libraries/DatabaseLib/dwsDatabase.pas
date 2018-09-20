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
unit dwsDatabase;

interface

uses
   SysUtils,
   dwsSymbols, dwsUtils, dwsExprs, dwsStack, dwsXPlatform, dwsDataContext;

// Simple database abstraction interfaces and optional base classes for DWS
// exposes transaction & forward-only cursor, which are all one really needs :p

type

   TdwsDataFieldType = (
      dftUnknown,
      dftNull,
      dftInteger,
      dftFloat,
      dftString,
      dftBoolean,
      dftDateTime,
      dftBlob
      );

   TdwsDataSet = class;
   IdwsDataSet = interface;
   IdwsDataField = interface;

   IdwsDataBase = interface
      ['{66B9A9E4-87B4-4181-A357-50ECC9BDA3F0}']
      procedure BeginTransaction;
      procedure Commit;
      procedure Rollback;
      function InTransaction : Boolean;
      // if can't should return string with descriptive reason
      function CanReleaseToPool : String;

      procedure Exec(const sql : String; const parameters : IDataContext; context : TExprBase);
      function Query(const sql : String; const parameters : IDataContext; context : TExprBase) : IdwsDataSet;

      function VersionInfoText : String;

      function OptionList : TStringDynArray;
      function GetOption(const name : String) : String;
      procedure SetOption(const name, value : String);
      property Options[const name : String] : String read GetOption write SetOption;
   end;

   IdwsDataSet = interface
      ['{8C1F4B26-C7C7-45A7-9699-4092F8DEA66F}']
      function Eof : Boolean;
      procedure Next;

      function GetField(index : Integer) : IdwsDataField;
      property Fields[index : Integer] : IdwsDataField read GetField;
      function FieldCount : Integer;

      function GetIsNullField(index : Integer) : Boolean;
      procedure GetStringField(index : Integer; var result : String);
      function GetIntegerField(index : Integer) : Int64;
      function GetFloatField(index : Integer) : Double;
      function GetBooleanField(index : Integer) : Boolean;
      function GetBlobField(index : Integer) : RawByteString;
   end;

   IdwsDataField = interface
      ['{1376FC38-6BDB-4E24-99A0-7987C02B2E23}']
      function Name : String;
      function DataType : TdwsDataFieldType;
      function DeclaredType : String;

      function IsNull : Boolean;
      procedure GetAsString(var Result : String);
      function AsInteger : Int64;
      function AsFloat : Double;
      function AsBoolean : Boolean;
      function AsBlob : RawByteString;
   end;

   IdwsBlob = interface
      ['{018C9441-3177-49E1-97EF-EA5F2584FA60}']
   end;

   IdwsDataBaseFactory = interface
      ['{0DB5DAED-FAAF-4ED3-A157-3914A5260607}']
      function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
   end;

   TdwsDataBaseFactory = class (TInterfacedSelfObject, IdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; virtual; abstract;
   end;

   TdwsDataBaseApplyPathVariablesEvent = function (const path : String) : String of object;

   TdwsDataBase = class (TInterfacedSelfObject)
      private
         class var vOnApplyPathVariables : TdwsDataBaseApplyPathVariablesEvent;

      public
         class property OnApplyPathVariables : TdwsDataBaseApplyPathVariablesEvent read vOnApplyPathVariables write vOnApplyPathVariables;
         class function ApplyPathVariables(const path : String) : String; static;

         class procedure RegisterDriver(const driverName : String; const factory : IdwsDataBaseFactory); static;
         class function CreateDataBase(const driverName : String; const parameters : TStringDynArray) : IdwsDataBase; static;

         function OptionList : TStringDynArray; virtual;
         function GetOption(const name : String) : String; virtual;
         procedure SetOption(const name, value : String); virtual;
   end;

   TdwsDataSet = class (TInterfacedSelfObject, IUnknown, IdwsDataSet)
      private
         FDataBase : IdwsDataBase;
         FFieldCount : Integer;

      protected
         FFields : array of IdwsDataField;

         procedure PrepareFields;
         procedure DoPrepareFields; virtual; abstract;

         function _Release : Integer; stdcall;

         property DataBase : IdwsDataBase read FDataBase;

         class procedure RaiseInvalidFieldIndex(index : Integer); static;

      public
         constructor Create(const db : IdwsDataBase);
         destructor Destroy; override;

         function Eof : Boolean; virtual; abstract;
         procedure Next; virtual; abstract;

         function GetField(index : Integer) : IdwsDataField;
         function FieldCount : Integer; virtual;

         function GetIsNullField(index : Integer) : Boolean;
         procedure GetStringField(index : Integer; var result : String);
         function GetIntegerField(index : Integer) : Int64;
         function GetFloatField(index : Integer) : Double;
         function GetBooleanField(index : Integer) : Boolean;
         function GetBlobField(index : Integer) : RawByteString;
   end;

   TdwsDataField = class (TInterfacedSelfObject, IdwsDataField)
      private
         FDataSet : IdwsDataSet;
         FIndex : Integer;
         FName : String;
         FDataType : TdwsDataFieldType;
         FDeclaredType : String;

      protected
         function GetName : String; virtual; abstract;
         function GetDataType : TdwsDataFieldType; virtual; abstract;
         function GetDeclaredType : String; virtual; abstract;

         procedure RaiseNoActiveRecord;

      public
         constructor Create(const dataSet : IdwsDataSet; fieldIndex : Integer);

         property DataSet : IdwsDataSet read FDataSet;
         property Index : Integer read FIndex;

         function Name : String;
         function DataType : TdwsDataFieldType; virtual;
         function DeclaredType : String;

         procedure GetAsString(var Result : String);

         function IsNull : Boolean; virtual; abstract;
         procedure AsString(var Result : String); virtual; abstract;
         function AsInteger : Int64; virtual; abstract;
         function AsFloat : Double; virtual; abstract;
         function AsBoolean : Boolean; virtual;
         function AsBlob : RawByteString; virtual; abstract;
   end;

   EDWSDataBase = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TRegisteredDriver = record
      Name : String;
      Factory : IdwsDataBaseFactory;
   end;

var
   vDrivers : array of TRegisteredDriver;

// RegisterDriver
//
class procedure TdwsDatabase.RegisterDriver(const driverName : String; const factory : IdwsDataBaseFactory);
var
   n : Integer;
begin
   n:=Length(vDrivers);
   SetLength(vDrivers, n+1);
   vDrivers[n].Name:=driverName;
   vDrivers[n].Factory:=factory;
end;

// CreateDataBase
//
class function TdwsDatabase.CreateDataBase(const driverName : String; const parameters : TStringDynArray) : IdwsDataBase;
var
   i : Integer;
begin
   for i:=0 to High(vDrivers) do
      if UnicodeSameText(vDrivers[i].Name, driverName) then
         Exit(vDrivers[i].Factory.CreateDataBase(parameters));
   raise EDWSDataBase.CreateFmt('No driver of name "%s"', [driverName]);
end;

// ApplyPathVariables
//
class function TdwsDataBase.ApplyPathVariables(const path : String) : String;
begin
   if Assigned(vOnApplyPathVariables) then
      Result:=vOnApplyPathVariables(path)
   else Result:=path;
end;

// OptionList
//
function TdwsDataBase.OptionList : TStringDynArray;
begin
   Result := nil;
end;

// GetOption
//
function TdwsDataBase.GetOption(const name : String) : String;
begin
   raise EDWSDataBase.CreateFmt('Option "%s" cannot be read or does not exist', [ name ]);
end;

// SetOption
//
procedure TdwsDataBase.SetOption(const name, value : String);
begin
   raise EDWSDataBase.CreateFmt('Option "%s" cannot be written or does not exist', [ name ]);
end;

// ------------------
// ------------------ TdwsDataSet ------------------
// ------------------

// Create
//
constructor TdwsDataSet.Create(const db : IdwsDataBase);
begin
   inherited Create;
   FDataBase := db;
   FFieldCount := -1;
end;

// Destroy
//
destructor TdwsDataSet.Destroy;
begin
   SetLength(FFields, 0);
   FFieldCount := -1;
   inherited;
end;

// RaiseInvalidFieldIndex
//
class procedure TdwsDataSet.RaiseInvalidFieldIndex(index : Integer);
begin
   raise Exception.CreateFmt('Invalid field index %d', [index]);
end;

// GetField
//
function TdwsDataSet.GetField(index : Integer) : IdwsDataField;
begin
   if FFieldCount < 0 then
      PrepareFields;
   if Cardinal(index) < Cardinal(FFieldCount) then
      Result := FFields[index]
   else RaiseInvalidFieldIndex(index);
end;

// FieldCount
//
function TdwsDataSet.FieldCount : Integer;
begin
   if FFieldCount < 0 then
      PrepareFields;
   Result := FFieldCount;
end;

// GetIsNullField
//
function TdwsDataSet.GetIsNullField(index : Integer) : Boolean;
begin
   Result := GetField(index).IsNull;
end;

// GetStringField
//
procedure TdwsDataSet.GetStringField(index : Integer; var result : String);
begin
   if FFieldCount < 0 then
      PrepareFields;
   if Cardinal(index) < Cardinal(FFieldCount) then
      FFields[index].GetAsString(result)
   else RaiseInvalidFieldIndex(index);
end;

// GetIntegerField
//
function TdwsDataSet.GetIntegerField(index : Integer) : Int64;
begin
   Result := GetField(index).AsInteger;
end;

// GetFloatField
//
function TdwsDataSet.GetFloatField(index : Integer) : Double;
begin
   Result := GetField(index).AsFloat;
end;

// GetBooleanField
//
function TdwsDataSet.GetBooleanField(index : Integer) : Boolean;
begin
   Result := GetField(index).AsBoolean;
end;

// GetBlobField
//
function TdwsDataSet.GetBlobField(index : Integer) : RawByteString;
begin
   Result := GetField(index).AsBlob;
end;

// PrepareFields
//
procedure TdwsDataSet.PrepareFields;
begin
   if FFieldCount < 0 then begin
      DoPrepareFields;
      FFieldCount := Length(FFields);
   end;
end;

// _Release
//
function TdwsDataSet._Release : Integer;
begin
   Result := DecRefCount;
   if Result = 0 then
      Destroy
   else if Result=Length(FFields) then begin
      SetLength(FFields, 0);
      FFieldCount := -1;
   end;
end;

// ------------------
// ------------------ TdwsDataField ------------------
// ------------------

// Create
//
constructor TdwsDataField.Create(const dataSet : IdwsDataSet; fieldIndex : Integer);
begin
   FDataSet:=dataSet;
   FIndex:=fieldIndex;
end;

// Name
//
function TdwsDataField.Name : String;
begin
   if FName='' then
      FName:=GetName;
   Result:=FName;
end;

// DataType
//
function TdwsDataField.DataType : TdwsDataFieldType;
begin
   if FDataType=dftUnknown then
      FDataType:=GetDataType;
   Result:=FDataType;
end;

// DeclaredType
//
function TdwsDataField.DeclaredType : String;
begin
   if FDeclaredType='' then
      FDeclaredType:=GetDeclaredType;
   Result:=FDeclaredType;
end;

// GetAsString
//
procedure TdwsDataField.GetAsString(var Result : String);
begin
   AsString(Result);
end;

// AsBoolean
//
function TdwsDataField.AsBoolean : Boolean;
begin
   Result:=(AsInteger<>0);
end;

// RaiseNoActiveRecord
//
procedure TdwsDataField.RaiseNoActiveRecord;
begin
   raise EDWSDataBase.Create('No active record');
end;

end.
