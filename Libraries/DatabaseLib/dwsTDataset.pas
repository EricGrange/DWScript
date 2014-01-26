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
{    Current maintainer: Mason Wheeler                                 }
{                                                                      }
{**********************************************************************}
unit dwsTDataset;

interface

uses
   DB,
   dwsDatabase;

type
   TdwsTDataset = class(TInterfacedObject, IdwsDataset)
   private
      FDataset: TDataset;
      FFields: TArray<IdwsDataField>;
      FOwner: boolean;
   private //IdwsDataset implementation
      function Eof : Boolean;
      procedure Next;

      function GetField(index : Integer) : IdwsDataField;
      function FieldCount : Integer;
   public
      constructor Create(ds: TDataset; owner: boolean = true);
      destructor Destroy; override;
   end;

   TdwsTField = class(TInterfacedObject, IdwsDataField)
   private
      FField: TField;
      FDataType: TdwsDataFieldType;
   private //IdwsDataField implementation
      function Name : String;
      function DataType : TdwsDataFieldType;
      function DeclaredType : String;

      function IsNull : Boolean;
      function AsString : String;
      function AsInteger : Int64;
      function AsFloat : Double;
      function AsBoolean : Boolean;
      function AsBlob : RawByteString;
   public
      constructor Create(field: TField);
   end;

implementation
uses
   SysUtils;

{ TdwsTDataset }

constructor TdwsTDataset.Create(ds: TDataset; owner: boolean);
var
  i: Integer;
begin
   inherited Create;
   FDataset := ds;
   SetLength(FFields, ds.FieldCount);
   for i := 0 to high(FFields) do
      FFields[i] := TdwsTField.Create(ds.Fields[i]);
   FOwner := owner;
end;

destructor TdwsTDataset.Destroy;
begin
   if FOwner then
      FDataset.Free;
   inherited Destroy;
end;

function TdwsTDataset.Eof: Boolean;
begin
   result := FDataset.Eof;
end;

function TdwsTDataset.FieldCount: Integer;
begin
   result := length(FFields);
end;

function TdwsTDataset.GetField(index: Integer): IdwsDataField;
begin
   result := FFields[index];
end;

procedure TdwsTDataset.Next;
begin
   FDataset.Next;
end;

{ TdwsTField }

constructor TdwsTField.Create(field: TField);
begin
   FField := field;
   case FField.DataType of
      ftSmallint..ftWord, ftAutoInc, ftLargeint, ftLongWord, ftShortint, ftByte: FDataType := dftInteger;
      ftBoolean: FDataType := dftBoolean;
      ftFloat, ftExtended, ftSingle: FDataType := dftFloat;
      ftDate..ftDateTime, ftTimeStamp: FDataType := dftDateTime;
      ftBytes, ftVarBytes, ftBlob, ftGraphic: FDataType := dftBlob;
      ftString, ftMemo, ftFixedChar, ftWideString, ftFixedWideChar, ftWideMemo: FDataType := dftString;
      else FDataType := dftUnknown;
   end;
end;

function TdwsTField.DataType: TdwsDataFieldType;
begin
   result := FDataType;
end;

function TdwsTField.DeclaredType: String;
begin
   result := FField.ClassName;
end;

function TdwsTField.IsNull: Boolean;
begin
   result := FFIeld.IsNull;
end;

function TdwsTField.Name: String;
begin
   result := FField.FieldName;
end;

function TdwsTField.AsBlob: RawByteString;
begin
   result := FField.AsAnsiString;
end;

function TdwsTField.AsBoolean: Boolean;
begin
   result := FField.AsBoolean;
end;

function TdwsTField.AsFloat: Double;
begin
   result := FField.AsFloat;
end;

function TdwsTField.AsInteger: Int64;
begin
   result := FField.AsLargeInt;
end;

function TdwsTField.AsString: String;
begin
   result := FField.AsString;
end;

end.
