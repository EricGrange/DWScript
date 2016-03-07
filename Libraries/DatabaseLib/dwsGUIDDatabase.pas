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
{
   This is a pseudo-database where all queries return an infinite database
   comprised of a single column whose values are GUIDs
}
unit dwsGUIDDatabase;

interface

uses
   Classes, Variants, SysUtils,
   dwsUtils, dwsExprs, dwsDatabase, dwsStack, dwsXPlatform, dwsDataContext, dwsSymbols;

type

   TdwsGUIDDataBase = class (TdwsDataBase, IdwsDataBase)
      public
         constructor Create(const parameters : array of String);

         procedure BeginTransaction;
         procedure Commit;
         procedure Rollback;
         function InTransaction : Boolean;
         function CanReleaseToPool : String;

         procedure Exec(const sql : String; const parameters : TData; context : TExprBase);
         function Query(const sql : String; const parameters : TData; context : TExprBase) : IdwsDataSet;

         function VersionInfoText : String;
   end;

   TdwsGUIDDataSet = class (TdwsDataSet)
      private
         FGUID : TGUID;

      protected
         procedure DoPrepareFields; override;

      public
         function Eof : Boolean; override;
         procedure Next; override;

         function FieldCount : Integer; override;
   end;

   TdwsGUIDDataField = class (TdwsDataField)
      protected
         function GetName : String; override;
         function GetDataType : TdwsDataFieldType; override;
         function GetDeclaredType : String; override;

      public
         constructor Create(dataSet : TdwsGUIDDataSet; fieldIndex : Integer);

         function IsNull : Boolean; override;
         function AsString : String; override;
         function AsInteger : Int64; override;
         function AsFloat : Double; override;
         function AsBlob : RawByteString; override;
   end;

//   IdwsBlob = interface
//      ['{018C9441-3177-49E1-97EF-EA5F2584FA60}']
//   end;

   TdwsGUIDBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ToBase32
//
function ToBase32(value : UInt64) : String;
const
   cBase32 : String = 'ABCDEFGHIJKLMNOPQRTSUVWXYZ234567'; //  RFC 4648
var
   i, n : Integer;
   buf : array [0..(64 div 5)+1] of Char;
begin
   Assert(Length(cBase32)=32);
   n:=0;
   repeat
      i:=(value and 31);
      value:=(value shr 5);
      buf[n]:=cBase32[i+1];
      Inc(n);
   until value=0;
   SetString(Result, buf, n);
end;

// GUIDToString32
//
function GUIDToString32(const g : TGUID) : String;
begin
   Result:=ToBase32(PUInt64(@g.D1)^)+ToBase32(PUInt64(@g.D4)^);
end;

// ------------------
// ------------------ TdwsGUIDBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsGUIDBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsGUIDDataBase;
begin
   db:=TdwsGUIDDataBase.Create(parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsGUIDDataBase ------------------
// ------------------

// Create
//
constructor TdwsGUIDDataBase.Create(const parameters : array of String);
begin
   // nothing
end;

// BeginTransaction
//
procedure TdwsGUIDDataBase.BeginTransaction;
begin
   raise Exception.Create('Transactions not supported');
end;

// Commit
//
procedure TdwsGUIDDataBase.Commit;
begin
   BeginTransaction;
end;

// Rollback
//
procedure TdwsGUIDDataBase.Rollback;
begin
   Rollback;
end;

// InTransaction
//
function TdwsGUIDDataBase.InTransaction : Boolean;
begin
   Result:=False;
end;

// CanReleaseToPool
//
function TdwsGUIDDataBase.CanReleaseToPool : String;
begin
   Result:='';
end;

// Exec
//
procedure TdwsGUIDDataBase.Exec(const sql : String; const parameters : TData; context : TExprBase);
begin
   raise Exception.Create('Exec not supported');
end;

// Query
//
function TdwsGUIDDataBase.Query(const sql : String; const parameters : TData; context : TExprBase) : IdwsDataSet;
var
   ds : TdwsGUIDDataSet;
begin
   ds:=TdwsGUIDDataSet.Create(Self);
   Result:=ds;
end;

// VersionInfoText
//
function TdwsGUIDDataBase.VersionInfoText : String;
begin
   Result:='GUID pseudo-database';
end;

// ------------------
// ------------------ TdwsGUIDDataSet ------------------
// ------------------

// Eof
//
function TdwsGUIDDataSet.Eof : Boolean;
begin
   Result:=False;
end;

// Next
//
procedure TdwsGUIDDataSet.Next;
begin
   CreateGUID(FGUID);
end;

// FieldCount
//
function TdwsGUIDDataSet.FieldCount : Integer;
begin
   Result:=2;
end;

// DoPrepareFields
//
procedure TdwsGUIDDataSet.DoPrepareFields;
var
   i : Integer;
begin
   Next;
   SetLength(FFields, 2);
   for i:=Low(FFields) to High(FFields) do
      FFields[i]:=TdwsGUIDDataField.Create(Self, i);
end;

// ------------------
// ------------------ TdwsGUIDDataField ------------------
// ------------------

// Create
//
constructor TdwsGUIDDataField.Create(dataSet : TdwsGUIDDataSet; fieldIndex : Integer);
begin
   inherited Create(dataSet, fieldIndex);
end;

// IsNull
//
function TdwsGUIDDataField.IsNull : Boolean;
begin
   Result:=False;
end;

// GetName
//
function TdwsGUIDDataField.GetName : String;
begin
   case Index of
      1 : Result:='GUID32';
   else
      Result:='GUID';
   end;
end;

// GetDataType
//
function TdwsGUIDDataField.GetDataType : TdwsDataFieldType;
begin
   Result:=dftString;
end;

// GetDeclaredType
//
function TdwsGUIDDataField.GetDeclaredType : String;
begin
   Result:='Text';
end;

// AsString
//
function TdwsGUIDDataField.AsString : String;
var
   ds : TdwsGUIDDataSet;
begin
   ds:=TdwsGUIDDataSet(DataSet);
   case Index of
      1 : Result:=GUIDToString32(ds.FGUID);
   else
      Result:=GUIDToString(ds.FGUID);
   end;
end;

// AsInteger
//
function TdwsGUIDDataField.AsInteger : Int64;
begin
   raise Exception.Create('Not supported');
end;

// AsFloat
//
function TdwsGUIDDataField.AsFloat : Double;
begin
   raise Exception.Create('Not supported');
end;

// AsBlob
//
function TdwsGUIDDataField.AsBlob : RawByteString;
begin
   raise Exception.Create('Not supported');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsDatabase.RegisterDriver('GUID', TdwsGUIDBaseFactory.Create);

end.
