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
    This unit wraps SynSQLite3 from Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info
}
unit dwsSynSQLiteDatabase;

interface

uses
   Classes, Variants, SysUtils,
   SynSQLite3, SynCommons,
   dwsUtils, dwsExprs, dwsDatabase, dwsStack, dwsXPlatform, dwsDataContext;

type

   TdwsSynSQLiteDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FDB : TSQLDatabase;

      public
         constructor Create(const parameters : array of String);
         destructor Destroy; override;

         procedure BeginTransaction;
         procedure Commit;
         procedure Rollback;
         function InTransaction : Boolean;

         procedure Exec(const sql : String; const parameters : TData);
         function Query(const sql : String; const parameters : TData) : IdwsDataSet;

         function VersionInfoText : String;
   end;

   TdwsSynSQLiteDataSet = class (TdwsDataSet)
      private
         FDB : TdwsSynSQLiteDataBase;
         FQuery : TSQLRequest;
         FEOFReached : Boolean;

      protected
         procedure DoPrepareFields; override;

      public
         constructor Create(db : TdwsSynSQLiteDataBase; const sql : String; const parameters : TData);
         destructor Destroy; override;

         function Eof : Boolean; override;
         procedure Next; override;

         function FieldCount : Integer; override;
   end;

   TdwsSynSQLiteDataField = class (TdwsDataField)
      protected
         function GetName : String; override;
         function GetDataType : TdwsDataFieldType; override;
         function GetDeclaredType : String; override;

      public
         constructor Create(dataSet : TdwsSynSQLiteDataSet; fieldIndex : Integer);

         function IsNull : Boolean; override;
         function AsString : String; override;
         function AsInteger : Int64; override;
         function AsFloat : Double; override;
   end;

//   IdwsBlob = interface
//      ['{018C9441-3177-49E1-97EF-EA5F2584FA60}']
//   end;

   TdwsSynSQLiteDataBaseFactory = class (TdwsDataBaseFactory)
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

var
   vSQLite3DynamicCS : TFixedCriticalSection;

procedure InitializeSQLite3Dynamic;
begin
   vSQLite3DynamicCS.Enter;
   try
      if sqlite3=nil then
         sqlite3:=TSQLite3LibraryDynamic.Create;
   finally
      vSQLite3DynamicCS.Leave;
   end;
end;

function SQLiteTypeToDataType(sqliteType : Integer) : TdwsDataFieldType;
const
   cSQLiteTypeToDataType : array [SQLITE_INTEGER..SQLITE_NULL] of TdwsDataFieldType = (
      dftInteger, dftFloat, dftString, dftBlob, dftNull
   );
begin
   if sqliteType in [Low(cSQLiteTypeToDataType)..High(SQLITE_NULL)] then
      Result:=cSQLiteTypeToDataType[sqliteType]
   else Result:=dftUnknown;
end;

procedure AssignParameters(var rq : TSQLRequest; const params : TData);
var
   i : Integer;
   p : PVarData;
begin
   for i:=1 to Length(params) do begin
      p:=PVarData(@params[i-1]);
      case p.VType of
         varInt64 : rq.Bind(i, p.VInt64);
         varDouble : rq.Bind(i, p.VDouble);
         varUString : rq.BindS(i, String(p.VUString));
         varBoolean : rq.Bind(i, Ord(p.VBoolean));
         varNull : rq.BindNull(i);
      else
         raise Exception.CreateFmt('Unsupported VarType %d', [p.VType]);
      end;
   end;
end;

// ------------------
// ------------------ TdwsSynSQLiteDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynSQLiteDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynSQLiteDataBase;
begin
   if sqlite3=nil then
      InitializeSQLite3Dynamic;
   db:=TdwsSynSQLiteDataBase.Create(parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynSQLiteDataBase ------------------
// ------------------

// Create
//
constructor TdwsSynSQLiteDataBase.Create(const parameters : array of String);
var
   dbName : String;
begin
   if Length(parameters)>0 then
      dbName:=TdwsDataBase.ApplyPathVariables(parameters[0])
   else dbName:=':memory:';
   try
      FDB:=TSQLDatabase.Create(dbName);
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynSQLiteDataBase.Destroy;
begin
   FDB.Free;
   inherited;
end;

// BeginTransaction
//
procedure TdwsSynSQLiteDataBase.BeginTransaction;
begin
   FDB.TransactionBegin;
end;

// Commit
//
procedure TdwsSynSQLiteDataBase.Commit;
begin
   FDB.Commit;
end;

// Rollback
//
procedure TdwsSynSQLiteDataBase.Rollback;
begin
   FDB.Rollback;
end;

// InTransaction
//
function TdwsSynSQLiteDataBase.InTransaction : Boolean;
begin
   Result:=FDB.TransactionActive;
end;

// Exec
//
procedure TdwsSynSQLiteDataBase.Exec(const sql : String; const parameters : TData);
var
   rq : TSQLRequest;
begin
   rq.Prepare(FDB.DB, StringToUTF8(sql));
   try
      AssignParameters(rq, parameters);
      rq.Execute;
   except
      rq.Close;
      raise;
   end;
end;

// Query
//
function TdwsSynSQLiteDataBase.Query(const sql : String; const parameters : TData) : IdwsDataSet;
var
   ds : TdwsSynSQLiteDataSet;
begin
   ds:=TdwsSynSQLiteDataSet.Create(Self, sql, parameters);
   Result:=ds;
end;

// VersionInfoText
//
function TdwsSynSQLiteDataBase.VersionInfoText : String;
begin
   Result:=UTF8ToString(sqlite3.libversion);
end;

// ------------------
// ------------------ TdwsSynSQLiteDataSet ------------------
// ------------------

// Create
//
constructor TdwsSynSQLiteDataSet.Create(db : TdwsSynSQLiteDataBase; const sql : String; const parameters : TData);
begin
   FDB:=db;
   inherited Create(db);
   try
      FQuery.Prepare(db.FDB.DB, StringToUTF8(sql));
      try
         AssignParameters(FQuery, parameters);
         FEOFReached:=(FQuery.Step=SQLITE_DONE);
      except
         FQuery.Close;
         raise;
      end;
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynSQLiteDataSet.Destroy;
begin
   if FQuery.Request<>0 then
      FQuery.Close;
   inherited;
end;

// Eof
//
function TdwsSynSQLiteDataSet.Eof : Boolean;
begin
   Result:=FEOFReached;
end;

// Next
//
procedure TdwsSynSQLiteDataSet.Next;
begin
   FEOFReached:=(FQuery.Step=SQLITE_DONE);
end;

// FieldCount
//
function TdwsSynSQLiteDataSet.FieldCount : Integer;
begin
   Result:=FQuery.FieldCount;
end;

// DoPrepareFields
//
procedure TdwsSynSQLiteDataSet.DoPrepareFields;
var
   i, n : Integer;
begin
   n:=FQuery.FieldCount;
   SetLength(FFields, n);
   for i:=0 to n-1 do
      FFields[i]:=TdwsSynSQLiteDataField.Create(Self, i);
end;

// ------------------
// ------------------ TdwsSynSQLiteDataField ------------------
// ------------------

// Create
//
constructor TdwsSynSQLiteDataField.Create(dataSet : TdwsSynSQLiteDataSet; fieldIndex : Integer);
begin
   inherited Create(dataSet, fieldIndex);
end;

// IsNull
//
function TdwsSynSQLiteDataField.IsNull : Boolean;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldNull(Index);
end;

// GetName
//
function TdwsSynSQLiteDataField.GetName : String;
begin
   Result:=UTF8ToString(TdwsSynSQLiteDataSet(DataSet).FQuery.FieldName(Index));
end;

// GetDataType
//
function TdwsSynSQLiteDataField.GetDataType : TdwsDataFieldType;
begin
   Result:=SQLiteTypeToDataType(TdwsSynSQLiteDataSet(DataSet).FQuery.FieldType(Index));
end;

// GetDeclaredType
//
function TdwsSynSQLiteDataField.GetDeclaredType : String;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldDeclaredTypeS(Index);
end;

// AsString
//
function TdwsSynSQLiteDataField.AsString : String;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldS(Index);
end;

// AsInteger
//
function TdwsSynSQLiteDataField.AsInteger : Int64;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldInt(Index);
end;

// AsFloat
//
function TdwsSynSQLiteDataField.AsFloat : Double;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldDouble(Index);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsDatabase.RegisterDriver('SQLite', TdwsSynSQLiteDataBaseFactory.Create);

   vSQLite3DynamicCS:=TFixedCriticalSection.Create;

finalization

   FreeAndNil(vSQLite3DynamicCS);

end.
