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
   TdwsSynSQLiteDataSet = class;

   TdwsSynSQLiteDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FDB : TSQLDatabase;
         FDataSets : Integer;
         FExecRequest : TSQLRequest;
         FExecSQL : String;

      protected

      public
         constructor Create(const parameters : array of String);
         destructor Destroy; override;

         procedure BeginTransaction;
         procedure Commit;
         procedure Rollback;
         function InTransaction : Boolean;
         function CanReleaseToPool : String;

         procedure Exec(const sql : String; const parameters : TData);
         function Query(const sql : String; const parameters : TData) : IdwsDataSet;

         function VersionInfoText : String;
   end;

   TdwsSynSQLiteDataSet = class (TdwsDataSet)
      private
         FDB : TdwsSynSQLiteDataBase;
         FRequest : TSQLRequest;
         FEOFReached : Boolean;
         FSQL : String;

      protected
         procedure DoPrepareFields; override;

      public
         constructor Create(db : TdwsSynSQLiteDataBase; const sql : String; const parameters : TData);
         destructor Destroy; override;

         function Eof : Boolean; override;
         procedure Next; override;

         function FieldCount : Integer; override;

         property SQL : String read FSQL;
   end;

   TdwsSynSQLiteDataField = class (TdwsDataField)
      private
         FDataSet : TdwsSynSQLiteDataSet;

      protected
         function GetName : String; override;
         function GetDataType : TdwsDataFieldType; override;
         function GetDeclaredType : String; override;

      public
         constructor Create(dataSet : TdwsSynSQLiteDataSet; fieldIndex : Integer);

         function DataType : TdwsDataFieldType; override;

         function IsNull : Boolean; override;
         function AsString : String; override;
         function AsInteger : Int64; override;
         function AsFloat : Double; override;
         function AsBlob : RawByteString; override;
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

// SQLAssignParameters
//
procedure SQLAssignParameters(var rq : TSQLRequest; const params : TData);

   procedure BindDateTime(var rq : TSQLRequest; i : Integer; p : PVarData);
   var
      dtStr : String;
   begin
      dtStr:=DateTimeToISO8601(p.VDate, True);
      rq.BindS(i, dtStr);
   end;

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
         varString : rq.Bind(i, p.VString, Length(RawByteString(p.VString)));
         varDate : BindDateTime(rq, i, p);
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
   i, flags : Integer;
begin
   if Length(parameters)>0 then
      dbName:=TdwsDataBase.ApplyPathVariables(parameters[0])
   else dbName:=':memory:';

   flags:=SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
   for i:=1 to High(parameters) do begin
      if UnicodeSameText(parameters[i], 'read_only') then
         flags:=SQLITE_OPEN_READONLY
      else if UnicodeSameText(parameters[i], 'shared_cache') then
         flags:=flags or SQLITE_OPEN_SHAREDCACHE;
   end;

   try
      FDB:=TSQLDatabase.Create(dbName, '', flags);
      FDB.BusyTimeout:=1500;
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynSQLiteDataBase.Destroy;
begin
   FExecRequest.Close;
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

// CanReleaseToPool
//
function TdwsSynSQLiteDataBase.CanReleaseToPool : String;
begin
   if FDB.TransactionActive then
      Result:='in transaction'
   else if FDataSets>0 then  // need to check as they could maintain a lock
      Result:='has opened datasets'
   else begin
      FExecRequest.Close;
      Result:='';
   end;
end;

// Exec
//
procedure TdwsSynSQLiteDataBase.Exec(const sql : String; const parameters : TData);
begin
   if sql='' then
      raise ESQLite3Exception.CreateFmt('Empty query', []);
   if FExecRequest.Request<>0 then begin
      if FExecSQL<>sql then
         FExecRequest.Close;
   end;
   if FExecRequest.Request=0 then begin
      FExecRequest.Prepare(FDB.DB, StringToUTF8(sql));
      FExecSQL:=sql;
   end;
   try
      SQLAssignParameters(FExecRequest, parameters);
      while FExecRequest.Step=SQLITE_ROW do ;
      if FExecRequest.Reset=SQLITE_OK then
         FExecRequest.BindReset
      else FExecRequest.Close;
   except
      FExecRequest.Close;
      raise;
   end;
end;

// Query
//
function TdwsSynSQLiteDataBase.Query(const sql : String; const parameters : TData) : IdwsDataSet;
var
   ds : TdwsSynSQLiteDataSet;
begin
   if sql='' then
      raise ESQLite3Exception.CreateFmt('Empty query', []);
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
   FSQL:=sql;
   FDB:=db;
   inherited Create(db);
   try
      FRequest.Prepare(db.FDB.DB, StringToUTF8(sql));
      try
         if FRequest.Request<>0 then begin
            SQLAssignParameters(FRequest, parameters);
            FEOFReached:=(FRequest.Step=SQLITE_DONE);
         end else FEOFReached:=True;
         Inc(FDB.FDataSets);
      except
         FRequest.Close;
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
   Dec(FDB.FDataSets);
   FRequest.Close;
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
   FEOFReached:=(FRequest.Step=SQLITE_DONE);
end;

// FieldCount
//
function TdwsSynSQLiteDataSet.FieldCount : Integer;
begin
   Result:=FRequest.FieldCount;
end;

// DoPrepareFields
//
procedure TdwsSynSQLiteDataSet.DoPrepareFields;
var
   i, n : Integer;
begin
   n:=FRequest.FieldCount;
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
   FDataSet:=dataSet;
   inherited Create(dataSet, fieldIndex);
end;

// DataType
//
function TdwsSynSQLiteDataField.DataType : TdwsDataFieldType;
begin
   Result:=GetDataType;
end;

// IsNull
//
function TdwsSynSQLiteDataField.IsNull : Boolean;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynSQLiteDataSet(DataSet).FRequest.FieldNull(Index);
end;

// GetName
//
function TdwsSynSQLiteDataField.GetName : String;
begin
   Result:=UTF8ToString(TdwsSynSQLiteDataSet(DataSet).FRequest.FieldName(Index));
end;

// GetDataType
//
function TdwsSynSQLiteDataField.GetDataType : TdwsDataFieldType;
begin
   Result:=SQLiteTypeToDataType(TdwsSynSQLiteDataSet(DataSet).FRequest.FieldType(Index));
end;

// GetDeclaredType
//
function TdwsSynSQLiteDataField.GetDeclaredType : String;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FRequest.FieldDeclaredTypeS(Index);
end;

// AsString
//
function TdwsSynSQLiteDataField.AsString : String;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynSQLiteDataSet(DataSet).FRequest.FieldS(Index);
end;

// AsInteger
//
function TdwsSynSQLiteDataField.AsInteger : Int64;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynSQLiteDataSet(DataSet).FRequest.FieldInt(Index);
end;

// AsFloat
//
function TdwsSynSQLiteDataField.AsFloat : Double;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynSQLiteDataSet(DataSet).FRequest.FieldDouble(Index);
end;

// AsBlob
//
function TdwsSynSQLiteDataField.AsBlob : RawByteString;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynSQLiteDataSet(DataSet).FRequest.FieldBlob(Index);
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
