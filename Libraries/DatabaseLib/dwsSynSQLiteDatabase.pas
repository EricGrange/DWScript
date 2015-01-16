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

   TdwsSynSQLiteRequest = class
      private
         FRequest : TSQLRequest;
         FSQL : String;

      public
         destructor Destroy; override;

         procedure Prepare(db : TSQLite3DB; const sql : String);

         procedure AssignParameters(const params : TData);

         property Request : TSQLRequest read FRequest;
         property SQL : String read FSQL;
   end;

   TdwsSynSQLiteDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FDB : TSQLDatabase;
         FDataSets : Integer;
         FRequest : TdwsSynSQLiteRequest;

      protected
         function AcquireRequest(const sql : String) : TdwsSynSQLiteRequest;
         procedure ReleaseRequest(var r : TdwsSynSQLiteRequest);

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
         FQuery : TdwsSynSQLiteRequest;
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
   FreeAndNil(FRequest);
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
   else Result:='';
end;

// Exec
//
procedure TdwsSynSQLiteDataBase.Exec(const sql : String; const parameters : TData);
var
   rq : TdwsSynSQLiteRequest;
begin
   rq:=AcquireRequest(sql);
   try
      if rq.Request.Request<>0 then begin
         rq.AssignParameters(parameters);
         while rq.Request.Step=SQLITE_ROW do ;
      end;
      ReleaseRequest(rq);
   except
      rq.Free;
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

// AcquireRequest
//
function TdwsSynSQLiteDataBase.AcquireRequest(const sql : String) : TdwsSynSQLiteRequest;
begin
   if FRequest=nil then
      FRequest:=TdwsSynSQLiteRequest.Create;
   if FRequest.SQL<>sql then
      FRequest.Prepare(FDB.DB, sql);
   Result:=FRequest;
   FRequest:=nil;
end;

// ReleaseRequest
//
procedure TdwsSynSQLiteDataBase.ReleaseRequest(var r : TdwsSynSQLiteRequest);
begin
   if FRequest=nil then begin
      if r.Request.Request<>0 then begin
         if r.Request.Reset=SQLITE_OK then
            r.Request.BindReset
         else FreeAndNil(r);
      end;
      FRequest:=r
   end else r.Free;
   r:=nil;
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
      FQuery:=db.AcquireRequest(sql);
      try
         if FQuery.Request.Request<>0 then begin
            FQuery.AssignParameters(parameters);
            FEOFReached:=(FQuery.Request.Step=SQLITE_DONE);
         end else FEOFReached:=True;
         Inc(FDB.FDataSets);
      except
         FQuery.Free;
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
   FDB.ReleaseRequest(FQuery);
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
   FEOFReached:=(FQuery.Request.Step=SQLITE_DONE);
end;

// FieldCount
//
function TdwsSynSQLiteDataSet.FieldCount : Integer;
begin
   Result:=FQuery.Request.FieldCount;
end;

// DoPrepareFields
//
procedure TdwsSynSQLiteDataSet.DoPrepareFields;
var
   i, n : Integer;
begin
   n:=FQuery.Request.FieldCount;
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
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.Request.FieldNull(Index);
end;

// GetName
//
function TdwsSynSQLiteDataField.GetName : String;
begin
   Result:=UTF8ToString(TdwsSynSQLiteDataSet(DataSet).FQuery.Request.FieldName(Index));
end;

// GetDataType
//
function TdwsSynSQLiteDataField.GetDataType : TdwsDataFieldType;
begin
   Result:=SQLiteTypeToDataType(TdwsSynSQLiteDataSet(DataSet).FQuery.Request.FieldType(Index));
end;

// GetDeclaredType
//
function TdwsSynSQLiteDataField.GetDeclaredType : String;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.Request.FieldDeclaredTypeS(Index);
end;

// AsString
//
function TdwsSynSQLiteDataField.AsString : String;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.Request.FieldS(Index);
end;

// AsInteger
//
function TdwsSynSQLiteDataField.AsInteger : Int64;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.Request.FieldInt(Index);
end;

// AsFloat
//
function TdwsSynSQLiteDataField.AsFloat : Double;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.Request.FieldDouble(Index);
end;

// AsBlob
//
function TdwsSynSQLiteDataField.AsBlob : RawByteString;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.Request.FieldBlob(Index);
end;

// ------------------
// ------------------ TdwsSynSQLiteRequest ------------------
// ------------------

// Destroy
//
destructor TdwsSynSQLiteRequest.Destroy;
begin
   if FRequest.Request<>0 then
      FRequest.Close;
end;

// Prepare
//
procedure TdwsSynSQLiteRequest.Prepare(db : TSQLite3DB; const sql : String);
begin
   FRequest.Prepare(db, StringToUTF8(sql));
   FSQL:=sql;
end;

// AssignParameters
//
procedure TdwsSynSQLiteRequest.AssignParameters(const params : TData);
var
   i : Integer;
   p : PVarData;
begin
   for i:=1 to Length(params) do begin
      p:=PVarData(@params[i-1]);
      case p.VType of
         varInt64 : Request.Bind(i, p.VInt64);
         varDouble : Request.Bind(i, p.VDouble);
         varUString : Request.BindS(i, String(p.VUString));
         varBoolean : Request.Bind(i, Ord(p.VBoolean));
         varNull : Request.BindNull(i);
         varString : Request.Bind(i, p.VString, Length(RawByteString(p.VString)));
      else
         raise Exception.CreateFmt('Unsupported VarType %d', [p.VType]);
      end;
   end;
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
