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
    This unit wraps SynDBODBC from Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info
}
unit dwsSynODBCDatabase;

interface

uses
   Classes, Variants, SysUtils,
   SynDB, SynDBODBC,
   dwsUtils, dwsExprs, dwsDatabase, dwsStack, dwsXPlatform;

type

   TdwsSynODBCDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FProps : TSQLDBConnectionProperties;
         FConn : TODBCConnection;

      public
         constructor Create(const parameters : array of String); override;
         destructor Destroy; override;

         procedure BeginTransaction;
         procedure Commit;
         procedure Rollback;
         function InTransaction : Boolean;

         procedure Exec(const sql : String; const parameters : TData);
         function Query(const sql : String; const parameters : TData) : IdwsDataSet;
   end;

   TdwsSynODBCDataSet = class (TdwsDataSet)
      private
         FDB : TdwsSynODBCDataBase;
         FStmt : TODBCStatement;
         FEOFReached : Boolean;

      protected
         procedure DoPrepareFields; override;

      public
         constructor Create(db : TdwsSynODBCDataBase; const sql : String; const parameters : TData);
         destructor Destroy; override;

         function Eof : Boolean; override;
         procedure Next; override;

         function FieldCount : Integer; override;
   end;

   TdwsSynODBCDataField = class (TdwsDataField)
      protected
         function GetName : String; override;
         function GetDataType : TdwsDataFieldType; override;
         function GetDeclaredType : String; override;

      public
         constructor Create(dataSet : TdwsSynODBCDataSet; fieldIndex : Integer);

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

procedure AssignParameters(var stmt : TODBCStatement; const params : TData);
var
   i : Integer;
   p : PVarData;
begin
   for i:=1 to Length(params) do begin
      p:=PVarData(@params[i-1]);
      case p.VType of
         varInt64 : stmt.Bind(i, p.VInt64);
         varDouble : stmt.Bind(i, p.VDouble);
         varUString : stmt.BindTextS(i, String(p.VUString));
         varBoolean : stmt.Bind(i, Ord(p.VBoolean));
         varNull : stmt.BindNull(i);
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
   db : TdwsSynODBCDataBase;
begin
   db:=TdwsSynODBCDataBase.Create(parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynODBCDataBase ------------------
// ------------------

// Create
//
constructor TdwsSynODBCDataBase.Create(const parameters : array of String);
var
   n : Integer;
   serverName, dbName, user, passWord : String;
begin
   n:=Length(parameters);
   if n>0 then
      serverName:=parameters[0];
   if n>1 then
      dbName:=TdwsDataBase.ApplyPathVariables(parameters[1]);
   if n>2 then
      user:=parameters[2];
   if n>3 then
      passWord:=parameters[3];
   try
      FProps:=TODBCConnectionProperties.Create(UTF8Encode(serverName), UTF8Encode(dbName),
                                               UTF8Encode(user), UTF8Encode(passWord));
      FConn:=(FProps.NewConnection as TODBCConnection);
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynODBCDataBase.Destroy;
begin
   FConn.Free;
   FProps.Free;
   inherited;
end;

// BeginTransaction
//
procedure TdwsSynODBCDataBase.BeginTransaction;
begin
   FConn.StartTransaction;
end;

// Commit
//
procedure TdwsSynODBCDataBase.Commit;
begin
   FConn.Commit;
end;

// Rollback
//
procedure TdwsSynODBCDataBase.Rollback;
begin
   FConn.Rollback;
end;

// InTransaction
//
function TdwsSynODBCDataBase.InTransaction : Boolean;
begin
   Result:=(FConn.TransactionCount>0);
end;

// Exec
//
procedure TdwsSynODBCDataBase.Exec(const sql : String; const parameters : TData);
var
   stmt : TODBCStatement;
begin
   stmt:=TODBCStatement.Create(FConn);
   try
      stmt.Prepare(UTF8Encode(sql));
      AssignParameters(stmt, parameters);
      stmt.ExecutePrepared;
   finally
      stmt.Free;
   end;
end;

// Query
//
function TdwsSynODBCDataBase.Query(const sql : String; const parameters : TData) : IdwsDataSet;
var
   ds : TdwsSynODBCDataSet;
begin
   ds:=TdwsSynODBCDataSet.Create(Self, sql, parameters);
   Result:=ds;
end;

// ------------------
// ------------------ TdwsSynODBCDataSet ------------------
// ------------------

// Create
//
constructor TdwsSynODBCDataSet.Create(db : TdwsSynODBCDataBase; const sql : String; const parameters : TData);
begin
   FDB:=db;
   inherited Create(db);
   try
      FStmt:=TODBCStatement.Create(db.FConn);
      FStmt.Prepare(UTF8Encode(sql), True);
      AssignParameters(FStmt, parameters);
      FStmt.ExecutePrepared;
      FEOFReached:=not FStmt.Step;
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynODBCDataSet.Destroy;
begin
   FStmt.Free;
   inherited;
end;

// Eof
//
function TdwsSynODBCDataSet.Eof : Boolean;
begin
   Result:=FEOFReached;
end;

// Next
//
procedure TdwsSynODBCDataSet.Next;
begin
   FEOFReached:=not FStmt.Step;
end;

// FieldCount
//
function TdwsSynODBCDataSet.FieldCount : Integer;
begin
   Result:=FStmt.ColumnCount;
end;

// DoPrepareFields
//
procedure TdwsSynODBCDataSet.DoPrepareFields;
var
   i, n : Integer;
begin
   n:=FStmt.ColumnCount;
   SetLength(FFields, n);
   for i:=0 to n-1 do
      FFields[i]:=TdwsSynODBCDataField.Create(Self, i);
end;

// ------------------
// ------------------ TdwsSynODBCDataField ------------------
// ------------------

// Create
//
constructor TdwsSynODBCDataField.Create(dataSet : TdwsSynODBCDataSet; fieldIndex : Integer);
begin
   inherited Create(dataSet, fieldIndex);
end;

// IsNull
//
function TdwsSynODBCDataField.IsNull : Boolean;
begin
   Result:=TdwsSynODBCDataSet(DataSet).FStmt.ColumnNull(Index);
end;

// GetName
//
function TdwsSynODBCDataField.GetName : String;
begin
   Result:=UTF8ToString(TdwsSynODBCDataSet(DataSet).FStmt.ColumnName(Index));
end;

// GetDataType
//
function TdwsSynODBCDataField.GetDataType : TdwsDataFieldType;
const
   cSynDBTypeToDataType : array [TSQLDBFieldType] of TdwsDataFieldType = (
      dftUnknown, dftNull, dftInteger, dftFloat, dftFloat, dftDateTime, dftString, dftBlob
   );
begin
   Result:=cSynDBTypeToDataType[TdwsSynODBCDataSet(DataSet).FStmt.ColumnType(Index)];
end;

// GetDeclaredType
//
function TdwsSynODBCDataField.GetDeclaredType : String;
const
   cSynDBTypeToString : array [TSQLDBFieldType] of String = (
      'Unknown', 'Null', 'Integer', 'Float', 'Currency', 'Date', 'Text', 'Blob'
   );
begin
   Result:=cSynDBTypeToString[TdwsSynODBCDataSet(DataSet).FStmt.ColumnType(Index)];
end;

// AsString
//
function TdwsSynODBCDataField.AsString : String;
begin
   Result:=TdwsSynODBCDataSet(DataSet).FStmt.ColumnString(Index);
end;

// AsInteger
//
function TdwsSynODBCDataField.AsInteger : Int64;
begin
   Result:=TdwsSynODBCDataSet(DataSet).FStmt.ColumnInt(Index);
end;

// AsFloat
//
function TdwsSynODBCDataField.AsFloat : Double;
begin
   Result:=TdwsSynODBCDataSet(DataSet).FStmt.ColumnDouble(Index);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsDatabase.RegisterDriver('ODBC', TdwsSynSQLiteDataBaseFactory.Create);

end.
