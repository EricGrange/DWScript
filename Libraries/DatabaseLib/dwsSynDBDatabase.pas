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
    This unit wraps SynDB from Synopse mORMot framework.
    Specific SynDB drivers must be referred explicitly by including the relevant units
    (dwsSynDBODBCDatabase, dwsSynDBOracleDatabase, etc.)

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info
}
unit dwsSynDBDatabase;

interface

uses
   Classes, Variants, SysUtils,
   SynDB, SynCommons,
   dwsUtils, dwsExprs, dwsDatabase, dwsStack, dwsXPlatform, dwsDataContext;

type

   TdwsSynDBDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FProps : TSQLDBConnectionProperties;
         FConn : TSQLDBConnection;
         FDataSets : Integer;

      public
         constructor Create(connPropsClass : TSQLDBConnectionPropertiesClass; const parameters : array of String); reintroduce;
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

   TdwsSynDBDataSet = class (TdwsDataSet)
      private
         FDB : TdwsSynDBDataBase;
         FStmt : TSQLDBStatement;
         FEOFReached : Boolean;

      protected
         procedure DoPrepareFields; override;

      public
         constructor Create(db : TdwsSynDBDataBase; const sql : String; const parameters : TData);
         destructor Destroy; override;

         function Eof : Boolean; override;
         procedure Next; override;

         function FieldCount : Integer; override;
   end;

   TdwsSynDBDataField = class (TdwsDataField)
      private
         FDataSet : TdwsSynDBDataSet;

      protected
         function GetName : String; override;
         function GetDataType : TdwsDataFieldType; override;
         function GetDeclaredType : String; override;

      public
         constructor Create(dataSet : TdwsSynDBDataSet; fieldIndex : Integer);

         function IsNull : Boolean; override;
         function AsString : String; override;
         function AsInteger : Int64; override;
         function AsFloat : Double; override;
         function AsBlob : RawByteString; override;
   end;

//   IdwsBlob = interface
//      ['{018C9441-3177-49E1-97EF-EA5F2584FA60}']
//   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure AssignParameters(var stmt : TSQLDBStatement; const params : TData);
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
         varDate : stmt.BindDateTime(i, p.VDate);
         varString : stmt.BindBlob(i, p.VString, Length(RawByteString(p.VString)));
      else
         stmt.BindVariant(i, PVariant(p)^, False);
      end;
   end;
end;

// ------------------
// ------------------ TdwsSynDBDataBase ------------------
// ------------------

// Create
//
constructor TdwsSynDBDataBase.Create(connPropsClass : TSQLDBConnectionPropertiesClass; const parameters : array of String);
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
      FProps:=connPropsClass.Create(StringToUTF8(serverName), StringToUTF8(dbName),
                                    StringToUTF8(user), StringToUTF8(passWord));
      FConn:=FProps.NewConnection;
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynDBDataBase.Destroy;
begin
   FConn.Free;
   FProps.Free;
   inherited;
end;

// BeginTransaction
//
procedure TdwsSynDBDataBase.BeginTransaction;
begin
   FConn.StartTransaction;
end;

// Commit
//
procedure TdwsSynDBDataBase.Commit;
begin
   FConn.Commit;
end;

// Rollback
//
procedure TdwsSynDBDataBase.Rollback;
begin
   FConn.Rollback;
end;

// InTransaction
//
function TdwsSynDBDataBase.InTransaction : Boolean;
begin
   Result:=(FConn.TransactionCount>0);
end;

// CanReleaseToPool
//
function TdwsSynDBDataBase.CanReleaseToPool : String;
begin
   if InTransaction then
      Result:='in transaction'
   else if FDataSets>0 then // just to be safe, some drivers may not support it
      Result:='has opened datasets'
   else Result:='';
end;

// Exec
//
procedure TdwsSynDBDataBase.Exec(const sql : String; const parameters : TData);
var
   stmt : TSQLDBStatement;
begin
   stmt:=FConn.NewStatement;
   try
      stmt.Prepare(StringToUTF8(sql), False);
      AssignParameters(stmt, parameters);
      stmt.ExecutePrepared;
   finally
      stmt.Free;
   end;
end;

// Query
//
function TdwsSynDBDataBase.Query(const sql : String; const parameters : TData) : IdwsDataSet;
var
   ds : TdwsSynDBDataSet;
begin
   ds:=TdwsSynDBDataSet.Create(Self, sql, parameters);
   Result:=ds;
end;

// VersionInfoText
//
function TdwsSynDBDataBase.VersionInfoText : String;
begin
   Result:=UTF8ToString(FProps.EngineName);
end;

// ------------------
// ------------------ TdwsSynDBDataSet ------------------
// ------------------

// Create
//
constructor TdwsSynDBDataSet.Create(db : TdwsSynDBDataBase; const sql : String; const parameters : TData);
begin
   FDB:=db;
   inherited Create(db);
   try
      FStmt:=db.FConn.NewStatement;
      FStmt.Prepare(StringToUTF8(sql), True);
      AssignParameters(FStmt, parameters);
      FStmt.ExecutePrepared;
      FEOFReached:=not FStmt.Step;
      Inc(FDB.FDataSets);
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynDBDataSet.Destroy;
begin
   Dec(FDB.FDataSets);
   FStmt.Free;
   inherited;
end;

// Eof
//
function TdwsSynDBDataSet.Eof : Boolean;
begin
   Result:=FEOFReached;
end;

// Next
//
procedure TdwsSynDBDataSet.Next;
begin
   FEOFReached:=not FStmt.Step;
end;

// FieldCount
//
function TdwsSynDBDataSet.FieldCount : Integer;
begin
   Result:=FStmt.ColumnCount;
end;

// DoPrepareFields
//
procedure TdwsSynDBDataSet.DoPrepareFields;
var
   i, n : Integer;
begin
   n:=FStmt.ColumnCount;
   SetLength(FFields, n);
   for i:=0 to n-1 do
      FFields[i]:=TdwsSynDBDataField.Create(Self, i);
end;

// ------------------
// ------------------ TdwsSynDBDataField ------------------
// ------------------

// Create
//
constructor TdwsSynDBDataField.Create(dataSet : TdwsSynDBDataSet; fieldIndex : Integer);
begin
   FDataSet:=dataSet;
   inherited Create(dataSet, fieldIndex);
end;

// IsNull
//
function TdwsSynDBDataField.IsNull : Boolean;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynDBDataSet(DataSet).FStmt.ColumnNull(Index);
end;

// GetName
//
function TdwsSynDBDataField.GetName : String;
begin
   Result:=UTF8ToString(TdwsSynDBDataSet(DataSet).FStmt.ColumnName(Index));
end;

// GetDataType
//
function TdwsSynDBDataField.GetDataType : TdwsDataFieldType;
const
   cSynDBTypeToDataType : array [TSQLDBFieldType] of TdwsDataFieldType = (
      dftUnknown, dftNull, dftInteger, dftFloat, dftFloat, dftDateTime, dftString, dftBlob
   );
begin
   Result:=cSynDBTypeToDataType[TdwsSynDBDataSet(DataSet).FStmt.ColumnType(Index)];
end;

// GetDeclaredType
//
function TdwsSynDBDataField.GetDeclaredType : String;
const
   cSynDBTypeToString : array [TSQLDBFieldType] of String = (
      'Unknown', 'Null', 'Integer', 'Float', 'Currency', 'Date', 'Text', 'Blob'
   );
begin
   Result:=cSynDBTypeToString[TdwsSynDBDataSet(DataSet).FStmt.ColumnType(Index)];
end;

// AsString
//
function TdwsSynDBDataField.AsString : String;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynDBDataSet(DataSet).FStmt.ColumnString(Index);
end;

// AsInteger
//
function TdwsSynDBDataField.AsInteger : Int64;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynDBDataSet(DataSet).FStmt.ColumnInt(Index);
end;

// AsFloat
//
function TdwsSynDBDataField.AsFloat : Double;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynDBDataSet(DataSet).FStmt.ColumnDouble(Index);
end;

// AsBlob
//
function TdwsSynDBDataField.AsBlob : RawByteString;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result:=TdwsSynDBDataSet(DataSet).FStmt.ColumnBlob(Index);
end;

end.
