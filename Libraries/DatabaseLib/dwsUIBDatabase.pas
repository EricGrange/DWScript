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
    This unit wraps Unified InterBase from Henri Gourvest (www.progdigy.com)

    http://sourceforge.net/projects/uib/

    Parameters:
    - server & database name (ex: 'server:path/to.db.fdb')
    - user name, optional (ex: 'SYSDBA')
    - password, optional (ex: 'masterkey')
    - character set, optional (ex: 'WIN1252')
    - library name, optional (ex: 'fbclient.dll')

}
unit dwsUIBDatabase;

interface

uses
   Classes, SysUtils,
   uib, uiblib, uibmetadata,
   dwsUtils, dwsExprs, dwsDatabase, dwsStack, dwsXPlatform, dwsDataContext, dwsSymbols;

type

   TdwsUIBDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FDB : TUIBDataBase;
         FTransaction : TUIBTransaction;

      protected
         property DB : TUIBDataBase read FDB write FDB;
         property Transaction : TUIBTransaction read FTransaction write FTransaction;

      public
         constructor Create(const parameters : array of String);
         destructor Destroy; override;

         procedure BeginTransaction;
         procedure Commit;
         procedure Rollback;
         function InTransaction : Boolean;
         function CanReleaseToPool : String;

         procedure Exec(const sql : String; const parameters : IScriptDynArray; context : TExprBase);
         function Query(const sql : String; const parameters : IScriptDynArray; context : TExprBase) : IdwsDataSet;

         function VersionInfoText : String;
   end;

   TdwsUIBDataSet = class (TdwsDataSet)
      private
         FDB : TdwsUIBDataBase;
         FQuery : TUIBQuery;

      protected
         procedure DoPrepareFields; override;

      public
         constructor Create(db : TdwsUIBDataBase; const sql : String; const parameters : IScriptDynArray);
         destructor Destroy; override;

         function Eof : Boolean; override;
         procedure Next; override;

         function FieldCount : Integer; override;
   end;

   TdwsUIBDataField = class (TdwsDataField)
      protected
         function GetName : String; override;
         function GetDataType : TdwsDataFieldType; override;
         function GetDeclaredType : String; override;

      public
         constructor Create(dataSet : TdwsUIBDataSet; fieldIndex : Integer);

         function IsNull : Boolean; override;
         procedure AsString(var Result : String); override;
         function AsInteger : Int64; override;
         function AsFloat : Double; override;
         function AsBoolean : Boolean; override;
         function AsBlob : RawByteString; override;
   end;

//   IdwsBlob = interface
//      ['{018C9441-3177-49E1-97EF-EA5F2584FA60}']
//   end;

   TdwsUIBDataBaseFactory = class (TdwsDataBaseFactory)
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

procedure AssignParameters(var rq : TUIBQuery; const params : IScriptDynArray);
var
   i : Integer;
   p : PVarData;
   v : Variant;
   rqParams : TSQLParams;
begin
   rqParams:=rq.Params;
   for i:=0 to params.ArrayLength-1 do begin
      params.EvalAsVariant(i, v);
      p := PVarData(@v);
      case p.VType of
         varInt64 : rqParams.AsInt64[i]:=p.VInt64;
         varDouble : rqParams.AsDouble[i]:=p.VDouble;
         varUString : rqParams.AsString[i]:=String(p.VUString);
         varBoolean : rqParams.AsBoolean[i]:=p.VBoolean;
         varNull : rqParams.IsNull[i]:=True;
      else
         rqParams.AsVariant[i]:=PVariant(p)^;
      end;
   end;
end;

// ------------------
// ------------------ TdwsUIBDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsUIBDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsUIBDataBase;
begin
   db:=TdwsUIBDataBase.Create(parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsUIBDataBase ------------------
// ------------------

// Create
//
constructor TdwsUIBDataBase.Create(const parameters : array of String);
var
   nbParams : Integer;
   dbName : String;
begin
   if FDB=nil then begin
      nbParams := Length(parameters);
      if nbParams > 0 then
         dbName := TdwsDataBase.ApplyPathVariables(parameters[0]);
      try
         FDB:=TUIBDataBase.Create{$ifndef UIB_NO_COMPONENT}(nil){$endif};
         FDB.DatabaseName:=dbName;
         if nbParams > 1 then
            FDB.UserName := parameters[1];
         if nbParams > 2 then
            FDB.PassWord := parameters[2];
         if nbParams > 3 then
            FDB.CharacterSet := uiblib.StrToCharacterSet(ScriptStringToRawByteString(parameters[3]));
         if nbParams > 4 then
            FDB.LibraryName := parameters[4];
         FDB.Connected:=True;
      except
         RefCount:=0;
         raise;
      end;
   end;
   if FTransaction=nil then begin
      FTransaction:=TUIBTransaction.Create{$ifndef UIB_NO_COMPONENT}(nil){$endif};
      FTransaction.DataBase:=FDB;
   end;
end;

// Destroy
//
destructor TdwsUIBDataBase.Destroy;
begin
   FTransaction.Free;
   FDB.Free;
   inherited;
end;

// BeginTransaction
//
procedure TdwsUIBDataBase.BeginTransaction;
begin
   FTransaction.StartTransaction;
end;

// Commit
//
procedure TdwsUIBDataBase.Commit;
begin
   FTransaction.Commit;
end;

// Rollback
//
procedure TdwsUIBDataBase.Rollback;
begin
   FTransaction.RollBack;
end;

// InTransaction
//
function TdwsUIBDataBase.InTransaction : Boolean;
begin
   Result:=FTransaction.InTransaction;
end;

// CanReleasetoPool
//
function TdwsUIBDataBase.CanReleasetoPool : String;
begin
   // Note: UIB can't have datasets open outside transactions
   if InTransaction then
      Result:='in transaction'
   else Result:='';
end;

// Exec
//
procedure TdwsUIBDataBase.Exec(const sql : String; const parameters : IScriptDynArray; context : TExprBase);
var
   rq : TUIBQuery;
begin
   rq:=TUIBQuery.Create{$ifndef UIB_NO_COMPONENT}(nil){$endif};
   try
      rq.Transaction:=FTransaction;
      rq.SQL.Text:=sql;
      AssignParameters(rq, parameters);
      rq.Execute;
   except
      rq.Close;
      raise;
   end;
end;

// Query
//
function TdwsUIBDataBase.Query(const sql : String; const parameters : IScriptDynArray; context : TExprBase) : IdwsDataSet;
var
   ds : TdwsUIBDataSet;
begin
   ds:=TdwsUIBDataSet.Create(Self, sql, parameters);
   Result:=ds;
end;

// VersionInfoText
//
function TdwsUIBDataBase.VersionInfoText : String;
begin
   Result:='UIB '+DB.InfoVersion;
end;

// ------------------
// ------------------ TdwsUIBDataSet ------------------
// ------------------

// Create
//
constructor TdwsUIBDataSet.Create(db : TdwsUIBDataBase; const sql : String; const parameters : IScriptDynArray);
begin
   FDB:=db;
   inherited Create(db);
   FQuery:=TUIBQuery.Create{$ifndef UIB_NO_COMPONENT}(nil){$endif};
   try
      FQuery.CachedFetch := False;
      FQuery.UseCursor := False;
      FQuery.FetchBlobs := True;
      FQuery.Transaction := db.FTransaction;
      FQuery.SQL.Text := sql;
      FQuery.Prepare(True);
      AssignParameters(FQuery, parameters);
      FQuery.Open;
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsUIBDataSet.Destroy;
begin
   FQuery.Free;
   inherited;
end;

// Eof
//
function TdwsUIBDataSet.Eof : Boolean;
begin
   Result:=FQuery.Eof;
end;

// Next
//
procedure TdwsUIBDataSet.Next;
begin
   FQuery.Next;
end;

// FieldCount
//
function TdwsUIBDataSet.FieldCount : Integer;
begin
   Result:=FQuery.Fields.FieldCount;
end;

// DoPrepareFields
//
procedure TdwsUIBDataSet.DoPrepareFields;
var
   i, n : Integer;
begin
   n:=FQuery.Fields.FieldCount;
   SetLength(FFields, n);
   for i:=0 to n-1 do
      FFields[i]:=TdwsUIBDataField.Create(Self, i);
end;

// ------------------
// ------------------ TdwsUIBDataField ------------------
// ------------------

// Create
//
constructor TdwsUIBDataField.Create(dataSet : TdwsUIBDataSet; fieldIndex : Integer);
begin
   inherited Create(dataSet, fieldIndex);
end;

// IsNull
//
function TdwsUIBDataField.IsNull : Boolean;
begin
   Result:=TdwsUIBDataSet(DataSet).FQuery.Fields.IsNull[Index];
end;

// GetName
//
function TdwsUIBDataField.GetName : String;
var
   fields : TSQLResult;
begin
   fields := TdwsUIBDataSet(DataSet).FQuery.Fields;
   Result := fields.AliasName[Index];
end;

// GetDataType
//
function TdwsUIBDataField.GetDataType : TdwsDataFieldType;
var
   uibft : TUIBFieldType;
begin
   uibft:=TdwsUIBDataSet(DataSet).FQuery.Fields.FieldType[Index];
   case uibft of
      uftNumeric, uftFloat, uftDoublePrecision : Result:=dftFloat;
      uftChar, uftVarchar, uftCstring : Result:=dftString;
      uftSmallint, uftInteger, uftInt64 : Result:=dftInteger;
      uftTimestamp, uftDate, uftTime : Result:=dftDateTime;
      uftBlob, uftBlobId : Result:=dftBlob;
      {$IFDEF IB7_UP}
      uftBoolean : Result:=dftBoolean;
      {$ENDIF}
      {$IFDEF FB25_UP}
      uftNull : Result:=dftNull;
      {$ENDIF}
   else
      Result:=dftUnknown;
   end;
end;

// GetDeclaredType
//
function TdwsUIBDataField.GetDeclaredType : String;
const
   cFieldTypes: array [TUIBFieldType] of string =
   ('', 'NUMERIC', 'CHAR', 'VARCHAR', 'CSTRING', 'SMALLINT', 'INTEGER', 'QUAD',
    'FLOAT', 'DOUBLE PRECISION', 'TIMESTAMP', 'BLOB', 'BLOBID', 'DATE', 'TIME',
    'BIGINT' , 'ARRAY'{$IFDEF IB7_UP}, 'BOOLEAN' {$ENDIF}
    {$IFDEF FB25_UP}, 'NULL'{$ENDIF});
begin
   Result:=cFieldTypes[TdwsUIBDataSet(DataSet).FQuery.Fields.FieldType[Index]];
end;

// AsString
//
procedure TdwsUIBDataField.AsString(var Result : String);
begin
   Result:=TdwsUIBDataSet(DataSet).FQuery.Fields.AsString[Index];
end;

// AsInteger
//
function TdwsUIBDataField.AsInteger : Int64;
begin
   Result:=TdwsUIBDataSet(DataSet).FQuery.Fields.AsInteger[Index];
end;

// AsFloat
//
function TdwsUIBDataField.AsFloat : Double;
begin
   Result:=TdwsUIBDataSet(DataSet).FQuery.Fields.AsDouble[Index];
end;

// AsBoolean
//
function TdwsUIBDataField.AsBoolean : Boolean;
begin
   Result:=TdwsUIBDataSet(DataSet).FQuery.Fields.AsBoolean[Index];
end;

// AsBlob
//
function TdwsUIBDataField.AsBlob : RawByteString;
begin
   Result:=TdwsUIBDataSet(DataSet).FQuery.Fields.AsRawByteString[Index];
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsDatabase.RegisterDriver('UIB', TdwsUIBDataBaseFactory.Create);

end.
