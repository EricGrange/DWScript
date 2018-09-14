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
   Classes, SysUtils,
   SynSQLite3, SynCommons,
   dwsUtils, dwsExprs, dwsDatabase, dwsXPlatform, dwsXXHash,
   dwsDataContext, dwsStack, dwsSymbols;

type
   TdwsSynSQLiteDataSet = class;

   TdwsSynSQLiteDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FDB : TSQLDatabase;
         FDataSets : Integer;
         FExecRequest : TSQLRequest;
         FExecSQL : String;
         FModules : TNameObjectHash;

      protected
         function GetModule(const name : String) : TObject;
         procedure SetModule(const name : String; aModule : TObject);

      public
         constructor Create(const parameters : array of String);
         destructor Destroy; override;

         procedure BeginTransaction;
         procedure Commit;
         procedure Rollback;
         function InTransaction : Boolean;
         function CanReleaseToPool : String;

         procedure Exec(const sql : String; const parameters : IDataContext; context : TExprBase);
         function Query(const sql : String; const parameters : IDataContext; context : TExprBase) : IdwsDataSet;

         function VersionInfoText : String;

         property DB : TSQLDatabase read FDB;
         property Module[const name : String] : TObject read GetModule write SetModule;

         function OptionList : TStringDynArray; override;
         function GetOption(const name : String) : String; override;
         procedure SetOption(const name, value : String); override;
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
         constructor Create(db : TdwsSynSQLiteDataBase; const sql : String; const parameters : IDataContext);
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
         procedure AsString(var Result : String); override;
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

var
   vOnNeedSQLite3DynamicDLLName : function : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsSynSQLiteFunctions;

var
   vSQLite3DynamicMRSW : TMultiReadSingleWrite;
   vFloatFormatSettings : TFormatSettings;

procedure InitializeSQLite3Dynamic;

   procedure DoInitialize;
   var
      dllName : String;
   begin
      vSQLite3DynamicMRSW.BeginWrite;
      try
         if sqlite3=nil then begin
            if Assigned(vOnNeedSQLite3DynamicDLLName) then
               dllName:=vOnNeedSQLite3DynamicDLLName;
            if dllName<>'' then
               sqlite3:=TSQLite3LibraryDynamic.Create(dllName)
            else sqlite3:=TSQLite3LibraryDynamic.Create;
         end;
      finally
         vSQLite3DynamicMRSW.EndWrite;
      end;
   end;

begin
   vSQLite3DynamicMRSW.BeginRead;
   try
      if sqlite3<>nil then Exit;
   finally
      vSQLite3DynamicMRSW.EndRead;
   end;
   DoInitialize;
end;

// SQLAssignParameters
//
procedure SQLAssignParameters(var rq : TSQLRequest; const params : IDataContext);

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
   for i:=1 to params.DataLength do begin
      p:=PVarData(params.AsPVariant(i-1));
      case p.VType of
         varInt64 : rq.Bind(i, p.VInt64);
         varDouble : rq.Bind(i, p.VDouble);
         {$ifndef FPC}
         varUString : rq.BindS(i, String(p.VUString));
         {$endif}
         varBoolean : rq.Bind(i, Ord(p.VBoolean));
         varNull : rq.BindNull(i);
         varString : rq.Bind(i, p.VString, Length(RawByteString(p.VString)));
         varDate : BindDateTime(rq, i, p);
      else
         raise EDWSDataBase.CreateFmt('Unsupported parameter type (VarType %d) at index %d', [p.VType, i]);
      end;
   end;
end;

type
   TdwsSQLDatabase = class (TSQLDatabase)
      function DBOpen: integer; override;
   end;

// DBOpen
//
function TdwsSQLDatabase.DBOpen : Integer;
const
   SQLITE_DETERMINISTIC = $800;
begin
   Result := inherited DBOpen;
   if result = SQLITE_OK then begin
      sqlite3.create_function(DB, 'SQRT', 1, SQLITE_ANY or SQLITE_DETERMINISTIC, nil, SQLiteFunc_Sqrt, nil, nil);
      sqlite3.create_function(DB, 'MEDIAN', 1, SQLITE_ANY or SQLITE_DETERMINISTIC, nil, nil, SQLiteFunc_MedianStep, SQLiteFunc_MedianFinal);
      sqlite3.create_function(DB, 'BOOL_AND', 1, SQLITE_ANY or SQLITE_DETERMINISTIC, nil, nil, SQLiteFunc_BoolAndStep, SQLiteFunc_BoolFinal);
      sqlite3.create_function(DB, 'BOOL_OR', 1, SQLITE_ANY or SQLITE_DETERMINISTIC, nil, nil, SQLiteFunc_BoolOrStep, SQLiteFunc_BoolFinal);
      sqlite3.create_function(DB, 'BIT_AND', 1, SQLITE_ANY or SQLITE_DETERMINISTIC, nil, nil, SQLiteFunc_BitAndStep, SQLiteFunc_BitFinal);
      sqlite3.create_function(DB, 'BIT_OR', 1, SQLITE_ANY or SQLITE_DETERMINISTIC, nil, nil, SQLiteFunc_BitOrStep, SQLiteFunc_BitFinal);
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
         flags:=flags or SQLITE_OPEN_SHAREDCACHE
      else if UnicodeSameText(parameters[i], 'open_uri') then
         flags:=flags or SQLITE_OPEN_URI;
   end;

   try
      FDB := TdwsSQLDatabase.Create(dbName, '', flags);
      FDB.BusyTimeout := 1500;
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynSQLiteDataBase.Destroy;
begin
   FModules.Free;
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
procedure TdwsSynSQLiteDataBase.Exec(const sql : String; const parameters : IDataContext; context : TExprBase);
var
   err : Integer;
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
      err := FExecRequest.Reset;
      if err <> SQLITE_OK then
         raise Exception.CreateFmt('Statement Reset failed (%d)', [err]);
      FExecRequest.BindReset;
   except
      FExecRequest.Close;
      raise;
   end;
end;

// Query
//
function TdwsSynSQLiteDataBase.Query(const sql : String; const parameters : IDataContext; context : TExprBase) : IdwsDataSet;
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

// GetModule
//
function TdwsSynSQLiteDataBase.GetModule(const name : String) : TObject;
begin
   if FModules <> nil then
      Result := FModules.Objects[name]
   else Result := nil;
end;

// SetModule
//
procedure TdwsSynSQLiteDataBase.SetModule(const name : String; aModule : TObject);
begin
   if FModules = nil then begin
      FModules := TNameObjectHash.Create;
   end else if FModules.Objects[name] <> nil then
      raise Exception.CreateFmt('Module "%s" already registered', [name]);
   FModules.Objects[name] := aModule;
end;

const
   cSQLiteOptions : array [0..0] of String = (
      'soft_heap_limit64'
   );

// OptionList
//
function TdwsSynSQLiteDataBase.OptionList : TStringDynArray;
var
   n, i : Integer;
begin
   Result := inherited OptionList;
   n := Length(Result);
   SetLength(Result, n + Length(cSQLiteOptions));
   for i := 0 to High(cSQLiteOptions) do
      Result[n+i] := cSQLiteOptions[i];
end;

// GetOption
//
function TdwsSynSQLiteDataBase.GetOption(const name : String) : String;
begin
   if name = cSQLiteOptions[0] then
      Result := IntToStr(sqlite3.soft_heap_limit64(-1))
   else Result := inherited GetOption(name);
end;

// SetOption
//
procedure TdwsSynSQLiteDataBase.SetOption(const name, value : String);
begin
   if name = cSQLiteOptions[0] then
      sqlite3.soft_heap_limit64(StrToInt64(value))
   else inherited SetOption(name, value);
end;

// ------------------
// ------------------ TdwsSynSQLiteDataSet ------------------
// ------------------

// Create
//
constructor TdwsSynSQLiteDataSet.Create(db : TdwsSynSQLiteDataBase; const sql : String; const parameters : IDataContext);
begin
   FSQL := sql;
   FDB := db;

   inherited Create(db);

   try
      FRequest.Prepare(db.FDB.DB, StringToUTF8(sql));
      try
         Assert(FRequest.Request<>0);
         SQLAssignParameters(FRequest, parameters);
         FEOFReached:=(FRequest.Step=SQLITE_DONE);
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
   FDataSet := dataSet;
   inherited Create(dataSet, fieldIndex);
end;

// DataType
//
function TdwsSynSQLiteDataField.DataType : TdwsDataFieldType;
begin
   Result := GetDataType;
end;

// IsNull
//
function TdwsSynSQLiteDataField.IsNull : Boolean;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result := FDataSet.FRequest.FieldNull(Index);
end;

// GetName
//
function TdwsSynSQLiteDataField.GetName : String;
begin
   Result := UTF8ToString(FDataSet.FRequest.FieldName(Index));
end;

// GetDataType
//
function TdwsSynSQLiteDataField.GetDataType : TdwsDataFieldType;
var
   sqlt : Integer;
   r : RawUTF8;
begin
   sqlt := FDataSet.FRequest.FieldType(Index);
   case sqlt of
      SQLITE_INTEGER : begin
         r := FDataSet.FRequest.FieldDeclaredType(Index);
         if (Length(r) >= 4) and (r[1] in ['B', 'b'])
                             and (r[2] in ['O', 'o'])
                             and (r[3] in ['O', 'o'])
                             and (r[4] in ['L', 'l']) then
            Result := dftBoolean
         else if (Length(r) >= 4) and (r[1] in ['D', 'd'])
                             and (r[2] in ['A', 'a'])
                             and (r[3] in ['T', 't'])
                             and (r[4] in ['E', 'e']) then
            Result := dftDateTime
         else Result := dftInteger;
      end;
      SQLITE_FLOAT : begin
         r := FDataSet.FRequest.FieldDeclaredType(Index);
         if (Length(r) >= 4) and (r[1] in ['D', 'd'])
                             and (r[2] in ['A', 'a'])
                             and (r[3] in ['T', 't'])
                             and (r[4] in ['E', 'e']) then
            Result := dftDateTime
         else Result := dftFloat;
      end;
      SQLITE_TEXT : Result := dftString;
      SQLITE_BLOB : Result := dftBlob;
      SQLITE_NULL : Result := dftNull;
   else
      raise EDWSDataBase.CreateFmt('Unknown field SQLite type %d', [sqlt]);
   end;
end;

// GetDeclaredType
//
function TdwsSynSQLiteDataField.GetDeclaredType : String;
begin
   Result := FDataSet.FRequest.FieldDeclaredTypeS(Index);
end;

// AsString
//
procedure TdwsSynSQLiteDataField.AsString(var Result : String);
var
   rq : TSQLite3Statement;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   rq := FDataSet.FRequest.Request;
   case sqlite3.column_type(rq, Index) of
      SQLITE_INTEGER :
         FastInt64ToStr(sqlite3.column_int64(rq, Index), Result);
      SQLITE_FLOAT :
         FastFloatToStr(sqlite3.column_double(rq, Index), Result, vFloatFormatSettings);
      SQLITE_NULL :
         Result := '';
   else
      Result := sqlite3.column_text16(rq, Index);
   end;
end;

// AsInteger
//
function TdwsSynSQLiteDataField.AsInteger : Int64;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result := FDataSet.FRequest.FieldInt(Index);
end;

// AsFloat
//
function TdwsSynSQLiteDataField.AsFloat : Double;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result := FDataSet.FRequest.FieldDouble(Index);
end;

// AsBlob
//
function TdwsSynSQLiteDataField.AsBlob : RawByteString;
begin
   if FDataSet.FEOFReached then
      RaiseNoActiveRecord;
   Result := FDataSet.FRequest.FieldBlob(Index);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsDatabase.RegisterDriver('SQLite', TdwsSynSQLiteDataBaseFactory.Create);

   vSQLite3DynamicMRSW:=TMultiReadSingleWrite.Create;

   vFloatFormatSettings := FormatSettings;
   vFloatFormatSettings.DecimalSeparator := '.';

finalization

   FreeAndNil(vSQLite3DynamicMRSW);

end.
