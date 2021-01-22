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
unit dwsPostgreSQLDatabase;

interface

{$define POSTGRES_AUTO_PREPARE}

uses
   Classes, SysUtils,
   dwsUtils,  dwsDatabase, dwsXPlatform,
   dwsDataContext, dwsSymbols, dwsExprs,
   dwsLibpq;

type

   TdwsPostgreSQLDataSet = class;

   TdwsPostgreSQLPreparedQuery = record
      SQL : RawByteString;
      ParamTypes : TPGParamTypes;
      Name : RawByteString;
   end;

   TdwsPostgreSQLPreparedQueries = class (TSimpleHash<TdwsPostgreSQLPreparedQuery>)
      protected
         FTemp : TdwsPostgreSQLPreparedQuery;

         function SameItem(const item1, item2 : TdwsPostgreSQLPreparedQuery) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsPostgreSQLPreparedQuery) : Cardinal; override;
   end;

   TdwsPostgreSQLDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FConn : PGconn;
         FInTransaction : Boolean;
         FDataSets : Integer;

         FPreparedQueries : TdwsPostgreSQLPreparedQueries;

      protected
         procedure InternalExecute(const command : RawByteString; ignoreError : Boolean = False);
         procedure RaiseLastPGError;

         function PreparedQueryFor(const sql : RawByteString; const paramTypes : TPGParamTypes; var pgRes : PGresult) : RawByteString;

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

   TdwsPostgreSQLDataSetMode = (
      dsmAutomatic,
      dsmForceArray,
      dsmForceCursor
   );

   TdwsPostgreSQLDataSet = class (TdwsDataSet)
      private
         FDB : TdwsPostgreSQLDataBase;
         FSQL : RawByteString;
         FCursorName : RawByteString;
         FFetchNext : RawByteString;
         FResultRowCount : Integer;
         FResultRowNum : Integer;
         FResult : PGresult;

      protected
         procedure DoPrepareFields; override;

         procedure CleanupResult;
         procedure CloseCursor;
         procedure InternalFetchNext;

         function GetType(index : Integer) : Oid;
         function GetIsNull(index : Integer) : Boolean;
         function GetPValue(index : Integer) : Pointer;
         function GetLength(index : Integer) : Integer;

      public
         constructor Create(db : TdwsPostgreSQLDataBase; const sql : String; const parameters : IScriptDynArray;
                            aMode : TdwsPostgreSQLDataSetMode = dsmAutomatic);
         destructor Destroy; override;

         function Eof : Boolean; override;
         procedure Next; override;

         property SQL : RawByteString read FSQL;
   end;

   TdwsPostgreSQLDataField = class (TdwsDataField)
      private
         FName : String;
         FDataSet : TdwsPostgreSQLDataSet;

      protected
         procedure RaiseUnsupportedConversion(const asType : String);

         function GetName : String; override;
         function GetDataType : TdwsDataFieldType; override;
         function GetDeclaredType : String; override;

      public
         constructor Create(dataSet : TdwsPostgreSQLDataSet; fieldIndex : Integer);

         function DataType : TdwsDataFieldType; override;

         function IsNull : Boolean; override;
         procedure AsString(var Result : String); override;
         function AsInteger : Int64; override;
         function AsFloat : Double; override;
         function AsBoolean : Boolean; override;
         function AsBlob : RawByteString; override;
   end;

   TdwsPostgreSQLDataBaseFactory = class (TdwsDataBaseFactory)
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

uses dwsRandom, dwsXXHash;

const
   cInitialFetch = 'FETCH 2 in ';
   cNoBlobFetch = 'FETCH 500 in ';

var
   vLibpq : TdwsLibpq;
   vLibpqLock : TMultiReadSingleWrite;
   vNameGenerator : TXoroShiro128Plus;
   vBooleanTrue : RawByteString = '1';
   vBooleanFalse : RawByteString = '0';

// Generate pseudo-random names for use as cursor names, prepared query names, etc.
procedure NewPGName(var result : RawByteString);
const
   cNBDigits = 8; // 8 x 5 = 40 bits
var
   i, c : Integer;
   k : UInt64;
   buf : PAnsiChar;
begin
   vLibpqLock.BeginWrite;
   k := vNameGenerator.Next;
   vLibpqLock.EndWrite;

   SetLength(Result, cNBDigits+1);
   buf := Pointer(Result);
   buf[0] := '_';
   for i := 1 to cNBDigits do begin
      c := PByte(@k)^ and 31;
      if c <= 9 then
         buf[i] := AnsiChar(Ord('0') + c)
      else buf[i] := AnsiChar((Ord('a')-10) + c);
      k := k shr 5;
   end;
end;

type
   TPosgreSQLParams = record
      Length : Integer;
      Types : TPGParamTypes;
      Values : TPGParamValues;
      Lengths : TPGParamLengths;
      Formats : TPGParamFormats;
   end;

procedure PreparePGParams(const data : IScriptDynArray; var params : TPosgreSQLParams);
var
   i, n : Integer;
   v : Variant;
   p : PVarData;
   dt : Int64;
begin
   n := data.ArrayLength;
   params.Length := n;
   SetLength(params.Types, n);
   SetLength(params.Values, n);
   SetLength(params.Lengths, n);
   SetLength(params.Formats, n);
   for i := 0 to n-1 do begin
      data.EvalAsVariant(i, v);
      p := @v;
      case p.VType of
         varInt64 : begin
            if Int32(p.VInt64) = p.VInt64 then begin
               params.Types[i] := INT4OID;
               SetLength(params.Values[i], 4);
               PCardinal(params.Values[i])^ := SwapBytes(p.VLongWord);
               params.Lengths[i] := 4;
               params.Formats[i] := 1;
            end else begin
               params.Types[i] := INT8OID;
               SetLength(params.Values[i], 8);
               SwapInt64(@p.VInt64, Pointer(params.Values[i]));
               params.Lengths[i] := 8;
               params.Formats[i] := 1;
            end;
         end;
         varDouble : begin
            params.Types[i] := FLOAT8OID;
            SetLength(params.Values[i], SizeOf(Double));
            SwapInt64(Pointer(@p.VDouble), Pointer(params.Values[i]));
            params.Lengths[i] := SizeOf(Double);
            params.Formats[i] := 1;
         end;
         varUString : begin
            params.Types[i] := VARCHAROID;
            // nil is recognized as null,  an empty string is a non-nil pointer of length zero
            if p.VUString = nil then begin
               params.Values[i] := vBooleanFalse;
               params.Lengths[i] := 0;
            end else begin
               params.Values[i] := UTF8Encode(String(p.VUString));
               params.Lengths[i] := Length(params.Values[i]);
            end;
            params.Formats[i] := 1;
         end;
         varBoolean : begin
            params.Types[i] := BOOLOID;
            if p.VBoolean then
               params.Values[i] := vBooleanTrue
            else params.Values[i] := vBooleanFalse;
            params.Lengths[i] := 1;
            params.Formats[i] := 0;
         end;
         varNull : begin
            params.Types[i] := 0;
            params.Values[i] := '';
            params.Lengths[i] := 0;
            params.Formats[i] := 1;
         end;
         varString : begin
            params.Types[i] := BYTEAOID;
            params.Values[i] := RawByteString(p.VString);
            params.Lengths[i] := Length(params.Values[i]);
            params.Formats[i] := 1;
         end;
         varDate : begin
            params.Types[i] := TIMESTAMPOID;
            // Double precision has 15 decimal places,
            // for current date/times we have 5 for the day, 5 for seconds, 3 for milliseconds,
            // this leaves only 2 for microseconds, so a precision of 10 microseconds
            dt := Round(p.VDate*86400e5)*10 - Round(cPostgresEpochDeltaDays * 86400e6);
            SetLength(params.Values[i], SizeOf(Int64));
            SwapInt64(@dt, Pointer(params.Values[i]));
            params.Lengths[i] := SizeOf(Int64);
            params.Formats[i] := 1;
         end;
      else
         raise EDWSDataBase.CreateFmt('Unsupported parameter type (VarType %d) at index %d', [p.VType, i]);
      end;
   end;
end;

// ------------------
// ------------------ TdwsPostgreSQLPreparedQueries ------------------
// ------------------

// SameItem
//
function TdwsPostgreSQLPreparedQueries.SameItem(const item1, item2 : TdwsPostgreSQLPreparedQuery) : Boolean;
begin
   Result := (item1.SQL = item2.SQL) and PostgreSQLSameParamTypes(item1.ParamTypes, item2.ParamTypes);
end;

// GetItemHashCode
//
function TdwsPostgreSQLPreparedQueries.GetItemHashCode(const item1 : TdwsPostgreSQLPreparedQuery) : Cardinal;
var
   hash : xxHash32;
begin
   hash.Init;
   hash.Update(Pointer(item1.SQL), Length(item1.SQL)*SizeOf(AnsiChar));
   hash.Update(Pointer(item1.ParamTypes), Length(item1.ParamTypes)*SizeOf(Oid));
   Result := hash.Digest;
   if Result = 0 then Result := 1;
end;

// ------------------
// ------------------ TdwsPostgreSQLDataBase ------------------
// ------------------

// Create
//
constructor TdwsPostgreSQLDataBase.Create(const parameters : array of String);
begin
   inherited Create;

   try
      if Length(parameters) = 0 then
         raise EDWSDataBase.Create('Missing PostgreSQL connection info');
      FConn := vLibpq.PQconnectdb(PAnsiChar(UTF8Encode(parameters[0])));
   except
      RefCount := 0;
      raise;
   end;

   {$ifdef POSTGRES_AUTO_PREPARE }
   FPreparedQueries := TdwsPostgreSQLPreparedQueries.Create;
   {$endif}
end;

// Destroy
//
destructor TdwsPostgreSQLDataBase.Destroy;
begin
   if (FConn <> nil) and (vLibpq <> nil) then begin
      vLibpq.PQfinish(FConn);
      FConn := nil;
   end;
   FreeAndNil(FPreparedQueries);
   inherited;
end;

// InternalExecute
//
procedure TdwsPostgreSQLDataBase.InternalExecute(const command : RawByteString; ignoreError : Boolean = False);
var
   res : PGresult;
begin
   res := vLibpq.PQexec(FConn, PAnsiChar(command));
   try
      case vLibpq.PQresultStatus(res) of
         PGRES_COMMAND_OK, PGRES_EMPTY_QUERY : ; // no result
         PGRES_TUPLES_OK : ; // full results, discarded
      else
         if not ignoreError then
            raise EDWSDataBase.CreateFmt('Failed internal command: %s', [command]);
      end;
   finally
      vLibpq.PQclear(res);
   end;
end;

// RaiseLastPGError
//
procedure TdwsPostgreSQLDataBase.RaiseLastPGError;
var
   msg : String;
begin
   msg := UTF8ToString(vLibpq.PQerrorMessage(FConn));
   raise EDWSDataBase.Create(msg);
end;

// TdwsPostgreSQLDataBase.PreparedQueryFor
//
function TdwsPostgreSQLDataBase.PreparedQueryFor(const sql : RawByteString; const paramTypes : TPGParamTypes; var pgRes : PGresult) : RawByteString;
begin
   FPreparedQueries.FTemp.SQL := sql;
   FPreparedQueries.FTemp.ParamTypes := paramTypes;
   if FPreparedQueries.Match(FPreparedQueries.FTemp) then
      Result := FPreparedQueries.FTemp.Name
   else begin
      NewPGName(FPreparedQueries.FTemp.Name);
      pgRes := vLibpq.PQprepare(
         FConn, Pointer(FPreparedQueries.FTemp.Name), Pointer(sql),
         Length(paramTypes), paramTypes
      );
      if vLibpq.PQresultStatus(pgRes) = PGRES_COMMAND_OK then begin
         FPreparedQueries.Add(FPreparedQueries.FTemp);
         Result := FPreparedQueries.FTemp.Name;
      end else Result := '';
   end;
end;

// BeginTransaction
//
procedure TdwsPostgreSQLDataBase.BeginTransaction;
begin
   if FInTransaction then
      raise EDWSDataBase.Create('Already in transaction');
   InternalExecute('BEGIN ISOLATION LEVEL REPEATABLE READ;SELECT');
   FInTransaction := True;
end;

// Commit
//
procedure TdwsPostgreSQLDataBase.Commit;
begin
   if not FInTransaction then
      raise EDWSDataBase.Create('Not in transaction');
   FInTransaction := False;
   InternalExecute('COMMIT');
end;

// Rollback
//
procedure TdwsPostgreSQLDataBase.Rollback;
begin
   if not FInTransaction then
      raise EDWSDataBase.Create('Not in transaction');
   FInTransaction := False;
   InternalExecute('ROLLBACK');
end;

// InTransaction
//
function TdwsPostgreSQLDataBase.InTransaction : Boolean;
begin
   Result := FInTransaction;
end;

// CanReleaseToPool
//
function TdwsPostgreSQLDataBase.CanReleaseToPool : String;
begin
   if FInTransaction then
      Result := 'in transaction'
   else if FDataSets > 0 then
      Result := 'has opened datasets'
   else Result := '';
end;

// Exec
//
procedure TdwsPostgreSQLDataBase.Exec(const sql : String; const parameters : IScriptDynArray; context : TExprBase);
var
   query : RawByteString;

   function PQExecParams : PGresult;
   var
      params : TPosgreSQLParams;
      prepName : RawByteString;
   begin
      PreparePGParams(parameters, params);

      if FPreparedQueries <> nil then begin
         prepName := PreparedQueryFor(query, params.Types, Result);
         if prepName <> '' then begin
            Result := vLibpq.PQexecPrepared(
               FConn, Pointer(prepName),
               params.Length,params.Values, params.Lengths, params.Formats,
               1
            );
         end;
      end else begin
         Result := vLibpq.PQexecParams(
            FConn, PAnsiChar(query),
            params.Length, params.Types, params.Values, params.Lengths, params.Formats,
            1
         );
      end;
   end;

var
   res : PGresult;
begin
   query := UTF8Encode(sql);

   if parameters <> nil then
      res := PQExecParams
   else begin
      res := vLibpq.PQexecParams(
         FConn, PAnsiChar(query),
         0, nil, nil, nil, nil,
         1
      );
   end;

   try
      case vLibpq.PQresultStatus(res) of
         PGRES_COMMAND_OK, PGRES_EMPTY_QUERY : ; // no result
         PGRES_TUPLES_OK : ; // full results, discarded
      else
         RaiseLastPGError;
      end;
   finally
      vLibpq.PQclear(res);
   end;
end;

// Query
//
function TdwsPostgreSQLDataBase.Query(const sql : String; const parameters : IScriptDynArray; context : TExprBase) : IdwsDataSet;
var
   ds : TdwsPostgreSQLDataSet;
begin
   ds := TdwsPostgreSQLDataSet.Create(Self, sql, parameters);
   Result := ds;
end;

// VersionInfoText
//
function TdwsPostgreSQLDataBase.VersionInfoText : String;
var
   ds : TdwsPostgreSQLDataSet;
   i : IdwsDataSet;
begin
   ds := TdwsPostgreSQLDataSet.Create(Self, 'SELECT version()', nil, dsmForceArray);
   i := ds;
   i.GetField(0).GetAsString(Result);
end;

// ------------------
// ------------------ TdwsPostgreSQLDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsPostgreSQLDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsPostgreSQLDataBase;
begin
   if vLibpq = nil then begin
      vLibpqLock.BeginWrite;
      try
         vLibpq := TdwsLibpq.Create;
      finally
         vLibpqLock.EndWrite;
      end;
   end;

   db := TdwsPostgreSQLDataBase.Create(parameters);
   Result := db;
end;

// ------------------
// ------------------ TdwsPostgreSQLDataSet ------------------
// ------------------

// Create
//
constructor TdwsPostgreSQLDataSet.Create(
      db : TdwsPostgreSQLDataBase;
      const sql : String; const parameters : IScriptDynArray;
      aMode : TdwsPostgreSQLDataSetMode = dsmAutomatic
      );

   function PQExecParams : PGresult;
   var
      params : TPosgreSQLParams;
   begin
      PreparePGParams(parameters, params);
      Result := vLibpq.PQexecParams(
            FDB.FConn, PAnsiChar(FSQL),
            params.Length, params.Types, params.Values, params.Lengths, params.Formats,
            1
         );
   end;

var
   res : PGresult;
   status : TPGExecStatusType;
begin
   FDB := db;
   Inc(FDB.FDataSets);

   inherited Create(db);

   try
      FSQL := UTF8Encode(sql);

      if (aMode = dsmForceCursor) or (FDB.InTransaction and (aMode = dsmAutomatic)) then begin
         // within transactions, we use cursors by default for selects
         // they can be aborted mid-way and support multiple large blobs
         if StrIBeginsWith(sql, 'select')  then begin
            NewPGName(FCursorName);
            FSQL := 'DECLARE ' + FCursorName + ' NO SCROLL CURSOR FOR ' + FSQL;
            FFetchNext := cInitialFetch + FCursorName;
         end;
      end;

      if parameters <> nil then begin
         res := PQExecParams;
      end else begin
         res := vLibpq.PQexecParams(
            FDB.FConn, PAnsiChar(FSQL),
            0, nil, nil, nil, nil,
            1
         );
      end;

      try
         status := vLibpq.PQresultStatus(res);
         case status of
            PGRES_COMMAND_OK : begin
               if FFetchNext <> '' then
                  InternalFetchNext;

            end;
            PGRES_TUPLES_OK, PGRES_EMPTY_QUERY : begin
               FResult := res;
               res := nil;
               FResultRowCount := vLibpq.PQntuples(FResult);
            end;
         else
            FCursorName := '';
            FFetchNext := '';
            FDB.RaiseLastPGError;
         end;
      finally
         vLibpq.PQclear(res);
      end;
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsPostgreSQLDataSet.Destroy;
begin
   Dec(FDB.FDataSets);
   CleanupResult;
   CloseCursor;
   inherited;
end;

// Eof
//
function TdwsPostgreSQLDataSet.Eof : Boolean;
begin
   Result := (FResultRowNum >= FResultRowCount) and (FFetchNext = '');
end;

// Next
//
procedure TdwsPostgreSQLDataSet.Next;
begin
   if FResultRowNum >= FResultRowCount then
      raise EDWSDataBase.Create('EOF already reached');
   Inc(FResultRowNum);
   if FResultRowNum >= FResultRowCount then begin
      if FFetchNext <> '' then
         InternalFetchNext;
   end;
end;

// CleanupResult
//
procedure TdwsPostgreSQLDataSet.CleanupResult;
begin
   if FResult <> nil then begin
      vLibpq.PQclear(FResult);
      FResult := nil;
      FResultRowNum := 0;
      FResultRowCount := 0;
   end;
end;

// CloseCursor
//
procedure TdwsPostgreSQLDataSet.CloseCursor;

   procedure DoClose;
   var
      sql : RawByteString;
   begin
      sql := 'CLOSE ' + FCursorName;
      FCursorName := '';
      FDB.InternalExecute(sql, True);
   end;

begin
   if FCursorName <> '' then
      DoClose;
end;

// InternalFetchNext
//
procedure TdwsPostgreSQLDataSet.InternalFetchNext;
var
   status : TPGExecStatusType;
begin
   Assert(FFetchNext <> '');

   CleanupResult;

   FResult := vLibpq.PQexecParams(FDB.FConn, PAnsiChar(FFetchNext),
                                  0, nil, nil, nil, nil, 1);
   try
      status := vLibpq.PQresultStatus(FResult);
      case status of
         PGRES_COMMAND_OK :
            FFetchNext := '';
         PGRES_TUPLES_OK : ;
         PGRES_FATAL_ERROR : begin
            FFetchNext := '';
            FDB.RaiseLastPGError;
         end;
      else
         FDB.RaiseLastPGError;
      end;
   except
      CleanupResult;
      raise;
   end;

   FResultRowCount := vLibpq.PQntuples(FResult);
   if FResultRowCount = 0 then
      FFetchNext := '';

   if FFetchNext = '' then
      CloseCursor;
end;

// GetType
//
function TdwsPostgreSQLDataSet.GetType(index : Integer) : Oid;
begin
   Result := vLibpq.PQftype(FResult, index);
end;

// GetIsNull
//
function TdwsPostgreSQLDataSet.GetIsNull(index : Integer) : Boolean;
begin
   Result := (vLibpq.PQgetisnull(FResult, FResultRowNum, index) <> 0);
end;

// GetPValue
//
function TdwsPostgreSQLDataSet.GetPValue(index : Integer) : Pointer;
begin
   Result := vLibpq.PQgetvalue(FResult, FResultRowNum, index);
end;

// GetLength
//
function TdwsPostgreSQLDataSet.GetLength(index : Integer) : Integer;
begin
   Result := vLibpq.PQgetlength(FResult, FResultRowNum, index);
end;

// DoPrepareFields
//
procedure TdwsPostgreSQLDataSet.DoPrepareFields;
var
   i, n : Integer;
   noBlobs : Boolean;
begin
   noBlobs := True;
   n := vLibpq.PQnfields(FResult);
   SetLength(FFields, n);
   for i := 0 to n-1 do begin
      FFields[i] := TdwsPostgreSQLDataField.Create(Self, i);
      if noBlobs and (FFields[i].DataType = dftBlob) then
         noBlobs := false;
   end;

   if noBlobs and (FFetchNext <> '') then begin
      // if there is more data, and none of the fields are blobs, increase fetch size
      FFetchNext := cNoBlobFetch + FCursorName;
   end;

end;

// ------------------
// ------------------ TdwsPostgreSQLDataField ------------------
// ------------------

// Create
//
constructor TdwsPostgreSQLDataField.Create(dataSet : TdwsPostgreSQLDataSet; fieldIndex : Integer);
begin
   FDataSet := dataSet;
   inherited Create(dataSet, fieldIndex);
   FName := UTF8ToString(vLibpq.PQfname(dataSet.FResult, fieldIndex));
end;


// RaiseUnsupportedConversion
//
procedure TdwsPostgreSQLDataField.RaiseUnsupportedConversion(const asType : String);
begin
   raise EDWSDataBase.CreateFmt('Unsupported conversion to %s of field "%s" from OID %d',
                                [asType, FName, FDataSet.GetType(Index)]);
end;

// DataType
//
function TdwsPostgreSQLDataField.DataType : TdwsDataFieldType;
begin
   Result := GetDataType;
end;

// GetName
//
function TdwsPostgreSQLDataField.GetName : String;
begin
   Result := FName;
end;

// GetDataType
//
function TdwsPostgreSQLDataField.GetDataType : TdwsDataFieldType;
begin
   case FDataSet.GetType(Index) of
      0 :
         Result := dftNull;
      BOOLOID :
         Result := dftBoolean;
      INT2OID, INT4OID, INT8OID, OIDOID, REGPROCOID :
         Result := dftInteger;
      CHAROID, NAMEOID, TEXTOID, VARCHAROID, BPCHAROID, PGNODETREEOID, UNKNOWNOID, JSONOID, XMLOID :
         Result := dftString;
      FLOAT4OID, FLOAT8OID, NUMERICOID :
         Result := dftFloat;
      TIMESTAMPOID :
         Result := dftDateTime;
      BYTEAOID :
         Result := dftBlob;
   else
      Result := dftUnknown;
   end;
end;

// GetDeclaredType
//
function TdwsPostgreSQLDataField.GetDeclaredType : String;
begin
   Result := Format('OID(%d)', [vLibpq.PQftype(FDataSet.FResult, Index)]);

end;

// IsNull
//
function TdwsPostgreSQLDataField.IsNull : Boolean;
begin
   Result := FDataSet.GetIsNull(Index);
end;

// AsString
//
procedure TdwsPostgreSQLDataField.AsString(var Result : String);
var
   oid : Integer;
   p : Pointer;
   buf : RawByteString;
begin
   if FDataSet.GetIsNull(Index) then begin
      Result := '';
      Exit;
   end;

   p := FDataSet.GetPValue(Index);
   oid := FDataSet.GetType(Index);
   case oid of
      CHAROID, NAMEOID, TEXTOID, VARCHAROID, BPCHAROID, PGNODETREEOID, UNKNOWNOID, JSONOID, XMLOID : begin
         SetString(buf, PAnsiChar(p), FDataSet.GetLength(Index));
         Result := UTF8ToUnicodeString(buf);
      end;
      BOOLOID :
         if PByte(p)^ <> 0 then
            Result := 'true'
         else Result := 'false';
   else
      case GetDataType of
         dftInteger : Result := IntToStr(AsInteger);
         dftFloat : Result := FloatToStr(AsFloat);
         dftDateTime : Result := DateTimeToISO8601(AsFloat, True);
         dftBlob : RawByteStringToScriptString(AsBlob, Result);
      else
         // unknown
         SetString(buf, PAnsiChar(p), FDataSet.GetLength(Index));
         RawByteStringToScriptString(buf, Result);
      end;
   end;
end;

// AsInteger
//
function TdwsPostgreSQLDataField.AsInteger : Int64;

   function Fallback : Int64;
   var
      s : String;
   begin
      case DataType of
         dftFloat : Result := Trunc(AsFloat);
         dftString : begin
            GetAsString(s);
            Result := StrToInt64(s);
         end;
         dftDateTime : Result := Round((AsFloat+25596)*86400);
      else
         RaiseUnsupportedConversion('Integer');
         Result := 0;
      end;
   end;

var
   oid : Integer;
   p : Pointer;
begin
   Result := 0;
   if FDataSet.GetIsNull(Index) then Exit;

   p := FDataSet.GetPValue(Index);
   oid := FDataSet.GetType(Index);
   case oid of
      INT2OID : begin
         Result := SmallInt((PByteArray(p)[0] shl 8) + PByteArray(p)[1]);
      end;
      INT4OID, OIDOID, REGPROCOID : begin
         Result := Integer(SwapBytes(PCardinal(p)^));
      end;
      INT8OID :
         SwapInt64(p, @Result);
      BOOLOID :
         Result := Ord(PByte(p)^ <> 0);
   else
      Result := Fallback;
   end;
end;

// AsFloat
//
function TdwsPostgreSQLDataField.AsFloat : Double;
var
   oid : Integer;
   p : Pointer;
   s : Single;
   dt : Int64;
begin
   Result := 0;
   if FDataSet.GetIsNull(Index) then Exit;

   p := FDataSet.GetPValue(Index);
   oid := FDataSet.GetType(Index);
   case oid of
      FLOAT4OID : begin
         PCardinal(@s)^ := SwapBytes(PCardinal(p)^);
         Result := s;
      end;
      FLOAT8OID : begin
         SwapInt64(p, @Result);
      end;
      NUMERICOID : begin
         Result := PostgreSQLNumericToFloat(p);
      end;
      TIMESTAMPOID : begin
         SwapInt64(p, @dt);
         Result := dt / 86400e6 + cPostgresEpochDeltaDays;
      end;
   else
      RaiseUnsupportedConversion('Float');
   end;
end;

// AsBoolean
//
function TdwsPostgreSQLDataField.AsBoolean : Boolean;
var
   oid : Integer;
   p : Pointer;
begin
   Result := False;
   if FDataSet.GetIsNull(Index) then Exit;

   p := FDataSet.GetPValue(Index);
   oid := FDataSet.GetType(Index);
   case oid of
      BOOLOID :
         Result := (PByte(p)^ <> 0);
      INT2OID :
         Result := (PWord(p)^ <> 0);
      INT4OID, OIDOID, REGPROCOID :
         Result := (PCardinal(p)^ <> 0);
      INT8OID :
         Result := (PUInt64(p)^ <> 0);
   else
      RaiseUnsupportedConversion('Boolean');
   end;
end;

// AsBlob
//
function TdwsPostgreSQLDataField.AsBlob : RawByteString;
var
   oid : Integer;
   p : Pointer;
   n : Integer;
begin
   if FDataSet.GetIsNull(Index) then Exit('');

   oid := FDataSet.GetType(Index);
   case oid of
      BYTEAOID : begin
         n := FDataSet.GetLength(Index);
         SetLength(Result, n);
         if n > 0 then begin
            p := FDataSet.GetPValue(Index);
            System.Move(p^, Pointer(Result)^, n);
         end;
      end;
   else
      RaiseUnsupportedConversion('Blob');
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vNameGenerator.Randomize;
   vLibpqLock := TMultiReadSingleWrite.Create;
   TdwsDatabase.RegisterDriver('PostgreSQL', TdwsPostgreSQLDataBaseFactory.Create);

finalization

   FreeAndNil(vLibpqLock);
   FreeAndNil(vLibpq);

end.
