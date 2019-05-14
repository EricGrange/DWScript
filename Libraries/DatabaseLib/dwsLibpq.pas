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
unit dwsLibpq;

interface

uses
   Windows, Classes, SysUtils;

type

   PGconn = Pointer;
   PGresult = Pointer;

   TPGExecStatusType = (
      PGRES_EMPTY_QUERY,
      PGRES_COMMAND_OK,
      PGRES_TUPLES_OK,
      PGRES_COPY_OUT,
      PGRES_COPY_IN,
      PGRES_BAD_RESPONSE,
      PGRES_NONFATAL_ERROR,
      PGRES_FATAL_ERROR,
      PGRES_COPY_BOTH,
      PGRES_SINGLE_TUPLE
   );

   Oid = Integer;
   POid = ^Oid;
   TPGParamTypes = array of Oid;
   TPGParamValues = array of RawByteString;
   TPGParamLengths = array of Integer;
   TPGParamFormats = array of Integer;

   TdwsLibpq = class
      private
         FDLLHandle : THandle;

      public
         PQconnectdb : function (connInfo : PAnsiChar) : PGconn; cdecl;
         PQfinish : procedure (handle : PGconn); cdecl;

         PQerrorMessage : function (handle : PGconn) : PAnsiChar; cdecl;
         PQresultStatus : function (result : PGresult) : TPGExecStatusType; cdecl;
         PQclear : procedure (result : PGresult); cdecl;

         PQexec : function (handle : PGconn; command : PAnsiChar) : PGresult; cdecl;
         PQexecParams : function (handle : PGconn; command : PAnsiChar;
                                  nParams : Integer;
                                  const paramTypes : TPGParamTypes;
                                  const paramValues : TPGparamValues;
                                  const paramLengths : TPGparamLengths;
                                  const paramFormats : TPGparamFormats;
                                  resultFormat : Integer) : PGresult; cdecl;
         PQexecPrepared : function (handle : PGconn; stmtName : PAnsiChar;
                                    nParams : Integer;
                                    const paramValues : TPGparamValues;
                                    const paramLengths : TPGparamLengths;
                                    const paramFormats : TPGparamFormats;
                                    resultFormat : Integer) : PGresult; cdecl;
         PQprepare : function (handle : PGconn; stmtName : PAnsiChar; query : PAnsiChar;
                               nParams : Integer; const paramTypes : TPGParamTypes) : PGresult; cdecl;
         PQsendQueryParams : function (handle : PGconn; command : PAnsichar;
                                       nParams : Integer;
                                       const paramTypes : TPGParamTypes;
                                       const paramValues : TPGparamValues;
                                       const paramLengths : TPGparamLengths;
                                       const paramFormats : TPGparamFormats;
                                       resultFormat : Integer) : PGresult; cdecl;

         PQnfields : function (result : PGresult) : Integer; cdecl;
         PQntuples : function (result : PGresult) : Integer; cdecl;
         PQfname : function (result : PGresult; column_number : Integer) : PAnsiChar; cdecl;
         PQftype : function (result : PGresult; column_number : Integer) : Oid; cdecl;

         PQgetisnull : function (result : PGresult; tup_num, column_num : Integer) : Integer; cdecl;
         PQgetvalue : function (result : PGresult; tup_num, column_num : Integer) : PAnsiChar; cdecl;
         PQgetlength : function (result : PGresult; tup_num, column_num : Integer) : Integer; cdecl;

         constructor Create(const dllFileName : String = '');
         destructor Destroy; override;

         property DLLHandle : THandle read FDLLHandle;
   end;

const
   cPostgresEpochDeltaDays = 100*365 + 26;

  	BOOLOID        = 16;
  	BYTEAOID       = 17;
   CHAROID        = 18;
 	NAMEOID        = 19;
  	INT8OID        = 20;
  	INT2OID        = 21;
   INT2VECTOROID  = 22;
  	INT4OID        = 23;
  	REGPROCOID     = 24;
   TEXTOID        = 25;
  	OIDOID         = 26;
  	TIDOID         = 27;
  	XIDOID         = 28;
  	CIDOID         = 29;
   OIDVECTOROID   = 30;
   JSONOID        = 114;
   XMLOID        = 142;
  	PGNODETREEOID  = 194;
  	FLOAT4OID      = 700;
  	FLOAT8OID      = 701;
  	UNKNOWNOID     = 705;
  	BPCHAROID      = 1042;
  	VARCHAROID     = 1043;
  	TIMESTAMPOID   = 1114;
  	NUMERICOID     = 1700;

function PostgreSQLNumericToFloat(p : PByte) : Double;

function PostgreSQLSameParamTypes(const a, b : TPGParamTypes) : Boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// PostgreSQLNumericToFloat
//
function PostgreSQLNumericToFloat(p : PByte) : Double;
   (*
   from https://www.postgresql.org/message-id/16572.1091489720%40sss.pgh.pa.us

	pq_sendint(&buf, x.ndigits, sizeof(int16));
	pq_sendint(&buf, x.weight, sizeof(int16));
	pq_sendint(&buf, x.sign, sizeof(int16));
	pq_sendint(&buf, x.dscale, sizeof(int16));
	for (i = 0; i < x.ndigits; i++)
		pq_sendint(&buf, x.digits[i], sizeof(NumericDigit));

   Note that the "digits" are base-10000 digits.

   0.5      digits = 5000         weight = -1  dscale = 1
   5.0      digits = 5            weight = 0   dscale = 1
   1.5      digits = 1,5000       weight = 0   dscale = 1
   1.05     digits = 1,500        weight = 0   dscale = 2
   10.05    digits = 10,500       weight = 0   dscale = 2
   1.23456  digits = 1,2345,6000  weight = 0   dscale = 5
   1e12     digits = 1            weight = 3   dscale = 0
   1e12+0.5 digits = 1,0,0,0,
   1e13     digits = 10           weight = 3   dscale = 0
   1e26     digits = 10           weight = 6   dscale = 0

   select 0.0, 0.5, 5.0, 1.5, 1.05, 10.05, 1.23456, 1e12, 1e12+0.5, 1e25, 12345.6, 1234.5678

   *)

   function ReadInt16(var p : PByte) : Integer; inline;
   begin
      Result := SmallInt((p[0] shl 8) + p[1]);
      Inc(p, 2);
   end;

var
   nDigits, weight, sign, i, scale : Integer;
begin
   nDigits := ReadInt16(p);
   if nDigits = 0 then Exit(0);

   weight := ReadInt16(p);
   sign := ReadInt16(p);
   Inc(p, 2); //  dscale := ReadInt16(p);

   case nDigits of
      0 : Result := 0;
      1 : Result := ReadInt16(p);
   else
      Result := ReadInt16(p);
      for i := 2 to nDigits do
         Result := Result*10000 + ReadInt16(p);
   end;

   scale := weight-(nDigits-1);
   case scale of
      0 : ;
      1 : Result := Result * 1e4;
      2 : Result := Result * 1e8;
      3 : Result := Result * 1e12;
      -1 : Result := Result * 1e-4;
      -2 : Result := Result * 1e-8;
      -3 : Result := Result * 1e-12;
   else
      while scale > 0 do begin
         Result := Result * 1e4;
         Dec(scale);
      end;
      while scale < 0 do begin
         Result := Result * 1e-4;
         Inc(scale);
      end;
   end;
   if sign <> 0 then
      Result := -Result;
end;

// PostgreSQLSameParamTypes
//
function PostgreSQLSameParamTypes(const a, b : TPGParamTypes) : Boolean;
var
   n : Integer;
begin
   n := Length(a);
   if n <> Length(b) then Exit(False);
   if n = 0 then Exit(True);

   Result := CompareMem(Pointer(a), Pointer(b), n * SizeOf(Oid));
end;

// ------------------
// ------------------ TdwsLibpq ------------------
// ------------------

// Create
//
constructor TdwsLibpq.Create(const dllFileName : String = '');
var
   effectiveDLL : String;
begin
   inherited Create;

   if dllFileName = '' then
      effectiveDLL := 'libpq.dll'
   else effectiveDLL := dllFileName;

   FDLLHandle := LoadLibrary(PChar(effectiveDLL));
   if FDLLHandle = 0 then
      raise Exception.CreateFmt('Failed to load "%s" (%s)',
                                [ effectiveDLL, SysErrorMessage(GetLastError) ]);

   PQconnectdb := GetProcAddress(FDLLHandle, 'PQconnectdb');
   PQfinish := GetProcAddress(FDLLHandle, 'PQfinish');

   PQerrorMessage := GetProcAddress(FDLLHandle, 'PQerrorMessage');
   PQresultStatus := GetProcAddress(FDLLHandle, 'PQresultStatus');
   PQclear := GetProcAddress(FDLLHandle, 'PQclear');

   PQexec := GetProcAddress(FDLLHandle, 'PQexec');
   PQexecParams := GetProcAddress(FDLLHandle, 'PQexecParams');
   PQexecPrepared := GetProcAddress(FDLLHandle, 'PQexecPrepared');
   PQprepare := GetProcAddress(FDLLHandle, 'PQprepare');
   PQsendQueryParams := GetProcAddress(FDLLHandle, 'PQsendQueryParams');

   PQnfields := GetProcAddress(FDLLHandle, 'PQnfields');
   PQntuples := GetProcAddress(FDLLHandle, 'PQntuples');
   PQfname := GetProcAddress(FDLLHandle, 'PQfname');
   PQftype := GetProcAddress(FDLLHandle, 'PQftype');

   PQgetisnull := GetProcAddress(FDLLHandle, 'PQgetisnull');
   PQgetvalue := GetProcAddress(FDLLHandle, 'PQgetvalue');
   PQgetlength := GetProcAddress(FDLLHandle, 'PQgetlength');
end;

// Destroy
//
destructor TdwsLibpq.Destroy;
begin
   inherited;
   FreeLibrary(FDLLHandle);
end;

end.
