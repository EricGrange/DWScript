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
unit dwsSQLiteVTArrays;

interface

uses Classes, SysUtils,
   SynSQLite3, SynCommons,
   dwsUtils, dwsExprs, dwsDatabase, dwsStack, dwsXPlatform, dwsDataContext, dwsSymbols,
   dwsSynSQLiteDatabase, dwsSynSQLiteVirtualTables;

type
   TdwsSQLiteVTArraysModule = class (TdwsSQLiteModule)
      private
         FNextDynArray : IScriptDynArray;

      protected

         function xConnect(argc : Integer; const argv : PPUTF8CharArray;
                           var ppVTab: PSQLite3VTab; var pzErr : PUTF8Char) : Integer; override;

      public
         constructor Create(const aDataBase : TdwsSynSQLiteDataBase);
         destructor Destroy; override;

         procedure ExposeDynamicArray(const dynArray : IScriptDynArray; const tableName : String);
   end;

   TdwsSQLiteVTArrayCursorClass = class of TdwsSQLiteVTArrayCursor;

   TdwsSQLiteVTArray = class (TdwsSQLiteVirtualTable)
      private
         FDynArray : IScriptDynArray;
         FVTab : TSQLite3VTab;
         FDeclareSQL : RawUTF8;
         FColumnAddr : array of Integer;
         FCursorClass : TdwsSQLiteVTArrayCursorClass;

      protected
         function xDisconnect : Integer; override;
         function xOpen(var ppCursor: PSQLite3VTabCursor) : Integer; override;

      public
         constructor Create(module : TdwsSQLiteVTArraysModule; const dynArray : IScriptDynArray);
   end;

   TdwsSQLiteVTArrayCursor = class (TdwsSQLiteVirtualTableCursor)
      private
         FDynArray : IScriptDynArray;
         FIndex : Integer;
         FArray : TdwsSQLiteVTArray;

      protected
         function xFilter(idxNum: Integer; const idxStr: PAnsiChar;
                          argc: Integer; var argv: TSQLite3ValueArray) : Integer; override;
         function xNext : Integer; override;
         function xEof : Integer; override;
         function xRowid(var pRowid: Int64) : Integer; override;

         procedure ResultText16(sContext: TSQLite3FunctionContext; const s : UnicodeString);
         procedure ResultSetVariant(sContext: TSQLite3FunctionContext; const v : Variant);

      public
         constructor Create(vt : TdwsSQLiteVTArray);
   end;

   TdwsSQLiteVTValueArrayCursor = class (TdwsSQLiteVTArrayCursor)
      protected
         function xColumn(sContext: TSQLite3FunctionContext; N: Integer) : Integer; override;
   end;

   TdwsSQLiteVTRecordArrayCursor = class (TdwsSQLiteVTArrayCursor)
      protected
         function xColumn(sContext: TSQLite3FunctionContext; N: Integer) : Integer; override;
   end;

   TdwsSQLiteVTClassArrayCursor = class (TdwsSQLiteVTArrayCursor)
      protected
         function xColumn(sContext: TSQLite3FunctionContext; N: Integer) : Integer; override;
   end;

const
   cModule_ScriptArray = 'ScriptArray';

implementation

// ------------------
// ------------------ TdwsSQLiteVTArraysModule ------------------
// ------------------

// Create
//
constructor TdwsSQLiteVTArraysModule.Create(const aDataBase : TdwsSynSQLiteDataBase);
begin
   inherited Create(aDataBase, cModule_ScriptArray);
end;

// Destroy
//
destructor TdwsSQLiteVTArraysModule.Destroy;
begin
   inherited;
end;

// ExposeDynamicArray
//
procedure TdwsSQLiteVTArraysModule.ExposeDynamicArray(const dynArray : IScriptDynArray; const tableName : String);
var
   sql : RawUTF8;
   stmt : TSQLite3Statement;
   tail : PUTF8Char;
begin
   FNextDynArray := dynArray;
   try
      sql := 'CREATE VIRTUAL TABLE ' + StringToUTF8(tableName) + ' USING ' + cModule_ScriptArray + '()';
      sqlite3_check(DB, SQLite3.prepare_v2(DB, Pointer(sql), Length(sql), stmt, tail), sql);
      try
         sqlite3_check(DB, SQLite3.step(stmt), sql);
      finally
         SQLite3.finalize(stmt);
      end;
   finally
      FNextDynArray := nil;
   end;
end;

// xConnect
//
function TdwsSQLiteVTArraysModule.xConnect(argc : Integer; const argv : PPUTF8CharArray;
                                           var ppVTab: PSQLite3VTab; var pzErr : PUTF8Char) : Integer;
var
   vta : TdwsSQLiteVTArray;
   errBuf : RawUTF8;
begin
   if argc <> 3 then begin
      pzErr := 'Invalid ScriptArray invokation';
      Exit(SQLITE_ERROR);
   end;

   vta := TdwsSQLiteVTArray.Create(Self, FNextDynArray);
   ppVTab := @vta.FVTab;

   Result := SQLite3.declare_vtab(DB, PAnsiChar(vta.FDeclareSQL));
   if Result <> 0 then begin
      errBuf := SQLite3.errmsg(DB);
      pzErr := SQLite3.malloc(Length(errBuf)+1);
      System.Move(Pointer(errBuf)^, pzErr^, Length(errBuf)+1);
      vta.Free;
   end;
end;

// ------------------
// ------------------ TdwsSQLiteVTArray ------------------
// ------------------

// Create
//
constructor TdwsSQLiteVTArray.Create(module : TdwsSQLiteVTArraysModule; const dynArray : IScriptDynArray);

   function ColumnSQLType(aType : TTypeSymbol) : RawUTF8;
   begin
      if aType is TBaseIntegerSymbol then
         Result := 'INTEGER'
      else if aType is TBaseFloatSymbol then
         Result := 'FLOAT'
      else if aType is TBaseStringSymbol then
         Result := 'STRING'
      else Result := ''; // unsupported = ignored
   end;

   procedure AppendColumn(const name : String; aType : TTypeSymbol; addr : Integer);
   var
      n : Integer;
      sqlType : RawUTF8;
   begin
      if name <> '' then begin
         sqlType := ColumnSQLType(aType);
         if sqlType <> '' then begin
            FDeclareSQL := FDeclareSQL + ','#13#10#9 + StringToUTF8(name) + ' ' + sqlType;
            n := Length(FColumnAddr);
            SetLength(FColumnAddr, n+1);
            FColumnAddr[n] := addr;
         end;
      end;
   end;

var
   i : Integer;
   elemSymbol : TTypeSymbol;
   member : TSymbol;
   fieldSymbol : TFieldSymbol;
begin
   inherited Create(module);
   FDynArray := dynArray;
   FVTab.pInstance := Self;

   FDeclareSQL := 'CREATE TABLE dummy ('#13#10#9'_Index INTEGER HIDDEN';
   elemSymbol := dynArray.ElementType.UnAliasedType;
   if (elemSymbol is TRecordSymbol) or elemSymbol.IsClassSymbol then begin
      if elemSymbol is TRecordSymbol then
         FCursorClass := TdwsSQLiteVTRecordArrayCursor
      else FCursorClass := TdwsSQLiteVTClassArrayCursor;
      for i := 0 to TStructuredTypeSymbol(elemSymbol).Members.Count-1 do begin
         member := TStructuredTypeSymbol(elemSymbol).Members[i];
         if (member is TPropertySymbol) and (TPropertySymbol(member).Visibility = cvPublished) then begin
            if not (TPropertySymbol(member).ReadSym is TFieldSymbol) then continue;
            fieldSymbol := TFieldSymbol(TPropertySymbol(member).ReadSym);
            AppendColumn(fieldSymbol.Name, fieldSymbol.Typ, fieldSymbol.Offset);
         end else if member is TFieldSymbol then begin
            fieldSymbol := TFieldSymbol(member);
            if fieldSymbol.Visibility = cvPublished then
               AppendColumn(fieldSymbol.Name, fieldSymbol.Typ, fieldSymbol.Offset);
         end;
      end;
      if Length(FColumnAddr) = 0 then begin
         raise Exception.CreateFmt('"%s" does not have published fields',
                                   [dynArray.ElementType.ClassName])
      end;
   end else begin
      FCursorClass := TdwsSQLiteVTValueArrayCursor;
      AppendColumn('Value', elemSymbol, 0);
   end;
   FDeclareSQL := FDeclareSQL + ')';
end;

// xDisconnect
//
function TdwsSQLiteVTArray.xDisconnect : Integer;
begin
   DecRefCount;
   Result := SQLITE_OK;
end;

// xOpen
//
function TdwsSQLiteVTArray.xOpen(var ppCursor: PSQLite3VTabCursor) : Integer;
begin
   ppCursor := SQLite3.malloc(SizeOf(TSQLite3VTabCursor));
   ppCursor.pInstance := FCursorClass.Create(Self);
   Result := SQLITE_OK;
end;

// ------------------
// ------------------ TdwsSQLiteVTArrayCursor ------------------
// ------------------

// Create
//
constructor TdwsSQLiteVTArrayCursor.Create(vt : TdwsSQLiteVTArray);
begin
   inherited Create(vt);
   FDynArray := vt.FDynArray;
   FArray := vt;
end;

// xFilter
//
function TdwsSQLiteVTArrayCursor.xFilter(idxNum: Integer; const idxStr: PAnsiChar;
                                         argc: Integer; var argv: TSQLite3ValueArray) : Integer;
begin
   FIndex := 0;
   Result := SQLITE_OK;
end;

// xNext
//
function TdwsSQLiteVTArrayCursor.xNext : Integer;
begin
   Inc(FIndex);
   if FIndex > FDynArray.ArrayLength then
      Result := SQLITE_ERROR
   else Result := SQLITE_OK;
end;

// xEof
//
function TdwsSQLiteVTArrayCursor.xEof : Integer;
begin
   if FIndex >= FDynArray.ArrayLength then
      Result := 1
   else Result := 0;
end;

// xRowid
//
function TdwsSQLiteVTArrayCursor.xRowid(var pRowid: Int64) : Integer;
begin
   pRowid := FIndex;
   Result := SQLITE_OK;
end;

// ResultText16
//
procedure TdwsSQLiteVTArrayCursor.ResultText16(sContext: TSQLite3FunctionContext; const s : UnicodeString);
var
   buf : RawUTF8;
begin
   buf := StringToUTF8(s);
   SQLite3.result_text(sContext, Pointer(buf), Length(buf), SQLITE_TRANSIENT);
end;

// ResultSetVariant
//
procedure TdwsSQLiteVTArrayCursor.ResultSetVariant(sContext: TSQLite3FunctionContext; const v : Variant);
begin
   case TVarData(v).VType of
      varInt64 : SQLite3.result_int64(sContext, TVarData(v).VInt64);
      varDouble : SQLite3.result_double(sContext, TVarData(v).VDouble);
      varUString : ResultText16(sContext, String(TVarData(v).VUString));
      varNull : SQLite3.result_null(sContext);
   else
      SQLite3.result_null(sContext);
   end;
end;

// ------------------
// ------------------ TdwsSQLiteVTValueArrayCursor ------------------
// ------------------

// xColumn
//
function TdwsSQLiteVTValueArrayCursor.xColumn(sContext: TSQLite3FunctionContext; N: Integer) : Integer;
var
   v : Variant;
begin
   Assert(FIndex < FDynArray.ArrayLength);
   case N of
      0 : SQLite3.result_int64(sContext, FIndex);
      1 : begin
         FDynArray.EvalAsVariant(FIndex, v);
         ResultSetVariant(sContext, v);
      end;
   else
      Assert(False);
   end;
   Result := SQLITE_OK;
end;


// ------------------
// ------------------ TdwsSQLiteVTRecordArrayCursor ------------------
// ------------------

// xColumn
//
function TdwsSQLiteVTRecordArrayCursor.xColumn(sContext: TSQLite3FunctionContext; N: Integer) : Integer;
var
   addr : Integer;
   v : Variant;
begin
   Assert(FIndex < FDynArray.ArrayLength);
   if N = 0 then
      SQLite3.result_int64(sContext, FIndex)
   else begin
      Assert(N-1 < Length(FArray.FColumnAddr));
      addr := FIndex * FDynArray.ElementSize + FArray.FColumnAddr[N-1];
      FDynArray.EvalAsVariant(addr, v);
      ResultSetVariant(sContext, v);
   end;
   Result := SQLITE_OK;
end;

// ------------------
// ------------------ TdwsSQLiteVTClassArrayCursor ------------------
// ------------------

// xColumn
//
function TdwsSQLiteVTClassArrayCursor.xColumn(sContext: TSQLite3FunctionContext; N: Integer) : Integer;
var
   intf : IUnknown;
   obj : IScriptObj;
   v : Variant;
begin
   Assert(FIndex < FDynArray.ArrayLength);
   if N = 0 then
      SQLite3.result_int64(sContext, FIndex)
   else begin
      Assert(N-1 < Length(FArray.FColumnAddr));
      FDynArray.EvalAsInterface(FIndex, intf);
      obj := intf as IScriptObj;
      obj.EvalAsVariant(FArray.FColumnAddr[N-1], v);
      ResultSetVariant(sContext, v);
   end;
   Result := SQLITE_OK;
end;

end.
