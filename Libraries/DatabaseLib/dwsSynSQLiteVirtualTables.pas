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
unit dwsSynSQLiteVirtualTables;

interface

uses Classes, SysUtils,
   SynSQLite3, SynCommons,
   dwsExprs, dwsUtils, dwsDatabase, dwsSynSQLiteDatabase;

type
   TdwsSQLiteUtility = class (TRefCountedObject)
      private
         FDB : TSQLite3DB;
         FLibrary : TSQLite3Library;

      public
         constructor Create(const utility : TdwsSQLiteUtility); overload;
         constructor Create(const aDB : TSQLDataBase); overload;

         property DB : TSQLite3DB read FDB;
         property SQLite3 : TSQLite3Library read FLibrary;
   end;

   TdwsSQLiteModule = class (TdwsSQLiteUtility)
      private
         FDataBase : TdwsSynSQLiteDataBase;
         FName : String;
         FModule : TSQLite3Module;

      protected
         function xCreate(argc : Integer; const argv : PPUTF8CharArray;
                          var ppVTab : PSQLite3VTab; var pzErr : PUTF8Char) : Integer; virtual;
         function xConnect(argc : Integer; const argv : PPUTF8CharArray;
                           var ppVTab: PSQLite3VTab; var pzErr : PUTF8Char) : Integer; virtual; abstract;


      public
         constructor Create(const aDataBase : TdwsSynSQLiteDataBase; const aName : String);

         property Name : String read FName;
   end;

   TdwsSQLiteVirtualTable = class (TdwsSQLiteUtility)
      private
         FModule : TdwsSQLiteModule;

      protected
         function xBestIndex(var pInfo: TSQLite3IndexInfo) : Integer; virtual;
         function xDisconnect : Integer; virtual; abstract;
         function xDestroy : Integer; virtual;
         function xOpen(var ppCursor: PSQLite3VTabCursor) : Integer; virtual; abstract;
         function xUpdate(nArg: Integer; var ppArg: TSQLite3ValueArray; var pRowid: Int64) : Integer; virtual;
         function xBegin : Integer; virtual;
         function xSync : Integer; virtual;
         function xCommit : Integer; virtual;
         function xRollback : Integer; virtual;
         function xFindFunction(nArg : Integer; const zName : PAnsiChar;
                                var pxFunc : TSQLFunctionFunc; var ppArg : Pointer) : Integer; virtual;
         function xRename(const zNew: PAnsiChar) : Integer; virtual;
         function xSavepoint(iSavepoint : Integer) : Integer; virtual;
         function xRelease(iSavepoint : Integer) : Integer; virtual;
         function xRollbackTo(iSavepoint : Integer) : Integer; virtual;

      public
         constructor Create(const aModule : TdwsSQLiteModule);

         property Module : TdwsSQLiteModule read FModule;
   end;

   TdwsSQLiteVirtualTableCursor = class (TdwsSQLiteUtility)
      private
         FTable : TdwsSQLiteVirtualTable;

      protected
         function xDestroy : Integer; virtual;
         function xFilter(idxNum: Integer; const idxStr: PAnsiChar;
                          argc: Integer; var argv: TSQLite3ValueArray) : Integer; virtual; abstract;
         function xNext : Integer; virtual; abstract;
         function xEof : Integer; virtual; abstract;
         function xColumn(sContext: TSQLite3FunctionContext; N: Integer) : Integer; virtual; abstract;
         function xRowid(var pRowid: Int64) : Integer; virtual; abstract;

      public
         constructor Create(const aTable : TdwsSQLiteVirtualTable);
   end;

function StringToSQLiteErrString(const s : String) : PUTF8Char;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// StringToSQLiteErrString
//
function StringToSQLiteErrString(const s : String) : PUTF8Char;
var
   buf : UTF8String;
begin
   buf := StringToUTF8(s);
   Result := sqlite3.malloc(Length(buf)+1);
   System.Move(Pointer(buf)^, Result^, Length(buf)+1);
end;

procedure ModuleDestroy(p : Pointer); cdecl;
begin
   (TObject(p) as TdwsSQLiteModule).Free;
end;

function VTabCreate(DB: TSQLite3DB; pAux: Pointer; argc: Integer; const argv: PPUTF8CharArray;
                    var ppVTab: PSQLite3VTab; var pzErr: PUTF8Char): Integer; cdecl;
begin
   Result := (TObject(pAux) as TdwsSQLiteModule).xCreate(argc, argv, ppVTab, pzErr);
end;

function VTabConnect(DB: TSQLite3DB; pAux: Pointer; argc: Integer; const argv: PPUTF8CharArray;
                     var ppVTab: PSQLite3VTab; var pzErr: PUTF8Char): Integer; cdecl;
begin
   Result := (TObject(pAux) as TdwsSQLiteModule).xConnect(argc, argv, ppVTab, pzErr);
end;

function VTabBestIndex(var pVTab: TSQLite3VTab; var pInfo: TSQLite3IndexInfo): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xBestIndex(pInfo);
end;

function VTabDisconnect(pVTab: PSQLite3VTab): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xDisconnect;
end;

function VTabDestroy(pVTab: PSQLite3VTab): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xDestroy;
end;

function VTabOpen(var pVTab: TSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xOpen(ppCursor);
end;

function VTabClose(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
begin
   Result := (TObject(pVtabCursor.pInstance) as TdwsSQLiteVirtualTableCursor).xDestroy;
end;

function VTabFilter(var pVtabCursor: TSQLite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar;
                    argc: Integer; var argv: TSQLite3ValueArray): Integer; cdecl;
begin
   Result := (TObject(pVtabCursor.pInstance) as TdwsSQLiteVirtualTableCursor).xFilter(idxNum, idxStr, argc, argv);
end;

function VTabNext(var pVtabCursor: TSQLite3VTabCursor): Integer; cdecl;
begin
   Result := (TObject(pVtabCursor.pInstance) as TdwsSQLiteVirtualTableCursor).xNext;
end;

function VTabEof(var pVtabCursor: TSQLite3VTabCursor): Integer; cdecl;
begin
   Result := (TObject(pVtabCursor.pInstance) as TdwsSQLiteVirtualTableCursor).xEof;
end;

function VTabColumn(var pVtabCursor: TSQLite3VTabCursor; sContext: TSQLite3FunctionContext; N: Integer): Integer; cdecl;
begin
   Result := (TObject(pVtabCursor.pInstance) as TdwsSQLiteVirtualTableCursor).xColumn(sContext, N);
end;

function VTabRowid(var pVtabCursor: TSQLite3VTabCursor; var pRowid: Int64): Integer; cdecl;
begin
   Result := (TObject(pVtabCursor.pInstance) as TdwsSQLiteVirtualTableCursor).xRowid(pRowid);
end;

function VTabUpdate(var pVTab: TSQLite3VTab; nArg: Integer; var ppArg: TSQLite3ValueArray;
                    var pRowid: Int64): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xUpdate(nArg, ppArg, pRowid);
end;

function VTabBegin(var pVTab: TSQLite3VTab): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xBegin;
end;

function VTabSync(var pVTab: TSQLite3VTab): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xSync;
end;

function VTabCommit(var pVTab: TSQLite3VTab): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xCommit;
end;

function VTabRollback(var pVTab: TSQLite3VTab): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xRollback;
end;

function VTabFindFunction(var pVTab: TSQLite3VTab; nArg: Integer; const zName: PAnsiChar;
      var pxFunc: TSQLFunctionFunc; var ppArg: Pointer): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xFindFunction(nArg, zName, pxFunc, ppArg);
end;

function VTabRename(var pVTab: TSQLite3VTab; const zNew: PAnsiChar): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xRename(zNew);
end;

function VTabSavepoint(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xSavepoint(iSavepoint);
end;

function VTabRelease(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xRelease(iSavepoint);
end;

function VTabRollbackTo(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; cdecl;
begin
   Result := (TObject(pVTab.pInstance) as TdwsSQLiteVirtualTable).xRollbackTo(iSavepoint);
end;

// ------------------
// ------------------ TdwsSQLiteUtility ------------------
// ------------------

// Create
//
constructor TdwsSQLiteUtility.Create(const utility : TdwsSQLiteUtility);
begin
   inherited Create;
   FDB := utility.FDB;
   FLibrary := utility.FLibrary;
end;

// Create
//
constructor TdwsSQLiteUtility.Create(const aDB : TSQLDataBase);
begin
   inherited Create;
   FDB := aDB.DB;
   FLibrary := aDB.SQLite3Library;
end;

// ------------------
// ------------------ TdwsSQLiteModule ------------------
// ------------------

// Create
//
constructor TdwsSQLiteModule.Create(const aDataBase : TdwsSynSQLiteDataBase; const aName : String);
var
   err : Integer;
   utf8Name : UTF8String;
begin
   inherited Create(aDataBase.DB);
   FDataBase := aDataBase;

   utf8Name := StringToUTF8(aName);

   FModule.iVersion := 2;
   FModule.xCreate := @VTabCreate;
   FModule.xConnect := @VTabConnect;
   FModule.xBestIndex := @VTabBestIndex;
   FModule.xDisconnect := @VTabDisconnect;
   FModule.xDestroy := @VTabDestroy;
   FModule.xOpen := @VTabOpen;
   FModule.xClose := @VTabClose;
   FModule.xFilter := @VTabFilter;
   FModule.xNext := @VTabNext;
   FModule.xEof := @VTabEof;
   FModule.xColumn := @VTabColumn;
   FModule.xRowid := @VTabRowid;
   FModule.xUpdate := @VTabUpdate;
   FModule.xBegin := @VTabBegin;
   FModule.xSync := @VTabSync;
   FModule.xCommit := @VTabCommit;
   FModule.xRollback := @VTabRollback;
   FModule.xFindFunction := @VTabFindFunction;
   FModule.xRename := @VTabRename;
   FModule.xSavepoint := @VTabSavepoint;
   FModule.xRelease := @VTabRelease;
   FModule.xRollbackTo := @VTabRollbackTo;

   err := SQLite3.create_module_v2(DB, Pointer(utf8Name), FModule,
                                   Self, @ModuleDestroy);
   sqlite3_check(DB, err);

   aDataBase.Module[aName] := Self;
end;

// xCreate
//
function TdwsSQLiteModule.xCreate(argc : Integer; const argv : PPUTF8CharArray;
                                  var ppVTab : PSQLite3VTab; var pzErr : PUTF8Char) : Integer;
begin
   Result := xConnect(argc, argv, ppVTab, pzErr);
end;

// ------------------
// ------------------ TdwsSQLiteVirtualTable ------------------
// ------------------

// xBestIndex
//
function TdwsSQLiteVirtualTable.xBestIndex(var pInfo: TSQLite3IndexInfo) : Integer;
begin
   Result := 0;
end;

// xDestroy
//
function TdwsSQLiteVirtualTable.xDestroy : Integer;
begin
   Result := xDisconnect;
end;

// xUpdate
//
function TdwsSQLiteVirtualTable.xUpdate(nArg: Integer; var ppArg: TSQLite3ValueArray; var pRowid: Int64) : Integer;
begin
   Result := SQLITE_ERROR;
end;

// xBegin
//
function TdwsSQLiteVirtualTable.xBegin : Integer;
begin
   Result := SQLITE_OK;
end;

// xSync
//
function TdwsSQLiteVirtualTable.xSync : Integer;
begin
   Result := SQLITE_OK;
end;

// xCommit
//
function TdwsSQLiteVirtualTable.xCommit : Integer;
begin
   Result := SQLITE_OK;
end;

// xRollback
//
function TdwsSQLiteVirtualTable.xRollback : Integer;
begin
   Result := SQLITE_OK;
end;

// xFindFunction
//
function TdwsSQLiteVirtualTable.xFindFunction(nArg : Integer; const zName : PAnsiChar;
                                              var pxFunc : TSQLFunctionFunc; var ppArg : Pointer) : Integer;
begin
   Result := 0;
end;

// xRename
//
function TdwsSQLiteVirtualTable.xRename(const zNew: PAnsiChar) : Integer;
begin
   Result := SQLITE_ERROR;
end;

// xSavepoint
//
function TdwsSQLiteVirtualTable.xSavepoint(iSavepoint : Integer) : Integer;
begin
   Result := SQLITE_OK;
end;

// xRelease
//
function TdwsSQLiteVirtualTable.xRelease(iSavepoint : Integer) : Integer;
begin
   Result := SQLITE_OK;
end;

// xRollbackTo
//
function TdwsSQLiteVirtualTable.xRollbackTo(iSavepoint : Integer) : Integer;
begin
   Result := SQLITE_OK;
end;

// Create
//
constructor TdwsSQLiteVirtualTable.Create(const aModule : TdwsSQLiteModule);
begin
   inherited Create(aModule);
   FModule := aModule;
end;

// ------------------
// ------------------ TdwsSQLiteVirtualTableCursor ------------------
// ------------------

// xDestroy
//
function TdwsSQLiteVirtualTableCursor.xDestroy : Integer;
begin
   Free;
   Result := SQLITE_OK;
end;

// Create
//
constructor TdwsSQLiteVirtualTableCursor.Create(const aTable : TdwsSQLiteVirtualTable);
begin
   inherited Create(aTable);
   FTable := aTable;
end;

end.
