unit dwsSQLiteLibModule;

interface

uses
  System.SysUtils, System.Classes,
  dwsComp, dwsExprs, dwsSymbols, dwsUtils, dwsDatabase;

type
  TdwsSQLiteLib = class(TDataModule)
    dwsSQLite: TdwsUnit;
    procedure dwsSQLiteFunctionsSQLiteBackupEval(info: TProgramInfo);
  private
    { Private declarations }
    procedure SetScript(aScript : TDelphiWebScript);
  public
    { Public declarations }
    property Script : TDelphiWebScript write SetScript;
end;

implementation

uses dwsDatabaseLibModule, dwsSynSQLiteDataBase, SynSQLite3;

{$R *.dfm}

procedure TdwsSQLiteLib.SetScript(aScript : TDelphiWebScript);
begin
   dwsSQLite.Script:=aScript;
end;

procedure TdwsSQLiteLib.dwsSQLiteFunctionsSQLiteBackupEval(info: TProgramInfo);

   function SQLiteDBFromScriptObj(const obj : IScriptObj; const which : String) : TSQLite3DB;
   var
      scriptDB : TScriptDataBase;
      dbSelf : TObject;
   begin
      scriptDB := obj.ExternalObject as TScriptDataBase;
      dbSelf := (scriptDB.Intf as IGetSelf).GetSelf;
      if dbSelf is TdwsSynSQLiteDataBase then
         Result := TdwsSynSQLiteDataBase(dbSelf).DB.DB
      else raise EDWSDataBase.CreateFmt('%s parameter should be an SQLite database', [ which ]);
   end;

var
   sourceObj, destObj : IScriptObj;
   sourceDB, destDB : TSQLite3DB;
   sourceName, destName : UTF8String;
   backup : TSQLite3Backup;
   rc : Integer;
begin
   destObj := info.ParamAsScriptObj[0];
   destName := UTF8Encode(info.ParamAsString[1]);
   sourceObj := info.ParamAsScriptObj[2];
   sourceName := UTF8Encode(info.ParamAsString[3]);

   destDB := SQLiteDBFromScriptObj(destObj, 'Destination');
   sourceDB := SQLiteDBFromScriptObj(sourceObj, 'Source');

   backup := sqlite3.backup_init(destDB, Pointer(destName), sourceDB, Pointer(sourceName));
   if backup = 0 then
      raise Exception.CreateFmt('Cannot initialize backup: %s', [ UTF8ToString(sqlite3.errmsg(sourceDB)) ]);
   try
      rc := sqlite3.backup_step(backup, 0);
      if not rc in [SQLITE_BUSY, SQLITE_LOCKED, SQLITE_OK] then
         raise Exception.CreateFmt('Backup start error: %s', [ UTF8ToString(sqlite3.errmsg(sourceDB)) ]);
      repeat
         rc := sqlite3.backup_step(backup, -1);
         if rc = SQLITE_LOCKED then
            Sleep(10);
      until (rc <> SQLITE_BUSY) and (rc <> SQLITE_LOCKED);
   finally
      rc := sqlite3.backup_finish(backup);
      if rc <> SQLITE_OK then
         raise Exception.CreateFmt('Backup failed: %s', [ UTF8ToString(sqlite3.errmsg(sourceDB)) ]);
   end;
end;

end.
