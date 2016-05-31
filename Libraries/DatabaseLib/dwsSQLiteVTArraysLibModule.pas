unit dwsSQLiteVTArraysLibModule;

interface

uses
  SysUtils, Classes,
  dwsStrings, dwsUtils, dwsExprList, dwsXPlatform,
  dwsComp, dwsExprs, dwsSymbols, dwsStack, dwsDatabase, dwsJSON, dwsErrors,
  dwsDatabaseLibModule, dwsSynSQLiteDatabase, dwsSQLiteVTArrays;

type
  TdwsSQLiteVTArraysLib = class(TDataModule)
    dwsVTArrays: TdwsUnit;
    procedure dwsVTArraysFunctionsEnableVTArraysEval(info: TProgramInfo);
  private
    { Private declarations }
    procedure SetScript(aScript : TDelphiWebScript);
  public
    { Public declarations }
    property Script : TDelphiWebScript write SetScript;
  end;

implementation

{$R *.dfm}

// ------------------
// ------------------ TTdwsSQLiteVTArraysLib ------------------
// ------------------

procedure TdwsSQLiteVTArraysLib.SetScript(aScript : TDelphiWebScript);
begin
   dwsVTArrays.Script := aScript;
end;

// SetScript
//
procedure TdwsSQLiteVTArraysLib.dwsVTArraysFunctionsEnableVTArraysEval(
  info: TProgramInfo);
var
   dbObj : IScriptObj;
   sdb : TScriptDataBase;
   dbSelf : TObject;
   dbSQlite : TdwsSynSQLiteDataBase;
   regModule : TObject;
   module : TdwsSQLiteVTArraysModule;
   dynArray : IScriptDynArray;
   tableName : String;
begin
   dbObj := info.ParamAsScriptObj[0];
   if dbObj = nil then
      raise Exception.Create('Cannot expose to nil DataBase');

   sdb := dbObj.ExternalObject as TScriptDataBase;
   dbSelf := (sdb.Intf as IGetSelf).GetSelf;
   if not (dbSelf is TdwsSynSQLiteDataBase) then
      raise Exception.Create('DataBase must be an SQLite DB');
   dbSQlite := TdwsSynSQLiteDataBase(dbSelf);

   regModule := dbSQlite.Module[cModule_ScriptArray];
   if regModule = nil then
      module := TdwsSQLiteVTArraysModule.Create(dbSQlite)
   else module := (regModule as TdwsSQLiteVTArraysModule);

   dynArray := info.ParamAsScriptDynArray[1];
   tableName := info.ParamAsString[2];

   module.ExposeDynamicArray(dynArray, tableName);
end;

end.
