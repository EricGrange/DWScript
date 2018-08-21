unit dwsDatabaseLibModule;

interface

uses
  SysUtils, Classes, Masks,
  dwsStrings, dwsUtils, dwsExprList, dwsXPlatform, dwsInfo,
  dwsComp, dwsExprs, dwsSymbols, dwsStack, dwsDatabase, dwsJSON, dwsErrors;

type

  TdwsDatabaseLib = class(TDataModule)
    dwsDatabase: TdwsUnit;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    procedure dwsDatabaseClassesDataBaseConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseCleanUp(ExternalObject: TObject);

    function dwsDatabaseClassesDataBaseMethodsBeginTransactionFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataBaseMethodsCommitFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataBaseMethodsRollbackFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataBaseMethodsInTransactionFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;

    procedure dwsDatabaseClassesDataBaseMethodsExecEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsQueryEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseClassesDataSetMethodsEofFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsNextFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsDataTypeFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsAsStringFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataSetMethodsGetFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseClassesDataSetMethodsAsStringByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsAsIntegerByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsAsIntegerByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsAsFloatByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsAsFloatByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsAsStringByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsIsNullFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsAsIntegerFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsAsFloatFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsAsBooleanFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataSetMethodsFieldByNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseClassesDataSetMethodsIndexOfFieldFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsDeclaredTypeFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsStringifyFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsStringifyAllFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataSetMethodsFindFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsVersionInfoTextEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseFunctionsBlobParameterFastEval(
      const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataFieldMethodsAsBlobFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsAsBlobByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsAsBlobByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataBasePoolMethodsAcquireFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataBasePoolMethodsReleaseEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseClassesDataBasePoolMethodsCleanupFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataBasePoolMethodsCountFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsIsNullByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsIsNullByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    function dwsDatabaseFunctionsBlobHexParameterFastEval(
      const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsStringifyMapFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataBaseMethodsLowerCaseStringifyEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseFunctionsDateParameterFastEval(
      const args: TExprBaseListExec): Variant;
    function dwsDatabaseFunctionsBlobHexParameterDefFastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataBaseMethodsGetOptionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsSetOptionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsOptionListEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseClassesDataSetMethodsStepFastEval(baseExpr: TTypedExpr;
      const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataSetMethodsFieldCountFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
  private
    { Private declarations }
    procedure SetScript(aScript : TDelphiWebScript);
    procedure RaiseDBException(Info: TProgramInfo; const msg : String);

  public
    { Public declarations }
    class procedure CleanupDataBasePool(const filter : String = '*');
    function CountPooledDataBases(const filter : String = '*') : Integer;

    property Script : TDelphiWebScript write SetScript;
  end;

type
   TScriptDataBase = class
      Intf : IdwsDataBase;
      LowerCaseStringify : Boolean;
   end;

implementation

{$R *.dfm}

resourcestring
   FIELD_NOT_FOUND = 'Field ''%s'' not found';

type
   TDataSet = class
      Intf : IdwsDataSet;
      FirstDone : Boolean;
      ScriptFieldsPrepared : Boolean;
      WriterOptions : TdwsJSONWriterOptions;
      function IndexOfField(const aName : String) : Integer;
      function FieldByName(const args : TExprBaseListExec) : IdwsDataField;
      function Step : Boolean;
      class procedure WriteValueToJSON(wr : TdwsJSONWriter; const fld : IdwsDataField); static;
      procedure WriteToJSON(wr : TdwsJSONWriter; initial : Integer);
      function Stringify : String;
      function StringifyAll(maxRows : Integer) : String;
      function StringifyMap(maxRows : Integer) : String;
      procedure PrepareScriptFields(programInfo : TProgramInfo; var fieldsInfo : IInfo);
      class procedure NeedScriptFields(programInfo : TProgramInfo; extObject: TObject; var fieldsInfo : IInfo);
   end;

   TDataField = class
      Intf : IdwsDataField;
   end;

type
    TDataBaseQueue = TSimpleQueue<IdwsDataBase>;

var
   vPools : TSimpleNameObjectHash<TDataBaseQueue>;
   vPoolsCS : TMultiReadSingleWrite;
   vPoolsCount : Integer;

// IndexOfField
//
function TDataSet.IndexOfField(const aName : String) : Integer;
begin
   for Result:=0 to Intf.FieldCount-1 do
      if UnicodeSameText(Intf.Fields[Result].Name, aName) then Exit;
   Result:=-1;
end;

// FieldByName (args)
//
function TDataSet.FieldByName(const args : TExprBaseListExec) : IdwsDataField;
var
   fieldName : String;
   index : Integer;
begin
   fieldName := args.AsString[0];
   index := IndexOfField(fieldName);
   if index >= 0 then
      Result := Intf.GetField(index)
   else raise EDWSDataBase.CreateFmt('Unknown field "%s"', [fieldName]);
end;

// Step
//
function TDataSet.Step : Boolean;
begin
   if FirstDone then
      Intf.Next
   else FirstDone:=True;
   Result:=not Intf.EOF;
end;

// WriteValueToJSON
//
class procedure TDataSet.WriteValueToJSON(wr : TdwsJSONWriter; const fld : IdwsDataField);
begin
   case fld.DataType of
      dftInteger : wr.WriteInteger(fld.AsInteger);
      dftFloat : wr.WriteNumber(fld.AsFloat);
      dftString, dftBlob : wr.WriteString(fld.AsString);
      dftBoolean : wr.WriteBoolean(fld.AsBoolean);
      dftDateTime : wr.WriteDate(fld.AsFloat);
   else
      wr.WriteNull;
   end;
end;

// WriteToJSON
//
procedure TDataSet.WriteToJSON(wr : TdwsJSONWriter; initial : Integer);
var
   i : Integer;
   fld : IdwsDataField;
begin
   wr.BeginObject;
   for i:=initial to Intf.FieldCount-1 do begin
      fld:=intf.Fields[i];
      wr.WriteName(fld.Name);
      WriteValueToJSON(wr, fld);
   end;
   wr.EndObject;
end;

// Stringify
//
function TDataSet.Stringify : String;
var
   wr : TdwsJSONWriter;
begin
   wr:=TdwsJSONWriter.Create(nil, WriterOptions);
   try
      WriteToJSON(wr, 0);
      Result:=wr.ToString;
   finally
      wr.Free;
   end;
end;

// StringifyAll
//
function TDataSet.StringifyAll(maxRows : Integer) : String;
var
   wr : TdwsJSONWriter;
begin
   wr:=TdwsJSONWriter.Create(nil, WriterOptions);
   try
      wr.BeginArray;
      while not Intf.EOF do begin
         WriteToJSON(wr, 0);
         Intf.Next;
         Dec(maxRows);
         if maxRows=0 then break;
      end;
      wr.EndArray;
      Result:=wr.ToString;
   finally
      wr.Free;
   end;
end;

// StringifyMap
//
function TDataSet.StringifyMap(maxRows : Integer) : String;
var
   wr : TdwsJSONWriter;
begin
   wr:=TdwsJSONWriter.Create(nil, WriterOptions);
   try
      wr.BeginObject;
      while not Intf.EOF do begin
         wr.WriteName(Intf.Fields[0].AsString);
         WriteToJSON(wr, 1);
         Intf.Next;
         Dec(maxRows);
         if maxRows=0 then break;
      end;
      wr.EndObject;
      Result:=wr.ToString;
   finally
      wr.Free;
   end;
end;

// PrepareScriptFields
//
procedure TDataSet.PrepareScriptFields(programInfo : TProgramInfo; var fieldsInfo : IInfo);
var
   i : Integer;
   dataSetInfo : IInfo;
   dataFieldInfo : IInfo;
   dataFieldsArray : IScriptDynArray;
   dataFieldConstructor : IInfo;
   dataFieldObj : TDataField;
begin
   dataSetInfo:=programInfo.Vars[SYS_SELF];

   fieldsInfo:=dataSetInfo.Member['FFields'];

   dataFieldsArray:=fieldsInfo.ScriptDynArray;
   dataFieldsArray.ArrayLength:=Intf.FieldCount;

   dataFieldConstructor:=programInfo.Vars['DataField'].Method['Create'];
   for i:=0 to Intf.FieldCount-1 do begin
      dataFieldInfo:=dataFieldConstructor.Call;
      dataFieldObj:=TDataField.Create;
      dataFieldObj.Intf:=Intf.Fields[i];
      dataFieldInfo.ExternalObject:=dataFieldObj;
      dataFieldsArray.AsVariant[i]:=dataFieldInfo.Value;
   end;
end;

// NeedScriptFields
//
class procedure TDataSet.NeedScriptFields(programInfo : TProgramInfo; extObject : TObject; var fieldsInfo : IInfo);
var
   dataSet : TDataSet;
begin
   dataSet:=(extObject as TDataSet);
   if not dataSet.ScriptFieldsPrepared then begin
      dataSet.PrepareScriptFields(programInfo, fieldsInfo);
      dataSet.ScriptFieldsPrepared:=True;
   end else fieldsInfo:=programInfo.Vars['FFields'];
end;

procedure TdwsDatabaseLib.DataModuleCreate(Sender: TObject);
begin
   vPoolsCS.BeginWrite;
   if vPoolsCount=0 then
      vPools:=TSimpleNameObjectHash<TDataBaseQueue>.Create;
   Inc(vPoolsCount);
   vPoolsCS.EndWrite;
end;

procedure TdwsDatabaseLib.DataModuleDestroy(Sender: TObject);
begin
   vPoolsCS.BeginWrite;
   Dec(vPoolsCount);
   if vPoolsCount=0 then begin
      vPools.Clean;
      vPools.Free;
      vPools:=nil;
   end;
   vPoolsCS.EndWrite;
end;

// SetScript
//
procedure TdwsDatabaseLib.SetScript(aScript : TDelphiWebScript);
begin
   dwsDatabase.Script:=aScript;
end;

// RaiseDBException
//
procedure TdwsDatabaseLib.RaiseDBException(Info: TProgramInfo; const msg : String);
var
   exceptObj : IScriptObj;
begin
   exceptObj:=Info.Vars['EDBException'].Method[SYS_TOBJECT_CREATE].Call([msg]).ScriptObj;
   (exceptObj.ExternalObject as TdwsExceptionContext).Skip(1); // temporary constructor expression
   Info.RaiseExceptObj(msg, exceptObj);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsAcquireFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
var
   name : String;
   db : IdwsDataBase;
   dbo : TScriptDataBase;
   q : TSimpleQueue<IdwsDataBase>;
   obj : TScriptObjInstance;
begin
   name := args.AsString[0];
   vPoolsCS.BeginWrite;
   try
      q := vPools[name];
      if q <> nil then
         q.Pull(db);
   finally
      vPoolsCS.EndWrite;
   end;
   if Assigned(db) then begin
      obj := TScriptObjInstance.Create(TTypedExpr(args.Expr).Typ as TClassSymbol, args.Exec as TdwsProgramExecution);
      dbo := TScriptDataBase.Create;
      obj.ExternalObject := dbo;
      dbo.Intf := db;
      VarCopySafe(Result, IScriptObj(obj));
   end else VarCopySafe(Result, IScriptObj(nil));
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsReleaseEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name, checkRelease : String;
   obj : TObject;
   nb : Integer;
   db : IdwsDataBase;
   q : TSimpleQueue<IdwsDataBase>;
begin
   name:=Info.ParamAsString[0];
   obj:=Info.ParamAsObject[1];
   if obj is TScriptDataBase then begin
      db:=TScriptDataBase(obj).Intf;
      checkRelease:=db.CanReleaseToPool;
      if checkRelease<>'' then begin
         checkRelease:='Releasing to pool not allowed: '+checkRelease;
         RaiseDBException(Info, checkRelease);
      end;
   end;
   nb:=Info.ParamAsInteger[2];
   Info.ParamAsVariant[1]:=IUnknown(nil);
   vPoolsCS.BeginWrite;
   try
      q:=vPools[name];
      if q=nil then begin
         if db=nil then Exit;
         q:=TSimpleQueue<IdwsDataBase>.Create;
         vPools[name]:=q;
      end;
      if db<>nil then
         q.Push(db);
      while q.Count>nb do
         q.Pull(db);
   finally
      vPoolsCS.EndWrite;
   end;
end;

class procedure TdwsDatabaseLib.CleanupDataBasePool(const filter : String = '*');
var
   i : Integer;
   mask : TMask;
   q, detached : TSimpleQueue<IdwsDataBase>;
   db : IdwsDataBase;
begin
   detached:=TSimpleQueue<IdwsDataBase>.Create;
   mask:=TMask.Create(filter);
   try
      vPoolsCS.BeginWrite;
      try
         for i:=0 to vPools.HighIndex do begin
            q:=vPools.BucketObject[i];
            if q=nil then continue;
            if mask.Matches(vPools.BucketName[i]) then begin
               while q.Pull(db) do
                  detached.Push(db);
            end;
            if q.Count=0 then begin
               q.Free;
               vPools.BucketObject[i]:=nil;
            end;
         end;
      finally
         vPoolsCS.EndWrite;
      end;
      while detached.Pull(db) do ;
   finally
      mask.Free;
      detached.Free;
   end;
end;

// CountPooledDataBases
//
function TdwsDatabaseLib.CountPooledDataBases(const filter : String = '*') : Integer;
var
   i : Integer;
   mask : TMask;
   q : TSimpleQueue<IdwsDataBase>;
begin
   Result:=0;
   mask:=TMask.Create(filter);
   try
      vPoolsCS.BeginRead;
      try
         for i:=0 to vPools.HighIndex do begin
            q:=vPools.BucketObject[i];
            if q=nil then continue;
            if mask.Matches(vPools.BucketName[i]) then
               Inc(Result, q.Count);
         end;
      finally
         vPoolsCS.EndRead;
      end;
   finally
      mask.Free;
   end;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsCleanupFastEval(
   baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   CleanupDataBasePool(args.AsString[0]);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsCountFastEval(
   baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   Result := CountPooledDataBases(args.AsString[0]);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   db : IdwsDataBase;
   scriptDyn : IScriptDynArray;
begin
   scriptDyn:=Info.ParamAsScriptDynArray[1];
   db:=TdwsDatabase.CreateDataBase(Info.ParamAsString[0], scriptDyn.ToStringArray);

   ExtObject:=TScriptDataBase.Create;
   TScriptDataBase(ExtObject).Intf:=db;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsBeginTransactionFastEval(
   baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TScriptDataBase).Intf.BeginTransaction;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsCommitFastEval(
   baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TScriptDataBase).Intf.Commit;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsRollbackFastEval(
   baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TScriptDataBase).Intf.Rollback;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsInTransactionFastEval(
   baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TScriptDataBase).Intf.InTransaction;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsExecEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   scriptDyn : IScriptDynArray;
   db : IdwsDataBase;
begin
   scriptDyn:=Info.ParamAsScriptDynArray[1];

   db := (ExtObject as TScriptDataBase).Intf;
   db.Exec(Info.ParamAsString[0], scriptDyn, Info.Execution.CallStackLastExpr);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsQueryEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   scriptDyn : IScriptDynArray;
   ids : IdwsDataSet;
   dbo : TScriptDataBase;
   dataSetInfo : IInfo;
   dataSet : TDataSet;
begin
   scriptDyn:=Info.ParamAsScriptDynArray[1];

   dbo:=(ExtObject as TScriptDataBase);
   ids:=dbo.Intf.Query(Info.ParamAsString[0], scriptDyn, Info.Execution.CallStackLastExpr);

   dataSetInfo:=Info.Vars['DataSet'].Method['Create'].Call;

   dataSet:=TDataSet.Create;
   dataSet.Intf:=ids;
   if dbo.LowerCaseStringify then
      dataSet.WriterOptions:=[woLowerCaseNames];

   dataSetInfo.ExternalObject:=dataSet;

   Info.ResultAsVariant:=dataSetInfo.Value;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsGetOptionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TScriptDataBase).Intf.Options[Info.ParamAsString[0]];
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsSetOptionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TScriptDataBase).Intf.Options[Info.ParamAsString[0]] := Info.ParamAsString[1];
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsLowerCaseStringifyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TScriptDataBase).LowerCaseStringify:=Info.ParamAsBoolean[0];
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsOptionListEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := (ExtObject as TScriptDataBase).Intf.OptionList;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsVersionInfoTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as TScriptDataBase).Intf.VersionInfoText;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsBlobFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, RawByteStringToScriptString((baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsBlob));
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsBooleanFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsBoolean);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsFloatFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsFloat);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsIntegerFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsInteger);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsStringFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsString);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsDataTypeFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, Ord((baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.DataType));
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsDeclaredTypeFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.DeclaredType);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsIsNullFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.IsNull);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.Name);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsStringByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).FieldByName(args).AsString);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsStringByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.GetField(args.AsInteger[0]).AsString);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsIntegerByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).FieldByName(args).AsInteger);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsIntegerByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.GetField(args.AsInteger[0]).AsInteger);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsBlobByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, RawByteStringToScriptString((baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).FieldByName(args).AsBlob));
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsBlobByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, RawByteStringToScriptString((baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.GetField(args.AsInteger[0]).AsBlob));
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsFloatByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).FieldByName(args).AsFloat);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsFloatByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.GetField(args.AsInteger[0]).AsFloat);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIsNullByNameFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).FieldByName(args).IsNull);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIsNullByIndexFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.GetField(args.AsInteger[0]).IsNull);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsEofFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.Eof);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsFieldByNameEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   fieldName : String;
   fieldsInfo : IInfo;
   index : Integer;
begin
   fieldName:=Info.ParamAsString[0];
   index:=(ExtObject as TDataSet).IndexOfField(fieldName);
   if index<0 then
      RaiseDBException(Info, Format(FIELD_NOT_FOUND, [fieldName]))
   else begin
      TDataSet.NeedScriptFields(Info, ExtObject, fieldsInfo);
      Info.ResultAsVariant:=fieldsInfo.Element([index]).Value;
   end;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsFindFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   fieldsInfo : IInfo;
   index : Integer;
begin
   index:=(ExtObject as TDataSet).IndexOfField(Info.ParamAsString[0]);
   if index<0 then
      Info.ResultAsVariant:=IUnknown(nil)
   else begin
      TDataSet.NeedScriptFields(Info, ExtObject, fieldsInfo);
      Info.ResultAsVariant:=fieldsInfo.Element([index]).Value;
   end;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsFieldCountFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.FieldCount;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsGetFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   fieldsInfo : IInfo;
   index : Integer;
begin
   TDataSet.NeedScriptFields(Info, ExtObject, fieldsInfo);
   index:=Info.ParamAsInteger[0];
   Info.ResultAsVariant:=fieldsInfo.Element([index]).Value;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIndexOfFieldFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).IndexOfField(args.AsString[0]))
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsNextFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.Next;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStepFastEval(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Step);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyAllFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).StringifyAll(args.AsInteger[0]));
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Stringify);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyMapFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).StringifyMap(args.AsInteger[0]));
end;

function TdwsDatabaseLib.dwsDatabaseFunctionsBlobHexParameterFastEval(
  const args: TExprBaseListExec): Variant;
begin
   Result := dwsUtils.HexToBin(args.AsString[0]);
end;

function TdwsDatabaseLib.dwsDatabaseFunctionsBlobHexParameterDefFastEval(
  const args: TExprBaseListExec): Variant;
begin
   try
      Result := dwsUtils.HexToBin(args.AsString[0]);
   except
      on E : EHexEncodingException do
         Result := args.AsString[1]
      else raise;
   end;
end;

function TdwsDatabaseLib.dwsDatabaseFunctionsBlobParameterFastEval(
  const args: TExprBaseListExec): Variant;
begin
   Result:=args.AsDataString[0];
end;

function TdwsDatabaseLib.dwsDatabaseFunctionsDateParameterFastEval(
  const args: TExprBaseListExec): Variant;
begin
   VarSetDateTime(Result, args.AsFloat[0]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vPoolsCS:=TMultiReadSingleWrite.Create;

finalization

   vPoolsCS.Free;
   vPoolsCS:=nil;

end.
