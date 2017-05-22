unit dwsDatabaseLibModule;

interface

uses
  SysUtils, Classes, Masks,
  dwsStrings, dwsUtils, dwsExprList, dwsXPlatform, dwsInfo,
  dwsComp, dwsExprs, dwsSymbols, dwsStack, dwsDatabase, dwsJSON, dwsErrors;

type

  TdwsDatabaseLib = class(TDataModule)
    dwsDatabase: TdwsUnit;
    procedure dwsDatabaseClassesDataBaseConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseCleanUp(ExternalObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsBeginTransactionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsCommitEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsInTransactionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsExecEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsQueryEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsRollbackEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsEofEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsNextEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsFieldCountEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataFieldMethodsNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataFieldMethodsDataTypeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataFieldMethodsAsStringEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsGetFieldEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsAsStringByNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsAsIntegerByIndexEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsAsIntegerByNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsAsFloatByIndexEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsAsFloatByNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsAsStringByIndexEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataFieldMethodsIsNullEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataFieldMethodsAsIntegerEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataFieldMethodsAsFloatEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataFieldMethodsAsBooleanEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsFieldByNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsIndexOfFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataFieldMethodsDeclaredTypeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsStepEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsStringifyEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsStringifyAllEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsFindFieldEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsVersionInfoTextEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseFunctionsBlobParameterFastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataFieldMethodsAsBlobEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsAsBlobByNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsAsBlobByIndexEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataBasePoolMethodsAcquireEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure dwsDatabaseClassesDataBasePoolMethodsReleaseEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBasePoolMethodsCleanupEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBasePoolMethodsCountEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsIsNullByNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsIsNullByIndexEval(Info: TProgramInfo;
      ExtObject: TObject);
    function dwsDatabaseFunctionsBlobHexParameterFastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataSetMethodsStringifyMapEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsLowerCaseStringifyEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseFunctionsDateParameterFastEval(
      const args: TExprBaseListExec): Variant;
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
      function FieldByName(Info : TProgramInfo) : IdwsDataField;
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

// FieldByName
//
function TDataSet.FieldByName(Info : TProgramInfo) : IdwsDataField;
var
   fieldName : String;
   index : Integer;
begin
   fieldName:=Info.ParamAsString[0];
   index:=IndexOfField(fieldName);
   if index>=0 then
      Result:=Intf.GetField(index)
   else raise Exception.CreateFmt('Unknown field "%s"', [fieldName]);
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
      dataFieldsArray.AsData[i]:=dataFieldInfo.Value;
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

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsAcquireEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   db : IdwsDataBase;
   dbo : TScriptDataBase;
   q : TSimpleQueue<IdwsDataBase>;
   obj : TScriptObjInstance;
begin
   name:=Info.ParamAsString[0];
   vPoolsCS.BeginWrite;
   try
      q:=vPools[name];
      if q<>nil then
         q.Pull(db);
   finally
      vPoolsCS.EndWrite;
   end;
   if Assigned(db) then begin
      obj:=TScriptObjInstance.Create(Info.FuncSym.Typ as TClassSymbol, Info.Execution);
      dbo:=TScriptDataBase.Create;
      obj.ExternalObject:=dbo;
      dbo.Intf:=db;
      Info.ResultAsVariant:=IScriptObj(obj);
   end else Info.ResultAsVariant:=IScriptObj(nil);
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

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsCleanupEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   CleanupDataBasePool(Info.ParamAsString[0]);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=CountPooledDataBases(Info.ParamAsString[0]);
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

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsBeginTransactionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TScriptDataBase).Intf.BeginTransaction;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsCommitEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TScriptDataBase).Intf.Commit;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsExecEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   scriptDyn : IScriptDynArray;
   db : IdwsDataBase;
begin
   scriptDyn:=Info.ParamAsScriptDynArray[1];

   db := (ExtObject as TScriptDataBase).Intf;
   db.Exec(Info.ParamAsString[0], scriptDyn.AsData, Info.Execution.CallStackLastExpr);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsInTransactionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=(ExtObject as TScriptDataBase).Intf.InTransaction;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsLowerCaseStringifyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TScriptDataBase).LowerCaseStringify:=Info.ParamAsBoolean[0];
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
   ids:=dbo.Intf.Query(Info.ParamAsString[0], scriptDyn.AsData, Info.Execution.CallStackLastExpr);

   dataSetInfo:=Info.Vars['DataSet'].Method['Create'].Call;

   dataSet:=TDataSet.Create;
   dataSet.Intf:=ids;
   if dbo.LowerCaseStringify then
      dataSet.WriterOptions:=[woLowerCaseNames];

   dataSetInfo.ExternalObject:=dataSet;

   Info.ResultAsVariant:=dataSetInfo.Value;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsRollbackEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TScriptDataBase).Intf.Rollback;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsVersionInfoTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TScriptDataBase).Intf.VersionInfoText;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsBlobEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=(ExtObject as TDataField).Intf.AsBlob;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsBooleanEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=(ExtObject as TDataField).Intf.AsBoolean;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsFloatEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=(ExtObject as TDataField).Intf.AsFloat;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsIntegerEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=(ExtObject as TDataField).Intf.AsInteger;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsStringEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TDataField).Intf.AsString;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsDataTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=Ord((ExtObject as TDataField).Intf.DataType);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsDeclaredTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TDataField).Intf.DeclaredType;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsIsNullEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=(ExtObject as TDataField).Intf.IsNull;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TDataField).Intf.Name;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsStringByNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TDataSet).FieldByName(Info).AsString;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsStringByIndexEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TDataSet).Intf.GetField(Info.ParamAsInteger[0]).AsString;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsIntegerByNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=(ExtObject as TDataSet).FieldByName(Info).AsInteger;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsIntegerByIndexEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=(ExtObject as TDataSet).Intf.GetField(Info.ParamAsInteger[0]).AsInteger;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsBlobByNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=(ExtObject as TDataSet).FieldByName(Info).AsBlob;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsBlobByIndexEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=(ExtObject as TDataSet).Intf.GetField(Info.ParamAsInteger[0]).AsBlob;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsFloatByNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=(ExtObject as TDataSet).FieldByName(Info).AsFloat;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsFloatByIndexEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=(ExtObject as TDataSet).Intf.GetField(Info.ParamAsInteger[0]).AsFloat;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIsNullByNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=(ExtObject as TDataSet).FieldByName(Info).IsNull;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIsNullByIndexEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=(ExtObject as TDataSet).Intf.GetField(Info.ParamAsInteger[0]).IsNull;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsEofEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=(ExtObject as TDataSet).Intf.Eof;
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

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsFieldCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=(ExtObject as TDataSet).Intf.FieldCount;
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

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIndexOfFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=(ExtObject as TDataSet).IndexOfField(Info.ParamAsString[0]);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsNextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TDataSet).Intf.Next;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStepEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=(ExtObject as TDataSet).Step;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyAllEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TDataSet).StringifyAll(Info.ParamAsInteger[0]);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TDataSet).Stringify;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyMapEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TDataSet).StringifyMap(Info.ParamAsInteger[0]);
end;

function TdwsDatabaseLib.dwsDatabaseFunctionsBlobHexParameterFastEval(
  const args: TExprBaseListExec): Variant;
begin
   Result:=dwsUtils.HexToBin(args.AsString[0]);
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
