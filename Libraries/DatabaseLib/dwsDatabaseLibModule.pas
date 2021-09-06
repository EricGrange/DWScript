unit dwsDatabaseLibModule;

interface

uses
  SysUtils, Classes, Masks,
  dwsStrings, dwsUtils, dwsExprList, dwsXPlatform, dwsInfo, dwsFileSystem,
  dwsComp, dwsExprs, dwsSymbols, dwsStack, dwsDatabase, dwsJSON, dwsErrors;

type

  TdwsDatabaseLib = class(TDataModule)
    dwsDatabase: TdwsUnit;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    procedure dwsDatabaseClassesDataBaseConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseCleanUp(ExternalObject: TObject);

    procedure dwsDatabaseClassesDataBaseMethodsExecEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsQueryEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsGetFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsFieldByNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataSetMethodsFindFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsDatabaseClassesDataBaseMethodsVersionInfoTextEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseFunctionsBlobParameterFastEval(
      const args: TExprBaseListExec): Variant;
    function dwsDatabaseClassesDataBasePoolMethodsAcquireFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    procedure dwsDatabaseClassesDataBasePoolMethodsReleaseEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsDatabaseFunctionsBlobHexParameterFastEval(
      const args: TExprBaseListExec): Variant;
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
    function dwsDatabaseClassesDataSetMethodsStepFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    procedure dwsDatabaseClassesDataFieldMethodsNameFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsDatabaseClassesDataSetMethodsFieldCountFastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    procedure dwsDatabaseClassesDataSetMethodsAsString_String_FastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsDatabaseClassesDataBaseMethodsBeginTransactionFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsDatabaseClassesDataBaseMethodsCommitFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsDatabaseClassesDataBaseMethodsRollbackFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    function dwsDatabaseClassesDataBaseMethodsInTransactionFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    function dwsDatabaseClassesDataSetMethodsEofFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    procedure dwsDatabaseClassesDataSetMethodsNextFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    function dwsDatabaseClassesDataSetMethodsIndexOfFieldFastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    procedure dwsDatabaseClassesDataSetMethodsAsString_Integer_FastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsDatabaseClassesDataSetMethodsAsInteger_String_FastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    function dwsDatabaseClassesDataSetMethodsAsInteger_Integer_FastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    function dwsDatabaseClassesDataSetMethodsAsFloat_String_FastEvalFloat(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
    function dwsDatabaseClassesDataSetMethodsAsFloat_Integer_FastEvalFloat(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
    procedure dwsDatabaseClassesDataSetMethodsAsBlob_String_FastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsDatabaseClassesDataSetMethodsAsBlob_Integer_FastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsDatabaseClassesDataSetMethodsIsNull_String_FastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    function dwsDatabaseClassesDataSetMethodsIsNull_Integer_FastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    procedure dwsDatabaseClassesDataSetMethodsStringifyFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsDatabaseClassesDataSetMethodsStringifyAllFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsDatabaseClassesDataSetMethodsStringifyMapFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsDatabaseClassesDataFieldMethodsDataTypeFastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    procedure dwsDatabaseClassesDataFieldMethodsDeclaredTypeFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsDatabaseClassesDataFieldMethodsIsNullFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    procedure dwsDatabaseClassesDataFieldMethodsAsStringFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsDatabaseClassesDataFieldMethodsAsIntegerFastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    function dwsDatabaseClassesDataFieldMethodsAsFloatFastEvalFloat(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
    function dwsDatabaseClassesDataFieldMethodsAsBooleanFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    procedure dwsDatabaseClassesDataFieldMethodsAsBlobFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsDatabaseClassesDataBasePoolMethodsCleanupFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    function dwsDatabaseClassesDataBasePoolMethodsCountFastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    procedure dwsDatabaseClassesDataSetMethodsToSeparatedFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
  private
    { Private declarations }
    FFileSystem : IdwsFileSystem;

    procedure SetScript(aScript : TDelphiWebScript);
    procedure RaiseDBException(Info: TProgramInfo; const msg : String);

  public
    { Public declarations }
    class procedure CleanupDataBasePool(const filter : String = '*');
    function CountPooledDataBases(const filter : String = '*') : Integer;

    property Script : TDelphiWebScript write SetScript;
    property FileSystem : IdwsFileSystem read FFileSystem write FFileSystem;
  end;

   TScriptDataBase = class
      Intf : IdwsDataBase;
      LowerCaseStringify : Boolean;
   end;

   TScriptDataSet = class
      Intf : IdwsDataSet;
   end;

implementation

{$R *.dfm}

uses dwsRandom;

resourcestring
   FIELD_NOT_FOUND = 'Field ''%s'' not found';

type
   TDataSet = class (TScriptDataSet)
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
      function ToSeparated(maxRows : Integer; const separator, quoteChar : String) : String;
      procedure PrepareScriptFields(programInfo : TProgramInfo; var fieldsInfo : IInfo);
      class procedure NeedScriptFields(programInfo : TProgramInfo; extObject: TObject; var fieldsInfo : IInfo);
   end;

   TDataField = class
      Intf : IdwsDataField;
   end;

    TDataBaseQueue = TSimpleQueue<IdwsDataBase>;

var
   vPools : TSimpleNameObjectHash<TDataBaseQueue>;
   vPoolsCS : TMultiReadSingleWrite;
   vPoolsCount : Integer;

// NotifyDataSetCreate
//
function NotifyDataSetCreate(info : TProgramInfo) : NativeUInt;
begin
   Result := TdwsDataSet.NotifyCreate(Info.Execution.CallStackLastExpr.ScriptPos.AsInfo);
end;

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

   procedure ProcessString;
   var
      buf : String;
   begin
      fld.GetAsString(buf);
      wr.WriteString(buf);
   end;

begin
   case fld.DataType of
      dftInteger : wr.WriteInteger(fld.AsInteger);
      dftFloat : wr.WriteNumber(fld.AsFloat);
      dftString, dftBlob : ProcessString;
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
   buf : String;
begin
   wr:=TdwsJSONWriter.Create(nil, WriterOptions);
   try
      wr.BeginObject;
      while not Intf.EOF do begin
         Intf.Fields[0].GetAsString(buf);
         wr.WriteName(buf);
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

// ToSeparated
//
function TDataSet.ToSeparated(maxRows : Integer; const separator, quoteChar : String) : String;
var
   wobs : TWriteOnlyBlockStream;
   i : Integer;
   s, doubleQuote : String;
   fields : array of IdwsDataField;
   needQuoteChars : array [ 32..127 ] of Boolean;

   procedure WriteQuoted;
   begin
      wobs.WriteString(quoteChar);
      FastStringReplace(s, quoteChar, doubleQuote);
      wobs.WriteString(s);
      wobs.WriteString(quoteChar);
   end;

   procedure WriteQuotedIfNecessary;
   var
      i : Integer;
      p : PChar;
   begin
      p := Pointer(s);
      for i := 1 to Length(s) do begin
         case Ord(p[i-1]) of
            Low(needQuoteChars)..High(needQuoteChars) :
               if needQuoteChars[Ord(p[i-1])] then begin
                  WriteQuoted;
                  Exit;
               end;
         else
            WriteQuoted;
            Exit;
         end;
      end;
      wobs.WriteString(s);
   end;

begin
   // prepare needQuoteChars
   FillChar(needQuoteChars[Low(needQuoteChars)], SizeOf(needQuoteChars), 0);
   if separator <> '' then case separator[1] of
      #32..#127 : needQuoteChars[Ord(separator[1])] := True;
   end;
   if quoteChar <> '' then case quoteChar[1] of
      #32..#127 : needQuoteChars[Ord(quoteChar[1])] := True;
   end;
   doubleQuote := quoteChar + quoteChar;

   // prepare local fields array
   SetLength(fields, Intf.FieldCount);
   for i := 0 to High(fields) do
      fields[i] := Intf.Fields[i];

   wobs := TWriteOnlyBlockStream.AllocFromPool;
   try
      for i := 0 to High(fields) do begin
         if i > 0 then
            wobs.WriteString(separator);
         s := fields[i].Name;
         WriteQuotedIfNecessary;
      end;
      wobs.WriteCRLF;
      while not Intf.EOF do begin
         for i := 0 to High(fields) do begin
            if i > 0 then
               wobs.WriteString(separator);
            fields[i].GetAsString(s);
            WriteQuotedIfNecessary;
         end;
         wobs.WriteCRLF;
         Intf.Next;
         Dec(maxRows);
         if maxRows=0 then break;
      end;
      Result := wobs.ToString;
   finally
      wobs.ReturnToPool;
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
      dataFieldsArray.SetAsVariant(i, dataFieldInfo.Value);
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
   name := Info.ParamAsString[0];
   obj := Info.ParamAsObject[1];
   if obj is TScriptDataBase then begin
      db := TScriptDataBase(obj).Intf;
      checkRelease := db.CanReleaseToPool;
      if checkRelease <> '' then begin
         checkRelease := 'Releasing to pool not allowed: ' + checkRelease;
         RaiseDBException(Info, checkRelease);
      end;
   end;
   nb := Info.ParamAsInteger[2];
   Info.ParamAsVariant[1] := IUnknown(nil);

   vPoolsCS.BeginWrite;
   try
      q := vPools[name];
      if (nb > 0) and (db <> nil) then begin
         if q = nil then begin
            q := TSimpleQueue<IdwsDataBase>.Create;
            vPools[name] := q;
         end;
         q.Push(db);
      end;
      if q <> nil then begin
         while q.Count > nb do
            q.Pull(db);
      end;
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

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsCleanupFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   CleanupDataBasePool(args.AsString[0]);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBasePoolMethodsCountFastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
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
   scriptDyn := Info.ParamAsScriptDynArray[1];
   db := TdwsDatabase.CreateDataBase(Info.ParamAsString[0], scriptDyn.ToStringArray, FileSystem);

   ExtObject := TScriptDataBase.Create;
   TScriptDataBase(ExtObject).Intf:=db;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsBeginTransactionFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TScriptDataBase).Intf.BeginTransaction;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsCommitFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TScriptDataBase).Intf.Commit;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsRollbackFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TScriptDataBase).Intf.Rollback;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsInTransactionFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TScriptDataBase).Intf.InTransaction;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsExecEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   scriptDyn : IScriptDynArray;
   db : IdwsDataBase;
   dsID : NativeUInt;
begin
   scriptDyn := Info.ParamAsScriptDynArray[1];

   if TdwsDataSet.CallbacksRegistered then
      dsID := NotifyDataSetCreate(Info)
   else dsID := 0;
   try
      db := (ExtObject as TScriptDataBase).Intf;
      db.Exec(Info.ParamAsString[0], scriptDyn, Info.Execution.CallStackLastExpr);
   finally
      TdwsDataSet.NotifyDestroy(dsID);
   end;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataBaseMethodsQueryEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   scriptDyn : IScriptDynArray;
   ids : IdwsDataSet;
   dbo : TScriptDataBase;
   dataSetInfo : IInfo;
   dataSet : TDataSet;
   dsID : NativeUInt;
begin
   scriptDyn:=Info.ParamAsScriptDynArray[1];

   dbo := (ExtObject as TScriptDataBase);

   if TdwsDataSet.CallbacksRegistered then
      dsID := NotifyDataSetCreate(Info)
   else dsID := 0;
   try
      ids := dbo.Intf.Query(Info.ParamAsString[0], scriptDyn, Info.Execution.CallStackLastExpr);
   except
      TdwsDataSet.NotifyDestroy(dsID);
      raise;
   end;
   ids.ID := dsID;

   dataSetInfo := Info.Vars['DataSet'].Method['Create'].Call;

   dataSet:=TDataSet.Create;
   dataSet.Intf := ids;
   if dbo.LowerCaseStringify then
      dataSet.WriterOptions := [woLowerCaseNames];

   dataSetInfo.ExternalObject := dataSet;

   Info.ResultAsVariant := dataSetInfo.Value;
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

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsBlobFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   RawByteStringToScriptString((baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsBlob, result);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsBooleanFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsBoolean;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsFloatFastEvalFloat(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsFloat;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsIntegerFastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.AsInteger;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsAsStringFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.GetAsString(result);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsDataTypeFastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
begin
   Result := Ord((baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.DataType);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsDeclaredTypeFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.DeclaredType;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsIsNullFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.IsNull;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataFieldMethodsNameFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataField).Intf.Name;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsString_Integer_FastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet)
      .Intf
      .GetStringField(args.AsInteger[0], result);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsString_String_FastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet)
      .FieldByName(args)
      .GetAsString(result);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsInteger_Integer_FastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet)
      .Intf
      .GetIntegerField(args.AsInteger[0]);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsInteger_String_FastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).FieldByName(args).AsInteger;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsBlob_Integer_FastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   RawByteStringToScriptString(
      (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet)
      .Intf.GetBlobField(args.AsInteger[0]),
      result
   );
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsBlob_String_FastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   RawByteStringToScriptString((baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).FieldByName(args).AsBlob, result);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsFloat_Integer_FastEvalFloat(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet)
      .Intf
      .GetFloatField(args.AsInteger[0]);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsAsFloat_String_FastEvalFloat(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet)
      .FieldByName(args)
      .AsFloat;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIsNull_Integer_FastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet)
      .Intf
      .GetIsNullField(args.AsInteger[0]);
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIsNull_String_FastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet)
      .FieldByName(args)
      .IsNull;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsEofFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.Eof;
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

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsFieldCountFastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
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

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsIndexOfFieldFastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).IndexOfField(args.AsString[0]);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsNextFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Intf.Next;
end;

function TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStepFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Step;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyAllFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).StringifyAll(args.AsInteger[0]);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).Stringify;
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsStringifyMapFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).StringifyMap(args.AsInteger[0]);
end;

procedure TdwsDatabaseLib.dwsDatabaseClassesDataSetMethodsToSeparatedFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := (baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TDataSet).ToSeparated(
      args.AsInteger[0], args.AsString[1], args.AsString[2]
   );
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

   vPoolsCS := TMultiReadSingleWrite.Create;

finalization

   vPoolsCS.Free;
   vPoolsCS:=nil;

end.
