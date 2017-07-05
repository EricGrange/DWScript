unit dwsBackgroundWorkersLibModule;

interface

uses
  SysUtils, Classes,
  dwsExprs, dwsComp, dwsUtils, dwsXPlatform,
  dwsIOCPWorkerThreadPool, dwsWebEnvironment, dwsJSON;

const
   cDefaultMaxWorkersPerQueue = 16;

type
   TBackgroundWorkEvent = procedure (const request : TWebRequest) of object;

   TWorkQueues = TSimpleNameObjectHash<TIOCPWorkerThreadPool>;

   TdwsBackgroundWorkersLib = class;

   TWorkWebRequest = class (TWebRequest)
      private
         FHeaders : TStrings;
         FOwner : TdwsBackgroundWorkersLib;
         FPrev, FNext : TWorkWebRequest;

      protected
         function  GetHeaders : TStrings; override;

      public
         Task : String;
         Data : RawByteString;

         constructor Create(aModule : TdwsBackgroundWorkersLib);
         destructor Destroy; override;

         function RemoteIP : String; override;

         function RawURL : String; override;
         function URL : String; override;
         function FullURL : String; override;
         function Method : String; override;
         function MethodVerb : TWebRequestMethodVerb; override;

         function Security : String; override;
         function Secure : Boolean; override;

         function ContentLength : Integer; override;
         function ContentData : RawByteString; override;
         function ContentType : RawByteString; override;

         procedure Execute(Sender : TObject);
   end;

  TdwsBackgroundWorkersLib = class(TDataModule)
    dwsBackgroundWorkers: TdwsUnit;
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsCreateWorkQueueEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsDestroyWorkQueueEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueWorkEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueSizeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueDelayedWorkEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsGetWorkerCountEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsSetWorkerCountEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueStatusAsJSONEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
    FOnBackgroundWork : TBackgroundWorkEvent;
    FPools : TWorkQueues;
    FPoolsCS : TMultiReadSingleWrite;
    FWorkUnitHead : TWorkWebRequest;
    FWorkUnitLock : TMultiReadSingleWrite;

    FMaxWorkersPerQueue : Integer;

  public
    { Public declarations }
    property OnBackgroundWork : TBackgroundWorkEvent read FOnBackgroundWork write FOnBackgroundWork;
    property MaxWorkersPerQueue : Integer read FMaxWorkersPerQueue write FMaxWorkersPerQueue;
  end;

implementation

{$R *.dfm}

// Create
//
constructor TWorkWebRequest.Create(aModule : TdwsBackgroundWorkersLib);
begin
   inherited Create;
   aModule.FWorkUnitLock.BeginWrite;
   try
      FOwner := aModule;
      FNext := aModule.FWorkUnitHead;
      if FNext <> nil then
         FNext.FPrev := Self;
      aModule.FWorkUnitHead := Self;
   finally
      aModule.FWorkUnitLock.EndWrite;
   end;
end;

// Destroy
//
destructor TWorkWebRequest.Destroy;
begin
   FOwner.FWorkUnitLock.BeginWrite;
   try
      if FPrev = nil then
         FOwner.FWorkUnitHead := FNext
      else FPrev.FNext := FNext;
      if FNext <> nil then
         FNext.FPrev := FPrev;
   finally
      FOwner.FWorkUnitLock.EndWrite;
   end;
   FHeaders.Free;
   inherited;
end;

// GetHeaders
//
function TWorkWebRequest.GetHeaders : TStrings;
begin
   if FHeaders=nil then
      FHeaders:=TStringList.Create;
   Result:=FHeaders;
end;

// RemoteIP
//
function TWorkWebRequest.RemoteIP : String;
begin
   Result:='127.0.0.1';
end;

// RawURL
//
function TWorkWebRequest.RawURL : String;
begin
   Result := Task;
end;

// URL
//
function TWorkWebRequest.URL : String;
begin
   Result:=Task;
end;

// FullURL
//
function TWorkWebRequest.FullURL : String;
begin
   Result := 'worker:' + Task;
end;

// Method
//
function TWorkWebRequest.Method : String;
begin
   Result:='POST';
end;

// MethodVerb
//
function TWorkWebRequest.MethodVerb : TWebRequestMethodVerb;
begin
   Result:=wrmvPOST
end;

// Security
//
function TWorkWebRequest.Security : String;
begin
   Result:='';
end;

// Secure
//
function TWorkWebRequest.Secure : Boolean;
begin
   Result:=False;
end;

// ContentLength
//
function TWorkWebRequest.ContentLength : Integer;
begin
   Result:=Length(Data);
end;

// ContentData
//
function TWorkWebRequest.ContentData : RawByteString;
begin
   Result:=Data;
end;

// ContentType
//
function TWorkWebRequest.ContentType : RawByteString;
begin
   Result:='application/octet-stream';
end;

// Execute
//
procedure TWorkWebRequest.Execute(Sender : TObject);
var
   lib : TdwsBackgroundWorkersLib;
begin
   lib:=(Sender as TdwsBackgroundWorkersLib);
   try
      if Assigned(lib.FOnBackgroundWork) then
         lib.FOnBackgroundWork(Self);
   finally
      Free;
   end;
end;

procedure TdwsBackgroundWorkersLib.DataModuleCreate(Sender: TObject);
begin
   FPoolsCS := TMultiReadSingleWrite.Create;
   FPools := TWorkQueues.Create;
   FMaxWorkersPerQueue := cDefaultMaxWorkersPerQueue;
   FWorkUnitLock := TMultiReadSingleWrite.Create;
end;

procedure TdwsBackgroundWorkersLib.DataModuleDestroy(Sender: TObject);
var
   i : Integer;
begin
   FOnBackgroundWork := nil;

   // remove all workers gracefully
   for i := 0 to FPools.HighIndex do
      if FPools.BucketObject[i] <> nil then
         FPools.BucketObject[i].WorkerCount := 0;
   // wait for completion
   for i := 0 to FPools.HighIndex do
      if FPools.BucketObject[i] <> nil then
         FPools.BucketObject[i].Shutdown;

   FPools.Clean;
   FreeAndNil(FPools);
   FreeAndNil(FPoolsCS);

   while FWorkUnitHead <> nil do
      FWorkUnitHead.Destroy;
   FreeAndNil(FWorkUnitLock);
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsCreateWorkQueueEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
begin
   if not Assigned(FOnBackgroundWork) then
      raise Exception.Create('Cannot create workers during shutdown');

   name:=Info.ParamAsString[0];
   FPoolsCS.BeginWrite;
   try
      pool:=FPools[name];
      Info.ResultAsBoolean:=(pool=nil);
      if pool=nil then begin
         pool:=TIOCPWorkerThreadPool.Create(1);
         FPools[name]:=pool;
      end;
   finally
      FPoolsCS.EndWrite;
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsDestroyWorkQueueEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
begin
   if not Assigned(FOnBackgroundWork) then Exit;

   name:=Info.ParamAsString[0];
   FPoolsCS.BeginWrite;
   try
      pool:=FPools[name];
      if pool<>nil then
         FPools[name]:=nil;
   finally
      FPoolsCS.EndWrite;
   end;
   Info.ResultAsBoolean:=(pool<>nil);
   if pool<>nil then begin
      pool.Shutdown;
      pool.Free;
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueSizeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   n : Integer;
begin
   name := Info.ParamAsString[0];
   FPoolsCS.BeginRead;
   try
      pool:=FPools[name];
      if pool<>nil then
         n := pool.QueueSize
      else n := 0;
   finally
      FPoolsCS.EndRead;
   end;
   Info.ResultAsInteger:=n;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueWorkEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   workUnit : TWorkWebRequest;
   pool : TIOCPWorkerThreadPool;
begin
   if not Assigned(FOnBackgroundWork) then Exit;

   name:=Info.ParamAsString[0];

   workUnit:=TWorkWebRequest.Create(Self);
   workUnit.Task:=Info.ParamAsString[1];
   workUnit.Data:=Info.ParamAsDataString[2];

   FPoolsCS.BeginRead;
   try
      pool:=FPools[name];
      if pool<>nil then
         pool.QueueWork(workUnit.Execute, Self);
   finally
      FPoolsCS.EndRead;
   end;

   if pool=nil then begin
      workUnit.Free;
      raise Exception.CreateFmt('Unknown Work Queue "%s"', [name]);
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueDelayedWorkEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   workUnit : TWorkWebRequest;
   pool : TIOCPWorkerThreadPool;
   delayMilliseconds : Integer;
begin
   if not Assigned(FOnBackgroundWork) then Exit;

   name:=Info.ParamAsString[0];

   delayMilliseconds := Round(Info.ParamAsFloat[1] * 1000);

   workUnit:=TWorkWebRequest.Create(Self);
   workUnit.Task:=Info.ParamAsString[2];
   workUnit.Data:=Info.ParamAsDataString[3];

   FPoolsCS.BeginRead;
   try
      pool:=FPools[name];
      if pool<>nil then
         pool.QueueDelayedWork(delayMilliseconds, workUnit.Execute, Self);
   finally
      FPoolsCS.EndRead;
   end;

   if pool=nil then begin
      workUnit.Free;
      raise Exception.CreateFmt('Unknown Work Queue "%s"', [name]);
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsGetWorkerCountEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   n : Integer;
begin
   name := Info.ParamAsString[0];
   FPoolsCS.BeginRead;
   try
      pool := FPools[name];
      if pool <> nil then
         n := pool.WorkerCount
      else n := 0;
   finally
      FPoolsCS.EndRead;
   end;
   Info.ResultAsInteger := n;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsSetWorkerCountEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   n : Integer;
begin
   name := Info.ParamAsString[0];
   n := Info.ParamAsInteger[1];
   if n <= 0 then
      raise Exception.CreateFmt('WorkerCount value must be strictly positive (got %d)', [n]);
   if n > MaxWorkersPerQueue then
      raise Exception.CreateFmt('WorkerCount value too high (got %d, must be <= %d)', [n, MaxWorkersPerQueue]);
   if (n <> 0) and not Assigned(FOnBackgroundWork) then
      raise Exception.Create('Cannot create workers during shutdown');
   FPoolsCS.BeginRead;
   try
      pool := FPools[name];
      if pool <> nil then
         pool.WorkerCount := n;
   finally
      FPoolsCS.EndRead;
   end;
   if pool=nil then
      raise Exception.CreateFmt('Unknown Work Queue "%s"', [name]);
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueStatusAsJSONEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   wr : TdwsJSONWriter;
   sizeInfo : TWorkerThreadQueueSizeInfo;
begin
   name := Info.ParamAsString[0];
   FPoolsCS.BeginRead;
   try
      pool := FPools[name];
   finally
      FPoolsCS.EndRead;
   end;
   wr := TdwsJSONWriter.Create;
   try
      wr.BeginObject;
         wr.WriteString('name', name);
         if pool <> nil then begin
            wr.BeginObject('workers');
               wr.WriteInteger('count', pool.WorkerCount);
               wr.WriteInteger('live', pool.LiveWorkerCount);
               wr.WriteInteger('active', pool.ActiveWorkerCount);
            wr.EndObject;
            wr.BeginObject('queue');
               sizeInfo := pool.QueueSizeInfo;
               wr.WriteInteger('total', sizeInfo.Total);
               wr.WriteInteger('delayed', sizeInfo.Delayed);
               wr.WriteInteger('peak', sizeInfo.Peak);
            wr.EndObject;
         end;
      wr.EndObject;
      Info.ResultAsString := wr.ToString;
   finally
      wr.Free;
   end;
end;

end.
