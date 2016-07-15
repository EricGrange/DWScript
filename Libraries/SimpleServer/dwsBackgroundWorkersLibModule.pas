unit dwsBackgroundWorkersLibModule;

interface

uses
  SysUtils, Classes,
  dwsExprs, dwsComp, dwsUtils, dwsXPlatform,
  dwsIOCPWorkerThreadPool, dwsWebEnvironment;

type
   TBackgroundWorkEvent = procedure (const request : TWebRequest) of object;

   TWorkQueues = TSimpleNameObjectHash<TIOCPWorkerThreadPool>;

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
  private
    { Private declarations }
    FOnBackgroundWork : TBackgroundWorkEvent;
    FPools : TWorkQueues;
    FPoolsCS : TMultiReadSingleWrite;

  public
    { Public declarations }
    property OnBackgroundWork : TBackgroundWorkEvent read FOnBackgroundWork write FOnBackgroundWork;
  end;

implementation

{$R *.dfm}

type
   TWorkWebRequest = class (TWebRequest)
      private
         FHeaders : TStrings;

      protected
         function  GetHeaders : TStrings; override;

      public
         Task : String;
         Data : RawByteString;

         destructor Destroy; override;

         function RemoteIP : String; override;

         function RawURL : RawByteString; override;
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

// Destroy
//
destructor TWorkWebRequest.Destroy;
begin
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
function TWorkWebRequest.RawURL : RawByteString;
begin
   Result:=UTF8Encode(Task);
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
   FPoolsCS:=TMultiReadSingleWrite.Create;
   FPools:=TWorkQueues.Create;
end;

procedure TdwsBackgroundWorkersLib.DataModuleDestroy(Sender: TObject);
begin
   FOnBackgroundWork:=nil;
   FPools.Clean;
   FreeAndNil(FPools);
   FreeAndNil(FPoolsCS);
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsCreateWorkQueueEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
begin
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
   if pool<>nil then
      pool.Free;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueSizeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   n : Integer;
begin
   n:=0;
   name:=Info.ParamAsString[0];
   FPoolsCS.BeginRead;
   try
      pool:=FPools[name];
      if pool<>nil then
         n:=pool.QueueSize;
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
   name:=Info.ParamAsString[0];

   workUnit:=TWorkWebRequest.Create;
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
   name:=Info.ParamAsString[0];

   delayMilliseconds := Round(Info.ParamAsFloat[1] * 1000);

   workUnit:=TWorkWebRequest.Create;
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

end.
