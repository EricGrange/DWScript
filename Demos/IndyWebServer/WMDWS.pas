unit WMDWS;

interface

uses
  SysUtils, Classes, HTTPApp, dwsComp, dwsExprs, dwsGlobalVarsFunctions,
  dwsCompiler, dwsHtmlFilter, dwsMathFunctions, dwsTimeFunctions, DSimpleDWScript,
  dwsFileSystem, dwsUtils;

type
  TWebModuleDWS = class(TWebModule)
    dwsGlobalVarsFunctions: TdwsGlobalVarsFunctions;
    RestrictedFileSystem: TdwsRestrictedFileSystem;
    procedure WebModuleDWSDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FBasePath : String;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModuleDWS;

implementation

{$R *.dfm}

procedure TWebModuleDWS.WebModuleCreate(Sender: TObject);
begin
   FBasePath:=ExtractFilePath(ParamStr(0));
   if FileExists(ChangeFileExt(ParamStr(0), '.dpr')) then
      FBasePath:=FBasePath+'..\Data\www' // if compiled alongside dpr
   else FBasePath:=FBasePath+'..\..\..\Data\www'; // assume compiled in platform/target
   FBasePath:=IncludeTrailingPathDelimiter(ExpandFileName(FBasePath));

   RestrictedFileSystem.Paths.Text:=FBasePath;

   SimpleDWScript:=TSimpleDWScript.Create(Self);
   SimpleDWScript.FileSystem:=RestrictedFileSystem;
end;

procedure TWebModuleDWS.WebModuleDWSDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
   fileName, ext : String;
begin
   Handled:=True;

   // dirty conversion, not particularly quick
   // lowercase because TDictionary is case-sensitive
   fileName:=AnsiLowerCase(String(Request.PathInfo));
   fileName:=StringReplace(fileName, '/', '\', [rfReplaceAll]); // $0.02 of normalization

   if fileName='\' then
      fileName:='\index.dws';
   fileName:=ExpandFileName(FBasePath+fileName);

   if not (StrBeginsWith(fileName, FBasePath) and FileExists(fileName)) then begin
      Response.Content:='Error 404: not found';
      Response.StatusCode:=404;
      Exit;
   end;

   Response.StatusCode:=200;
   ext:=LowerCase(ExtractFileExt(fileName));
   if ext='.dws' then
      SimpleDWScript.HandleDWS(fileName, Request, Response)
   // some utility handler
   else if ext='.js' then begin
      Response.ContentType:='text/javascript';
   end else if ext='.css' then begin
      Response.ContentType:='text/css';
   end else if ext='.htm' then begin
      Response.ContentType:='text/html';
   end else if ext='.png' then begin
      Response.ContentType:='image/png';
   end else if ext='.jpg' then begin
      Response.ContentType:='image/jpeg';
   end else begin
      // should be 400, but no point hinting the file exists ;)
      Response.Content:='Error 404: not found';
      Response.StatusCode:=404;
   end;
   if Response.StatusCode=200 then
      Response.ContentStream:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyWrite);
end;

end.
