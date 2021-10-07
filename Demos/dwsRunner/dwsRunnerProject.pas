unit dwsRunnerProject;

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsXPlatform, dwsComp, dwsUnitSymbols,
   SynZip;

type
   TRunnerProject = class
      public
         function  Attach(script : TDelphiWebScript) : String; virtual;
         function  TryLoad(const fileName : String; var source : String) : Boolean; virtual; abstract;
         procedure DoInclude(const scriptName: String; var scriptSource: String);
         function  DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;
   end;

   TZipProject = class(TRunnerProject)
      private
         FZip : TZipRead;

      public
         constructor Create(const zipFile : String); overload;
         constructor Create(const zr : TZipRead); overload;
         destructor Destroy; override;

         function  Attach(script : TDelphiWebScript) : String; override;
         function  TryLoad(const fileName : String; var source : String) : Boolean; override;
   end;

   TDirectoryProject = class(TRunnerProject)
      private
         FRoot : String;

      public
         constructor Create(const aRoot : String);

         function  Attach(script : TDelphiWebScript) : String; override;
         function  TryLoad(const fileName : String; var source : String) : Boolean; override;
   end;

   TFileProject = class(TRunnerProject)
      private
         FFile : String;
         FPath : String;

      public
         constructor Create(const aFileName : String);

         function  Attach(script : TDelphiWebScript) : String; override;
         function  TryLoad(const fileName : String; var source : String) : Boolean; override;
   end;


implementation

// ------------------
// ------------------ TRunnerProject ------------------
// ------------------

// Attach
//
function TRunnerProject.Attach(script : TDelphiWebScript) : String;
begin
   script.OnNeedUnit:=DoNeedUnit;
   script.OnInclude:=DoInclude;
end;

// DoNeedUnit
//
function TRunnerProject.DoNeedUnit(const unitName : UnicodeString; var unitSource : UnicodeString) : IdwsUnit;
begin
   Result:=nil;
   DoInclude(unitName, unitSource);
end;

// DoInclude
//
procedure TRunnerProject.DoInclude(const scriptName: String; var scriptSource: String);
begin
   if TryLoad(scriptName, scriptSource) then Exit;
   if TryLoad(scriptName+'.pas', scriptSource) then Exit;
   raise Exception.CreateFmt('Dependency not found: "%s"', [scriptName]);
end;

// ------------------
// ------------------ TDirectoryProject ------------------
// ------------------

// Create
//
constructor TZipProject.Create(const zipFile : String);
begin
   FZip := TZipRead.Create(zipFile);
end;

// Create
//
constructor TZipProject.Create(const zr : TZipRead);
begin
   FZip := zr;
end;

// Destroy
//
destructor TZipProject.Destroy;
begin
   FZip.Free;
end;

// Attach
//
function TZipProject.Attach(script : TDelphiWebScript) : String;
begin
   inherited Attach(script);
   if FZip.Count=1 then
      DoInclude(FZip.Entry[0].zipName, Result)
   else DoInclude('main', Result);
end;

// TryLoad
//
function TZipProject.TryLoad(const fileName : String; var source : String) : Boolean;
var
   i : Integer;
begin
   i:=FZip.NameToIndex(fileName);
   Result:=(i>=0);
   if Result then
      source:=LoadTextFromRawBytes(FZip.UnZip(i))
end;

// ------------------
// ------------------ TDirectoryProject ------------------
// ------------------

// Create
//
constructor TDirectoryProject.Create(const aRoot : String);
begin
   FRoot:=IncludeTrailingPathDelimiter(aRoot);
end;

// Attach
//
function TDirectoryProject.Attach(script : TDelphiWebScript) : String;
begin
   inherited Attach(script);
   Result:=LoadTextFromFile(FRoot+'main.pas');
end;

// TryLoad
//
function TDirectoryProject.TryLoad(const fileName : String; var source : String) : Boolean;
begin
   source:=LoadTextFromFile(fileName);
   Result:=(source<>'');
end;

// ------------------
// ------------------ TFileProject ------------------
// ------------------

// Create
//
constructor TFileProject.Create(const aFileName : String);
begin
   FFile:=aFileName;
   FPath:=IncludeTrailingPathDelimiter(ExtractFilePath(aFileName));
end;

// Attach
//
function TFileProject.Attach(script : TDelphiWebScript) : String;
begin
   inherited Attach(script);
   Result:=LoadTextFromFile(FFile);
end;

// TryLoad
//
function TFileProject.TryLoad(const fileName : String; var source : String) : Boolean;
begin
   source:=LoadTextFromFile(FPath+fileName);
   Result:=(source<>'');
end;

end.
