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
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsRunnerProject;

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsXPlatform, dwsComp, dwsUnitSymbols,
   SynZip;

type
   TRunnerProject = class
      private
         FIncludePaths : TStringList;
      public
         constructor Create;
         destructor Destroy; override;

         property IncludePaths : TStringList read FIncludePaths;

         function  Attach(script : TDelphiWebScript) : String; virtual;
         function  TryLoad(const fileName : String; var source, location : String) : Boolean; virtual; abstract;
         procedure DoIncludeEx(const scriptName: String; var scriptSource, scriptLocation: String);
         function  DoNeedUnitEx(const unitName : String; var unitSource, unitLocation : String) : IdwsUnit;
   end;

   TZipProject = class(TRunnerProject)
      private
         FZip : TZipRead;

      public
         constructor Create(const zipFile : String); overload;
         constructor Create(const zr : TZipRead); overload;
         destructor Destroy; override;

         function  Attach(script : TDelphiWebScript) : String; override;
         function  TryLoad(const fileName : String; var source, location : String) : Boolean; override;
   end;

   TDirectoryProject = class(TRunnerProject)
      private
         FRoot : String;

      public
         constructor Create(const aRoot : String);

         function  Attach(script : TDelphiWebScript) : String; override;
         function  TryLoad(const fileName : String; var source, location : String) : Boolean; override;
   end;

   TFileProject = class(TRunnerProject)
      private
         FFile : String;
         FPath : String;

      public
         constructor Create(const aFileName : String);

         function  Attach(script : TDelphiWebScript) : String; override;
         function  TryLoad(const fileName : String; var source, location : String) : Boolean; override;
   end;

   TStringProject = class(TRunnerProject)
      private
         FSource : String;
         FPath : String;

      public
         constructor Create(const aSource : String);

         function  Attach(script : TDelphiWebScript) : String; override;
         function  TryLoad(const fileName : String; var source, location : String) : Boolean; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TRunnerProject ------------------
// ------------------

// Create
//
constructor TRunnerProject.Create;
begin
   FIncludePaths:=TStringList.Create;
end;

// Destroy
//
destructor TRunnerProject.Destroy;
begin
   FIncludePaths.Free;
   inherited;
end;

// Attach
//
function TRunnerProject.Attach(script : TDelphiWebScript) : String;
begin
   script.OnNeedUnitEx:=DoNeedUnitEx;
   script.OnIncludeEx:=DoIncludeEx;
end;

// DoNeedUnitEx
//
function TRunnerProject.DoNeedUnitEx(const unitName : String; var unitSource, unitLocation : String) : IdwsUnit;
begin
   Result:=nil;
   DoIncludeEx(unitName, unitSource, unitLocation);
end;

// DoIncludeEx
//
procedure TRunnerProject.DoIncludeEx(const scriptName: String; var scriptSource, scriptLocation: String);
var
   i : Integer;
   path : String;
begin
   if TryLoad(scriptName, scriptSource, scriptLocation) then Exit;
   if TryLoad(scriptName+'.pas', scriptSource, scriptLocation) then Exit;

   for i:=0 to FIncludePaths.Count-1 do begin
      path:=IncludeTrailingPathDelimiter(FIncludePaths[i]);
      if FileExists(path+scriptName) then begin
         scriptLocation:=path+scriptName;
         scriptSource:=LoadTextFromFile(scriptLocation);
         Exit;
      end;
      if FileExists(path+scriptName+'.pas') then begin
         scriptLocation:=path+scriptName+'.pas';
         scriptSource:=LoadTextFromFile(scriptLocation);
         Exit;
      end;
   end;

   raise Exception.CreateFmt('Dependency not found: "%s"', [scriptName]);
end;

// ------------------
// ------------------ TZipProject ------------------
// ------------------

// Create
//
constructor TZipProject.Create(const zipFile : String);
begin
   inherited Create;
   FZip := TZipRead.Create(zipFile);
end;

// Create
//
constructor TZipProject.Create(const zr : TZipRead);
begin
   inherited Create;
   FZip := zr;
end;

// Destroy
//
destructor TZipProject.Destroy;
begin
   FZip.Free;
   inherited;
end;

// Attach
//
function TZipProject.Attach(script : TDelphiWebScript) : String;
var
   location : String;
begin
   inherited Attach(script);
   if FZip.Count=1 then
      DoIncludeEx(FZip.Entry[0].zipName, Result, location)
   else DoIncludeEx('main', Result, location);
end;

// TryLoad
//
function TZipProject.TryLoad(const fileName : String; var source, location : String) : Boolean;
var
   i : Integer;
begin
   i:=FZip.NameToIndex(fileName);
   Result:=(i>=0);
   if Result then begin
      source:=LoadTextFromRawBytes(FZip.UnZip(i));
      location:='zip:'+fileName;
   end;
end;

// ------------------
// ------------------ TDirectoryProject ------------------
// ------------------

// Create
//
constructor TDirectoryProject.Create(const aRoot : String);
begin
   inherited Create;
   FRoot:=IncludeTrailingPathDelimiter(aRoot);
end;

// Attach
//
function TDirectoryProject.Attach(script : TDelphiWebScript) : String;
var
   location : String;
begin
   inherited Attach(script);
   DoIncludeEx('main', Result, location);
end;

// TryLoad
//
function TDirectoryProject.TryLoad(const fileName : String; var source, location : String) : Boolean;
begin
   location:=FRoot+fileName;
   if not FileExists(location) then begin
      location:=FRoot+fileName+'.pas';
      if not FileExists(location) then begin
         Result:=False;
         Exit;
      end;
   end;
   source:=LoadTextFromFile(location);
   Result:=(source<>'');
end;

// ------------------
// ------------------ TFileProject ------------------
// ------------------

// Create
//
constructor TFileProject.Create(const aFileName : String);
begin
   inherited Create;
   FFile:=aFileName;
   FPath:=IncludeTrailingPathDelimiter(ExtractFilePath(aFileName));
end;

// Attach
//
function TFileProject.Attach(script : TDelphiWebScript) : String;
var
   location : String;
begin
   inherited Attach(script);
   location:=FFile;
   Result:=LoadTextFromFile(location);
end;

// TryLoad
//
function TFileProject.TryLoad(const fileName : String; var source, location : String) : Boolean;
begin
   location:=FPath+fileName;
   source:=LoadTextFromFile(location);
   Result:=(source<>'');
end;

// ------------------
// ------------------ TStringProject ------------------
// ------------------

// Create
//
constructor TStringProject.Create(const aSource : String);
begin
   inherited Create;
   FSource:=aSource;
   FPath:=IncludeTrailingPathDelimiter(GetCurrentDir);
end;

// Attach
//
function TStringProject.Attach(script : TDelphiWebScript) : String;
begin
   inherited Attach(script);
   Result:=FSource;
end;

// TryLoad
//
function TStringProject.TryLoad(const fileName : String; var source, location : String) : Boolean;
begin
   location:=FPath+fileName;
   source:=LoadTextFromFile(location);
   Result:=(source<>'');
end;

end.
