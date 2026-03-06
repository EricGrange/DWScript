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
program dwsRunner;

{$SetPEFlags $0001}

{$IFNDEF VER200} // delphi 2009
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  dwsUtils,
  SynZip,
  dwsRunnerProject in 'dwsRunnerProject.pas',
  dwsRunnerUtils in 'dwsRunnerUtils.pas';

var
   fileName : String;
   project : TRunnerProject;
   zr : TZipRead;
   embedded : Boolean;
   paramOffset : Integer;
   defines : TStringList;
   includePaths : TStringList;
   timeoutMs : Integer;
   interval : Integer;
   i : Integer;
   arg : String;

begin
   zr:=TZipRead.Create(HInstance, 'SCRIPT', RT_RCDATA);
   if zr.Count<>0 then begin
      project:=TZipProject.Create(zr);
      paramOffset:=1;
      embedded:=True;
      try
         RunRunner(project, paramOffset, embedded, nil, 0);
      finally
         project.Free;
      end;
      Exit;
   end;
   FreeAndNil(zr);

   defines := TStringList.Create;
   includePaths := TStringList.Create;
   try
      timeoutMs := 0;
      interval := 10;

      i := 1;
      while i <= ParamCount do begin
         arg := ParamStr(i);
         if (arg = '-I') then begin
            Inc(i);
            if i <= ParamCount then includePaths.Add(ParamStr(i));
         end else if StrBeginsWith(arg, '-I') then begin
            includePaths.Add(Copy(arg, 3, MaxInt));
         end else if (arg = '-D') then begin
            Inc(i);
            if i <= ParamCount then defines.Add(ParamStr(i));
         end else if StrBeginsWith(arg, '-D') then begin
            defines.Add(Copy(arg, 3, MaxInt));
         end else if arg = '--timeout' then begin
            Inc(i);
            if i <= ParamCount then timeoutMs := StrToIntDef(ParamStr(i), 0);
         end else if arg = '--interval' then begin
            Inc(i);
            if i <= ParamCount then interval := StrToIntDef(ParamStr(i), 10);
         end else break; 
         Inc(i);
      end;

      if i > ParamCount then begin
         WriteHeader;
         Writeln('Usage: dwsRunner [options] <sourcefile|zipfile|directory> [args]');
         Writeln('       dwsRunner [options] eval "code" [args]');
         Writeln('       dwsRunner [options] check <sourcefile|zipfile|directory>');
         Writeln('       dwsRunner [options] deps <sourcefile|zipfile|directory>');
         Writeln('       dwsRunner [options] profile <sourcefile|zipfile|directory>');
         Writeln('       dwsRunner make <sourcefile|zipfile> [exeName]');
         Writeln('');
         Writeln('Options:');
         Writeln('  -c, eval "code"      Program passed in as string');
         Writeln('  check                Check script syntax/compilation without executing');
         Writeln('  deps                 List all script dependencies (units and includes)');
         Writeln('  profile              Profile script execution using sampling');
         Writeln('  make                 Bundle a project into a standalone executable');
         Writeln('  -I <path>            Add directory to include paths');
         Writeln('  -D <symbol>          Define a conditional compilation symbol');
         Writeln('  --timeout <ms>       Set maximum execution time in milliseconds');
         Writeln('  --interval <ms>      Set sampling interval for profiler (default 10ms)');
         Exit;
      end;

      fileName := ParamStr(i);
      Inc(i);
      paramOffset := i;

      if (fileName='-c') or (fileName='eval') then begin
         if paramOffset > ParamCount then begin
            Writeln('Missing code string after ', fileName);
            Exit;
         end;
         project := TStringProject.Create(ParamStr(paramOffset));
         Inc(paramOffset);
      end else if fileName='make' then begin
         MakeExe(paramOffset);
         Exit;
      end else if fileName='check' then begin
         if paramOffset > ParamCount then begin
            Writeln('Missing filename after check');
            Exit;
         end;
         fileName := ParamStr(paramOffset);
         if FileExists(fileName) then begin
            if StrEndsWith(fileName, '.zip') then
               project := TZipProject.Create(fileName)
            else project := TFileProject.Create(fileName);
         end else if DirectoryExists(fileName) then
            project := TDirectoryProject.Create(fileName)
         else begin
            Writeln('File "', fileName, '" not found.');
            Exit;
         end;
         try
            project.IncludePaths.Assign(includePaths);
            DoCheck(project, defines, timeoutMs);
         finally
            project.Free;
         end;
         Exit;
      end else if fileName='deps' then begin
         if paramOffset > ParamCount then begin
            Writeln('Missing filename after deps');
            Exit;
         end;
         fileName := ParamStr(paramOffset);
         if (fileName='-c') or (fileName='eval') then begin
            Inc(paramOffset);
            if paramOffset > ParamCount then begin
               Writeln('Missing code string after ', fileName);
               Exit;
            end;
            project := TStringProject.Create(ParamStr(paramOffset));
         end else if FileExists(fileName) then begin
            if StrEndsWith(fileName, '.zip') then
               project := TZipProject.Create(fileName)
            else project := TFileProject.Create(fileName);
         end else if DirectoryExists(fileName) then
            project := TDirectoryProject.Create(fileName)
         else begin
            Writeln('File "', fileName, '" not found.');
            Exit;
         end;
         try
            project.IncludePaths.Assign(includePaths);
            DoDeps(project, defines);
         finally
            project.Free;
         end;
         Exit;
      end else if fileName='profile' then begin
         if paramOffset > ParamCount then begin
            Writeln('Missing filename after profile');
            Exit;
         end;
         fileName := ParamStr(paramOffset);
         if (fileName='-c') or (fileName='eval') then begin
            Inc(paramOffset);
            if paramOffset > ParamCount then begin
               Writeln('Missing code string after ', fileName);
               Exit;
            end;
            project := TStringProject.Create(ParamStr(paramOffset));
         end else if FileExists(fileName) then begin
            if StrEndsWith(fileName, '.zip') then
               project := TZipProject.Create(fileName)
            else project := TFileProject.Create(fileName);
         end else if DirectoryExists(fileName) then
            project := TDirectoryProject.Create(fileName)
         else begin
            Writeln('File "', fileName, '" not found.');
            Exit;
         end;
         try
            project.IncludePaths.Assign(includePaths);
            DoProfile(project, paramOffset, defines, interval);
         finally
            project.Free;
         end;
         Exit;
      end else if FileExists(fileName) then begin
         if StrEndsWith(fileName, '.zip') then
            project := TZipProject.Create(fileName)
         else project := TFileProject.Create(fileName)
      end else if DirectoryExists(fileName) then
         project := TDirectoryProject.Create(fileName)
      else begin
         Writeln('File "', fileName, '" not found.');
         Exit;
      end;

      try
         project.IncludePaths.Assign(includePaths);
         RunRunner(project, paramOffset, False, defines, timeoutMs);
      finally
         project.Free;
      end;
   finally
      defines.Free;
      includePaths.Free;
   end;
end.
