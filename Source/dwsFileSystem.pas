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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsFileSystem;

{$I dws.inc}

interface

uses Classes, SysUtils, dwsUtils, dwsXPlatform;

type

   // TdwsFileOpenMode
   //
   TdwsFileOpenMode = (
      fomReadOnly,   // opens file for read-only, fails if doesn't exist
      fomReadWrite,  // opens file for read-write, creates a new one if doesn't exist
      fomCreate,     // opens file for read-write, always empty
      fomFastSequentialRead // opens file for sequential read-only, return nil if doesn't exist
      );

   EdwsFileSystemException = class (Exception)
   end;

   EdwsFSInvalidFileName = class (EdwsFileSystemException)
   end;

   // IdwsFileSystem
   //
   IdwsFileSystem = interface
      ['{D49F19A9-46C6-43E1-AF29-BDB8602A098C}']
      function FileExists(const fileName : String) : Boolean;
      function OpenFileStream(const fileName : String; const mode : TdwsFileOpenMode) : TStream;
      function ValidateFileName(const fileName : String) : String;
   end;

   // TdwsBaseFileSystem
   //
   {: Minimal virtualized filesystem interface. }
   TdwsBaseFileSystem = class abstract (TInterfacedObject, IdwsFileSystem)
      public
         constructor Create; virtual;

         function FileExists(const fileName : String) : Boolean; virtual; abstract;
         function OpenFileStream(const fileName : String; const mode : TdwsFileOpenMode) : TStream; virtual; abstract;
         function ValidateFileName(const fileName : String) : String; virtual; abstract;
   end;

   // TdwsNullFileSystem
   //
   {: Gives access to nothing. }
   TdwsNullFileSystem = class (TdwsBaseFileSystem)
      public
         function FileExists(const fileName : String) : Boolean; override;
         function OpenFileStream(const fileName : String; const mode : TdwsFileOpenMode) : TStream; override;
         function ValidateFileName(const fileName : String) : String; override;
   end;

   // TdwsOSFileSystem
   //
   {: Gives access to the whole OS FileSystem. }
   TdwsOSFileSystem = class (TdwsBaseFileSystem)
      public
         function ValidateFileName(const fileName : String) : String; override;

         function FileExists(const fileName : String) : Boolean; override;
         function OpenFileStream(const fileName : String; const mode : TdwsFileOpenMode) : TStream; override;
   end;

   // TdwsRestrictedOSFileSystem
   //
   {: Gives access to only specified directories and their subdirectories. }
   TdwsRestrictedOSFileSystem = class (TdwsOSFileSystem)
      private
         FPaths : TStrings;
         FPathsPrepared : Boolean;

      protected
         procedure SetPaths(const val : TStrings);
         procedure PathsChanged(Sender : TObject);
         procedure PreparePaths;

      public
         constructor Create; override;
         destructor Destroy; override;

         function ValidateFileName(const fileName : String) : String; override;

         property Paths : TStrings read FPaths write SetPaths;
   end;

   // TdwsCustomFileSystem
   //
   TdwsCustomFileSystem = class abstract (TComponent)
      public
         function AllocateFileSystem : IdwsFileSystem; virtual; abstract;
   end;

   // TdwsNoFileSystem
   //
   TdwsNoFileSystem = class abstract (TdwsCustomFileSystem)
      public
         function AllocateFileSystem : IdwsFileSystem; override;
   end;

   // TdwsRestrictedFileSystem
   //
   TdwsRestrictedFileSystem = class abstract (TdwsCustomFileSystem)
      private
         FPaths : TStrings;

      protected
         procedure SetPaths(const val : TStrings);

      public
         constructor Create(Owner : TComponent); override;
         destructor Destroy; override;

         function AllocateFileSystem : IdwsFileSystem; override;

      published
         property Paths : TStrings read FPaths write SetPaths;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TFileHandleStream = class (THandleStream)
      destructor Destroy; override;
   end;

// Destroy
//
destructor TFileHandleStream.Destroy;
begin
   CloseFileHandle(Handle);
   inherited;
end;

// ------------------
// ------------------ TdwsBaseFileSystem ------------------
// ------------------

// Create
//
constructor TdwsBaseFileSystem.Create;
begin
   inherited Create;
end;

// ------------------
// ------------------ TdwsNullFileSystem ------------------
// ------------------

// FileExists
//
function TdwsNullFileSystem.FileExists(const fileName : String) : Boolean;
begin
   Result:=False;
end;

// OpenFileStream
//
function TdwsNullFileSystem.OpenFileStream(const fileName : String; const mode : TdwsFileOpenMode) : TStream;
begin
   Result:=nil;
end;

// ValidateFileName
//
function TdwsNullFileSystem.ValidateFileName(const fileName : String) : String;
begin
   Result:='';
end;

// ------------------
// ------------------ TdwsOSFileSystem ------------------
// ------------------

// ValidateFileName
//
function TdwsOSFileSystem.ValidateFileName(const fileName : String) : String;
begin
   // accept all
   Result:=ExpandFileName(fileName);
end;

// FileExists
//
function TdwsOSFileSystem.FileExists(const fileName : String) : Boolean;
var
   validFileName : String;
begin
   validFileName:=ValidateFileName(fileName);
   Result:=SysUtils.FileExists(validFileName);
end;

// OpenFileStream
//
function TdwsOSFileSystem.OpenFileStream(const fileName : String; const mode : TdwsFileOpenMode) : TStream;
var
   validFileName : String;
   hFile : THandle;
begin
   validFileName:=ValidateFileName(fileName);
   case mode of
      fomReadOnly :
         Result:=TFileStream.Create(validFileName, fmOpenRead or fmShareDenyWrite);
      fomReadWrite :
         Result:=TFileStream.Create(validFileName, fmCreate);
      fomCreate : begin
         Result:=TFileStream.Create(validFileName, fmCreate);
         if Result.Size>0 then
            Result.Size:=0;
      end;
      fomFastSequentialRead : begin
         hFile:=OpenFileForSequentialReadOnly(validFileName);
         if hFile<>INVALID_HANDLE_VALUE then
            Result:=TFileHandleStream.Create(hFile)
         else Result:=nil;
      end;
   else
      Assert(False);
      Result:=nil;
   end;
end;

// ------------------
// ------------------ TdwsRestrictedOSFileSystem ------------------
// ------------------

// Create
//
constructor TdwsRestrictedOSFileSystem.Create;
begin
   inherited;
   FPaths:=TStringList.Create;
   TStringList(FPaths).OnChange:=PathsChanged;
end;

// Destroy
//
destructor TdwsRestrictedOSFileSystem.Destroy;
begin
   FPaths.Free;
   inherited;
end;

// SetPaths
//
procedure TdwsRestrictedOSFileSystem.SetPaths(const val : TStrings);
begin
   FPaths.Assign(val);
end;

// PathsChanged
//
procedure TdwsRestrictedOSFileSystem.PathsChanged(Sender : TObject);
begin
   FPathsPrepared:=False;
end;

// PreparePaths
//
procedure TdwsRestrictedOSFileSystem.PreparePaths;
const
   cDummyFileName = 'dummy.file';
var
   i : Integer;
   buf : String;
begin
   if FPathsPrepared then Exit;
   for i:=FPaths.Count-1 downto 0 do begin
      buf:=Trim(FPaths[i]);
      if buf='' then
         FPaths.Delete(i)
      else begin
         buf:=ExpandFileName(IncludeTrailingPathDelimiter(buf)+cDummyFileName);
         FPaths[i]:=Copy(buf, 1, Length(buf)-Length(cDummyFileName));
      end;
   end;
   FPathsPrepared:=True;
end;

// ValidateFileName
//
function TdwsRestrictedOSFileSystem.ValidateFileName(const fileName : String) : String;
var
   i : Integer;
   path : String;
begin
   for i:=0 to FPaths.Count-1 do begin
      path:=FPaths[i];
      if StrContains(fileName, ':') then
         Result:=ExpandFileName(fileName)
      else Result:=ExpandFileName(path+fileName);
      if StrIBeginsWith(Result, path) then Exit;
   end;
   Result:='';
end;

// ------------------
// ------------------ TdwsNoFileSystem ------------------
// ------------------

// AllocateFileSystem
//
function TdwsNoFileSystem.AllocateFileSystem : IdwsFileSystem;
begin
   Result:=TdwsNullFileSystem.Create;
end;

// ------------------
// ------------------ TdwsRestrictedFileSystem ------------------
// ------------------

// Create
//
constructor TdwsRestrictedFileSystem.Create(Owner : TComponent);
begin
   inherited;
   FPaths:=TStringList.Create;
end;

// Destroy
//
destructor TdwsRestrictedFileSystem.Destroy;
begin
   inherited;
   FPaths.Free;
end;

// AllocateFileSystem
//
function TdwsRestrictedFileSystem.AllocateFileSystem : IdwsFileSystem;
var
   fs : TdwsRestrictedOSFileSystem;
begin
   fs:=TdwsRestrictedOSFileSystem.Create;
   fs.Paths:=Paths;
   Result:=fs;
end;

// SetPaths
//
procedure TdwsRestrictedFileSystem.SetPaths(const val : TStrings);
begin
   FPaths.Assign(val);
end;

end.
