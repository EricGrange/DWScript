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

uses
   System.Classes, System.SysUtils,
   dwsUtils, dwsXPlatform;

type

   // TdwsFileOpenMode
   //
   TdwsFileOpenMode = (
      fomReadOnly,   // opens file for read-only, fails if doesn't exist
      fomReadWrite,  // opens file for read-write, creates a new one if doesn't exist
      fomCreate,     // opens file for read-write, always empty
      fomFastSequentialRead // opens file for sequential read-only, return nil if doesn't exist
      );

   TdwsFileSystemTextFormat = (
      fstfUTF8_with_BOM,
      fstfUTF8_no_BOM
   );

   EdwsFileSystemException = class (Exception);

   EdwsFSInvalidFileName = class (EdwsFileSystemException);

   TFileStreamOpenedEvent = procedure (Sender : TObject; const fileName : TFileName; const mode : TdwsFileOpenMode) of object;

   // IdwsFileSystem
   //
   IdwsFileSystem = interface
      ['{D49F19A9-46C6-43E1-AF29-BDB8602A098C}']
      function FileExists(const fileName : TFilename) : Boolean;

      function FindFileName(const fileName : TFileName) : TFileName;
      function OpenFileStream(const fileName : TFilename; const mode : TdwsFileOpenMode) : TStream;
      function ValidateFileName(const fileName : TFilename) : TFilename;

      function LoadTextFile(const fileName : TFilename) : UnicodeString;

      function GetSearchPaths : TStrings;
      procedure SetSearchPaths(const val : TStrings);
      property SearchPaths : TStrings read GetSearchPaths write SetSearchPaths;
   end;

   // IdwsFileSystemRW  (work in progress, not stable yet)
   //
   IdwsFileSystemRW = interface (IdwsFileSystem)
      ['{2A9B3826-4A52-4DB4-98B9-F7A3F97814EF}']
      function DirectoryExists(const directoryName : TFilename) : Boolean;
      function CreateDirectory(const directoryName : TFilename) : Boolean;
      function DeleteDirectory(const directoryName : TFilename; recursive : Boolean) : Boolean;
      function ForceDirectories(const directoryName : TFilename) : Boolean;

      function FileTimeStamp(const fileName : TFilename) : TdwsDateTime;

      procedure SaveTextFile(const fileName : TFileName; const strData : String; fmt : TdwsFileSystemTextFormat);
   end;

   // TdwsBaseFileSystem
   //
   {: Minimal virtualized filesystem interface. }
   TdwsBaseFileSystem = class abstract (TInterfacedObject, IdwsFileSystem, IdwsFileSystemRW)
      private
         FOnFileStreamOpened : TFileStreamOpenedEvent;
         FSearchPaths : TStrings;

      protected
         function GetSearchPaths : TStrings;
         procedure SetSearchPaths(const val : TStrings);

         procedure DoFileStreamOpenedEvent(const fileName : TFilename; const mode : TdwsFileOpenMode); inline;

      public
         constructor Create; virtual;
         destructor Destroy; override;

         function FindFileName(const fileName : TFileName) : TFileName; virtual; abstract;
         function FileExists(const fileName : TFilename) : Boolean; virtual; abstract;
         function DirectoryExists(const directoryName : TFilename) : Boolean; virtual; abstract;
         function CreateDirectory(const directoryName : TFilename) : Boolean; virtual; abstract;
         function DeleteDirectory(const directoryName : TFilename; recursive : Boolean) : Boolean; virtual; abstract;
         function ForceDirectories(const directoryName : TFilename) : Boolean; virtual; abstract;
         function OpenFileStream(const fileName : TFilename; const mode : TdwsFileOpenMode) : TStream; virtual; abstract;
         function ValidateFileName(const fileName : TFilename) : TFilename; virtual; abstract;
         function FileTimeStamp(const fileName : TFilename) : TdwsDateTime; virtual; abstract;

         function LoadTextFile(const fileName : TFilename) : UnicodeString;
         procedure SaveTextFile(const fileName : TFileName; const strData : String; fmt : TdwsFileSystemTextFormat);

         property OnFileStreamOpened : TFileStreamOpenedEvent read FOnFileStreamOpened write FOnFileStreamOpened;
         property SearchPaths : TStrings read FSearchPaths write SetSearchPaths;
   end;

   // TdwsNullFileSystem
   //
   {: Gives access to nothing. }
   TdwsNullFileSystem = class (TdwsBaseFileSystem)
      public
         function FindFileName(const fileName : TFileName) : TFileName; override;
         function FileExists(const fileName : TFilename) : Boolean; override;

         function DirectoryExists(const directoryName : TFilename) : Boolean; override;
         function CreateDirectory(const directoryName : TFilename) : Boolean; override;
         function DeleteDirectory(const directoryName : TFilename; recursive : Boolean) : Boolean; override;
         function ForceDirectories(const directoryName : TFilename) : Boolean; override;

         function OpenFileStream(const fileName : TFilename; const mode : TdwsFileOpenMode) : TStream; override;
         function ValidateFileName(const fileName : TFilename) : TFilename; override;
         function FileTimeStamp(const fileName : TFilename) : TdwsDateTime; override;
   end;

   // TdwsOSFileSystem
   //
   {: Gives access to the whole OS FileSystem. }
   TdwsOSFileSystem = class (TdwsBaseFileSystem)
      public
         function ValidateFileName(const fileName : TFilename) : TFilename; override;

         function FindFileName(const fileName : TFileName) : TFileName; override;
         function FileExists(const fileName : TFilename) : Boolean; override;

         function DirectoryExists(const directoryName : TFilename) : Boolean; override;
         function CreateDirectory(const directoryName : TFilename) : Boolean; override;
         function DeleteDirectory(const directoryName : TFilename; recursive : Boolean) : Boolean; override;
         function ForceDirectories(const directoryName : TFilename) : Boolean; override;

         function FileTimeStamp(const fileName : TFilename) : TdwsDateTime; override;

         function OpenFileStream(const fileName : TFilename; const mode : TdwsFileOpenMode) : TStream; override;
   end;

   // TdwsRestrictedOSFileSystem
   //
   {: Gives access to only specified directories and their subdirectories. }
   TdwsRestrictedOSFileSystem = class (TdwsOSFileSystem)
      private
         FPaths : TStrings;
         FPathsPrepared : Boolean;
         FVariables : TStrings;

      protected
         procedure SetPaths(const val : TStrings);
         procedure SetVariables(const val : TStrings);
         procedure PathsChanged(Sender : TObject);
         procedure PreparePaths;

      public
         constructor Create; override;
         destructor Destroy; override;

         function ValidateFileName(const aFileName : TFilename) : TFilename; override;

         property Paths : TStrings read FPaths write SetPaths;
         property Variables : TStrings read FVariables write SetVariables;
   end;

   // TdwsCustomFileSystem
   //
   TdwsCustomFileSystem = class abstract (TComponent)
      private
         FOnFileStreamOpened : TFileStreamOpenedEvent;

      public
         function AllocateFileSystem : IdwsFileSystem; virtual; abstract;
         function AllocateFileSystemRW : IdwsFileSystemRW; virtual; abstract;

         property OnFileStreamOpened : TFileStreamOpenedEvent read FOnFileStreamOpened write FOnFileStreamOpened;
   end;

   // TdwsNoFileSystem
   //
   TdwsNoFileSystem = class abstract (TdwsCustomFileSystem)
      public
         function AllocateFileSystem : IdwsFileSystem; override;
         function AllocateFileSystemRW : IdwsFileSystemRW; override;
   end;

   // TdwsRestrictedFileSystem
   //
   TdwsRestrictedFileSystem = class abstract (TdwsCustomFileSystem)
      private
         FPaths : TStrings;
         FSearchPaths : TStrings;
         FVariables : TStrings;

      protected
         procedure SetPaths(const val : TStrings);
         procedure SetSearchPaths(const val : TStrings);
         procedure SetVariables(const val : TStrings);

         function CreateRestrictedFS : TdwsRestrictedOSFileSystem;

      public
         constructor Create(Owner : TComponent); override;
         destructor Destroy; override;

         function AllocateFileSystem : IdwsFileSystem; override;
         function AllocateFileSystemRW : IdwsFileSystemRW; override;

      published
         property Paths : TStrings read FPaths write SetPaths;
         property SearchPaths : TStrings read FSearchPaths write SetSearchPaths;
         property Variables : TStrings read FVariables write SetVariables;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsUTF8;

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
   FSearchPaths := TStringList.Create;
end;

// Destroy
//
destructor TdwsBaseFileSystem.Destroy;
begin
   inherited;
   FSearchPaths.Free;
end;

// DoFileStreamOpenedEvent
//
procedure TdwsBaseFileSystem.DoFileStreamOpenedEvent(const fileName : TFilename; const mode : TdwsFileOpenMode);
begin
   if Assigned(FOnFileStreamOpened) then
      FOnFileStreamOpened(Self, fileName, mode);
end;

// LoadTextFile
//
function TdwsBaseFileSystem.LoadTextFile(const fileName : TFilename) : UnicodeString;
begin
   var stream := OpenFileStream(fileName, fomFastSequentialRead);
   if stream <> nil then begin
      try
         Result := LoadTextFromStream(stream);
      finally
         stream.Free;
      end;
   end else Result := '';
end;

// SaveTextFile
//
procedure TdwsBaseFileSystem.SaveTextFile(const fileName : TFileName; const strData : String; fmt : TdwsFileSystemTextFormat);
const
   cUTF8BOM : array [0..2] of Byte = ( $EF, $BB, $BF );
var
   utf8 : RawByteString;
begin
   var stream := OpenFileStream(fileName, fomCreate);
   if stream = nil then
      raise EdwsFileSystemException.Create('SaveTextFile cannot open stream');
   try
      if strData <> '' then begin
         utf8 := StringToUTF8(strData);
         case fmt of
            fstfUTF8_with_BOM :
               stream.Write(cUTF8BOM[0], 3);
            fstfUTF8_no_BOM: ;
         else
            Assert(False);
         end;
         stream.Write(Pointer(utf8)^, Length(utf8));
      end;
   finally
      stream.Free;
   end;
end;

// GetSearchPaths
//
function TdwsBaseFileSystem.GetSearchPaths : TStrings;
begin
   Result := FSearchPaths;
end;

// SetSearchPaths
//
procedure TdwsBaseFileSystem.SetSearchPaths(const val : TStrings);
begin
   FSearchPaths.Assign(val);
end;

// ------------------
// ------------------ TdwsNullFileSystem ------------------
// ------------------

// FileExists
//
function TdwsNullFileSystem.FileExists(const fileName : TFilename) : Boolean;
begin
   Result:=False;
end;

// DirectoryExists
//
function TdwsNullFileSystem.DirectoryExists(const directoryName : TFilename) : Boolean;
begin
   Result := False;
end;

// CreateDirectory
//
function TdwsNullFileSystem.CreateDirectory(const directoryName : TFilename) : Boolean;
begin
   Result := False;
end;

// DeleteDirectory
//
function TdwsNullFileSystem.DeleteDirectory(const directoryName : TFilename; recursive : Boolean) : Boolean;
begin
   Result := False;
end;

// ForceDirectories
//
function TdwsNullFileSystem.ForceDirectories(const directoryName : TFilename) : Boolean;
begin
   Result := False;
end;

// OpenFileStream
//
function TdwsNullFileSystem.OpenFileStream(const fileName : TFilename; const mode : TdwsFileOpenMode) : TStream;
begin
   Result:=nil;
end;

// ValidateFileName
//
function TdwsNullFileSystem.ValidateFileName(const fileName : TFilename) : TFilename;
begin
   Result:='';
end;

// FileTimeStamp
//
function TdwsNullFileSystem.FileTimeStamp(const fileName : TFilename) : TdwsDateTime;
begin
   Result := Default(TdwsDateTime);
end;

// FindFileName
//
function TdwsNullFileSystem.FindFileName(const fileName : TFileName) : TFileName;
begin
   Result := '';
end;

// ------------------
// ------------------ TdwsOSFileSystem ------------------
// ------------------

// ValidateFileName
//
function TdwsOSFileSystem.ValidateFileName(const fileName : TFilename) : TFilename;
begin
   // accept all
   Result := ExpandFileName(fileName);
end;

// FindFileName
//
function TdwsOSFileSystem.FindFileName(const fileName : TFileName) : TFileName;
var
   i : Integer;
begin
   if FileExists(fileName) then
      Exit(fileName);

   if (DriveDelim <> '') and StrContains(fileName, DriveDelim) then
      Exit('');
   if StrBeginsWith(fileName, PathDelim) then
      Exit('');

   for i := 0 to SearchPaths.Count-1 do begin
      Result := IncludeTrailingPathDelimiter(SearchPaths[i]) + fileName;
      if FileExists(Result) then Exit;
   end;
   Result := '';
end;

// FileExists
//
function TdwsOSFileSystem.FileExists(const fileName : TFilename) : Boolean;
var
   validFileName : TFilename;
begin
   validFileName := ValidateFileName(fileName);
   Result := System.SysUtils.FileExists(validFileName);
end;

// DirectoryExists
//
function TdwsOSFileSystem.DirectoryExists(const directoryName : TFilename) : Boolean;
var
   validDirectoryName : TFilename;
begin
   validDirectoryName := ValidateFileName(directoryName);
   Result := System.SysUtils.DirectoryExists(validDirectoryName);
end;

// CreateDirectory
//
function TdwsOSFileSystem.CreateDirectory(const directoryName : TFilename) : Boolean;
var
   validDirectoryName : TFilename;
begin
   validDirectoryName := ValidateFileName(directoryName);
   if validDirectoryName <> '' then
      Result := System.SysUtils.CreateDir(validDirectoryName)
   else Result := False;
end;

// DeleteDirectory
//
function TdwsOSFileSystem.DeleteDirectory(const directoryName : TFilename; recursive : Boolean) : Boolean;
var
   validDirectoryName : TFilename;
begin
   validDirectoryName := ValidateFileName(directoryName);
   if validDirectoryName <> '' then
      Result := dwsXPlatform.DeleteDirectory(validDirectoryName, recursive)
   else Result := True;
end;

// ForceDirectories
//
function TdwsOSFileSystem.ForceDirectories(const directoryName : TFilename) : Boolean;
var
   validDirectoryName : TFilename;
begin
   validDirectoryName := ValidateFileName(directoryName);
   if validDirectoryName <> '' then
      Result := System.SysUtils.ForceDirectories(validDirectoryName)
   else Result := False;
end;

// FileTimeStamp
//
function TdwsOSFileSystem.FileTimeStamp(const fileName : TFilename) : TdwsDateTime;
var
   validFileName : TFilename;
begin
   validFileName := ValidateFileName(fileName);
   Result := FileDateTime(validFileName);
end;

// OpenFileStream
//
function TdwsOSFileSystem.OpenFileStream(const fileName : TFilename; const mode : TdwsFileOpenMode) : TStream;
var
   validFileName : TFilename;
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
      Result:=nil;
   end;

   if Result <> nil then
      DoFileStreamOpenedEvent(validFileName, mode);
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
   FVariables:=TFastCompareTextList.Create;
end;

// Destroy
//
destructor TdwsRestrictedOSFileSystem.Destroy;
begin
   FVariables.Free;
   FPaths.Free;
   inherited;
end;

// SetPaths
//
procedure TdwsRestrictedOSFileSystem.SetPaths(const val : TStrings);
begin
   FPaths.Assign(val);
end;

// SetVariables
//
procedure TdwsRestrictedOSFileSystem.SetVariables(const val : TStrings);
begin
   FVariables.Assign(val);
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
   buf : TFileName;
begin
   if FPathsPrepared then Exit;
   for var i := FPaths.Count-1 downto 0 do begin
      buf:=Trim(FPaths[i]);
      if buf='' then
         FPaths.Delete(i)
      else begin
         buf:=ExpandFileName(IncludeTrailingPathDelimiter(buf)+cDummyFileName);
         FPaths[i] := Copy(buf, 1, Length(buf)-Length(cDummyFileName));
      end;
   end;
   FPathsPrepared := True;
end;

// ValidateFileName
//
function TdwsRestrictedOSFileSystem.ValidateFileName(const aFilename : TFilename) : TFilename;
var
   fileName, path : TFileName;
begin
   if not FPathsPrepared then
      PreparePaths;

   fileName := ApplyStringVariables(aFileName, FVariables, '%');
   if (DriveDelim <> '') and StrContains(fileName, DriveDelim) then begin
      // validate an absolute path
      Result := ExpandFileName(fileName);
      for var i := 0 to FPaths.Count-1 do begin
         if StrIBeginsWith(Result, FPaths[i]) then Exit;
      end;
   end else begin
      // search for match in paths
      for var i := 0 to FPaths.Count-1 do begin
         path := FPaths[i];
         Result := ExpandFileName(path + fileName);
         if StrIBeginsWith(Result, path) then Exit;
      end;
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

// AllocateFileSystemRW
//
function TdwsNoFileSystem.AllocateFileSystemRW : IdwsFileSystemRW;
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
   FSearchPaths:=TStringList.Create;
   FVariables:=TStringList.Create;
end;

// Destroy
//
destructor TdwsRestrictedFileSystem.Destroy;
begin
   inherited;
   FPaths.Free;
   FSearchPaths.Free;
   FVariables.Free;
end;

// CreateRestrictedFS
//
function TdwsRestrictedFileSystem.CreateRestrictedFS : TdwsRestrictedOSFileSystem;
begin
   Result := TdwsRestrictedOSFileSystem.Create;
   Result.Paths := Paths;
   Result.SearchPaths := SearchPaths;
   Result.Variables := Variables;
   Result.OnFileStreamOpened := OnFileStreamOpened;
end;

// AllocateFileSystem
//
function TdwsRestrictedFileSystem.AllocateFileSystem : IdwsFileSystem;
begin
   Result := CreateRestrictedFS;
end;

// AllocateFileSystemRW
//
function TdwsRestrictedFileSystem.AllocateFileSystemRW : IdwsFileSystemRW;
begin
   Result := CreateRestrictedFS;
end;

// SetPaths
//
procedure TdwsRestrictedFileSystem.SetPaths(const val : TStrings);
begin
   FPaths.Assign(val);
end;

// SetSearchPaths
//
procedure TdwsRestrictedFileSystem.SetSearchPaths(const val : TStrings);
begin
   FSearchPaths.Assign(val);
end;

// SetVariables
//
procedure TdwsRestrictedFileSystem.SetVariables(const val : TStrings);
begin
   FVariables.Assign(val);
end;

end.
