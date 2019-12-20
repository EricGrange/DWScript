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
unit dwsFileFunctions;

{$I dws.inc}

interface

uses
   Classes, SysUtils, Winapi.Windows,
   dwsXPlatform, dwsUtils, dwsStrings,
   dwsFunctions, dwsSymbols, dwsExprs, dwsCoreExprs, dwsExprList, dwsUnitSymbols,
   dwsConstExprs, dwsMagicExprs, dwsDataContext;

const
   SYS_FILE = 'File';

type

   IdwsFileHandle = interface
      function GetHandle : THandle;
      function GetOSError : Integer;
      procedure Clear;
   end;

   TdwsFileHandle = class (TInterfacedSelfObject, IdwsFileHandle)
      private
         FHandle : THandle;
         FOSError : Integer;

      public
         constructor Create(aHandle : THandle; osError : Integer);
         destructor Destroy; override;

         function GetHandle : THandle;
         function GetOSError : Integer;
         procedure Clear;
   end;

   TBaseFileSymbol = class (TBaseSymbol)
      public
         constructor Create;

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;
   end;

   TFileOpenFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TFileOpenReadFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TFileCreateFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TFileIsValidFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TFileRead1Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileRead2Func = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TFileRead3Func = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TFileRead4Func = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TFileWrite1Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileWrite2Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileSeekFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileFlushBuffersFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TFileCloseFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TFileSizeFileFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileSetSizeFileFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TFileSizeNameFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileDateTimeFileFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TFileDateTimeNameFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TFileAccessDateTimeFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TFileSetDateTimeFileProc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TFileSetDateTimeNameProc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TFileExistsFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TDirectoryExistsFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TExpandFileNameFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;
   TExtractFilePathFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;
   TExtractFileNameFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;
   TExtractFileExtFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TChangeFileExtFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TDeleteFileFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TCopyFileFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TMoveFileFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TRenameFileFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TForceDirectoriesFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TCreateDirFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TRemoveDirFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TEnumerateDirFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TEnumerateSubDirsFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RegisterFileTypes
//
procedure RegisterFileTypes(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                            unitTable : TSymbolTable);
var
   typFile : TBaseFileSymbol;
begin
   if systemTable.FindLocal(SYS_FILE)<>nil then exit;

   typFile:=TBaseFileSymbol.Create;

   systemTable.AddSymbol(typFile);

   systemTable.AddSymbol(TConstSymbol.CreateValue('fmCreate', systemTable.TypInteger, fmCreate));
   systemTable.AddSymbol(TConstSymbol.CreateValue('fmOpenRead', systemTable.TypInteger, fmOpenRead));
   systemTable.AddSymbol(TConstSymbol.CreateValue('fmOpenWrite', systemTable.TypInteger, fmOpenWrite));
   systemTable.AddSymbol(TConstSymbol.CreateValue('fmOpenReadWrite', systemTable.TypInteger, fmOpenReadWrite));
   systemTable.AddSymbol(TConstSymbol.CreateValue('fmExclusive', systemTable.TypInteger, fmExclusive));
   systemTable.AddSymbol(TConstSymbol.CreateValue('fmShareExclusive', systemTable.TypInteger, fmShareExclusive));
   systemTable.AddSymbol(TConstSymbol.CreateValue('fmShareDenyWrite', systemTable.TypInteger, fmShareDenyWrite));
   systemTable.AddSymbol(TConstSymbol.CreateValue('fmShareDenyNone', systemTable.TypInteger, fmShareDenyNone));

   systemTable.AddSymbol(TConstSymbol.CreateValue('soFromBeginning', systemTable.TypInteger, soFromBeginning));
   systemTable.AddSymbol(TConstSymbol.CreateValue('soFromCurrent', systemTable.TypInteger, soFromCurrent));
   systemTable.AddSymbol(TConstSymbol.CreateValue('soFromEnd', systemTable.TypInteger, soFromEnd));
end;

// GetFileHandle
//
function GetFileHandle(const args : TExprBaseListExec; index : Integer; checkValidity : Boolean = True) : THandle;
var
   ih : IdwsFileHandle;
   v : Variant;
begin
   args.ExprBase[index].EvalAsVariant(args.Exec, v);
   if (TVarData(v).VType=varUnknown) and (TVarData(v).VUnknown<>nil) then begin
      ih:=IdwsFileHandle(IUnknown(TVarData(v).VUnknown));
      Result:=ih.GetHandle;
      if checkValidity and (Result=INVALID_HANDLE_VALUE) then
         raise Exception.CreateFmt('Invalid file handle (%d): %s',
                                   [ih.GetOSError, SysErrorMessage(ih.GetOSError)]);
   end else begin
      if checkValidity then
         raise Exception.Create('File is not open');
      Result:=0;
   end;
end;

// ------------------
// ------------------ TdwsFileHandle ------------------
// ------------------

// Create
//
constructor TdwsFileHandle.Create(aHandle : THandle; osError : Integer);
begin
   FHandle:=aHandle;
   FOSError:=osError;
end;

// Destroy
//
destructor TdwsFileHandle.Destroy;
begin
   if FHandle<>0 then
      CloseFileHandle(FHandle);
end;

// GetHandle
//
function TdwsFileHandle.GetHandle : THandle;
begin
   Result:=FHandle;
end;

// GetOSError
//
function TdwsFileHandle.GetOSError : Integer;
begin
   Result:=FOSError;
end;

// Clear
//
procedure TdwsFileHandle.Clear;
begin
   FHandle:=0;
   FOSError:=0;
end;

// ------------------
// ------------------ TBaseFileSymbol ------------------
// ------------------

// Create
//
constructor TBaseFileSymbol.Create;
begin
   inherited Create(SYS_FILE);
end;

// IsCompatible
//
function TBaseFileSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym<>nil) and (typSym.UnAliasedType is TBaseFileSymbol);
end;

// InitData
//
procedure TBaseFileSymbol.InitData(const data : TData; offset : Integer);
begin
   data[offset]:=IUnknown(nil);
end;

// ------------------
// ------------------ TFileOpenFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TFileOpenFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   h : THandle;
   i : IdwsFileHandle;
begin
   h:=FileOpen(args.AsFileName[0], args.AsInteger[1]);
   i:=TdwsFileHandle.Create(h, GetLastError);
   Result:=IUnknown(i);
end;

// ------------------
// ------------------ TFileOpenReadFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TFileOpenReadFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   h : THandle;
   i : IdwsFileHandle;
begin
   h:=FileOpen(args.AsFileName[0], fmOpenRead+fmShareDenyNone);
   i:=TdwsFileHandle.Create(h, GetLastError);
   Result:=IUnknown(i);
end;

// ------------------
// ------------------ TFileCreateFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TFileCreateFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   h : THandle;
   i : IdwsFileHandle;
begin
   h:=FileCreate(args.AsFileName[0]);
   i:=TdwsFileHandle.Create(h, GetLastError);
   Result:=IUnknown(i);
end;

// ------------------
// ------------------ TFileIsValidFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TFileIsValidFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=GetFileHandle(args, 0, False)<>INVALID_HANDLE_VALUE;
end;

// ------------------
// ------------------ TFileRead1Func ------------------
// ------------------

// DoEvalAsInteger
//
function TFileRead1Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buf : RawByteString;
   n : Integer;
begin
   n:=args.AsInteger[2];
   SetLength(buf, n);
   if n>0 then begin
      n:=FileRead(GetFileHandle(args, 0), Pointer(buf)^, n);
      if n<0 then
         RaiseLastOSError
      else SetLength(buf, n);
   end;
   args.AsDataString[1]:=buf;
   Result:=n;
end;

// ------------------
// ------------------ TFileRead2Func ------------------
// ------------------

// DoEvalAsString
//
procedure TFileRead2Func.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
var
   buf : RawByteString;
   n : Integer;
begin
   n:=args.AsInteger[1];
   SetLength(buf, n);
   if n>0 then begin
      n:=FileRead(GetFileHandle(args, 0), Pointer(buf)^, n);
      if n<0 then
         RaiseLastOSError
      else SetLength(buf, n);
   end;
   RawByteStringToScriptString(buf, Result);
end;

// ------------------
// ------------------ TFileRead2Func ------------------
// ------------------

// DoEvalAsString
//
procedure TFileRead3Func.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
var
   buf : RawByteString;
   p, n : Int64;
   f : Integer;
begin
   f:=GetFileHandle(args, 0);
   p:=FileSeek(f, 0, 1);
   n:=FileSeek(f, 0, 2)-p;
   FileSeek(f, p, 0);
   SetLength(buf, n);
   if n>0 then begin
      n:=FileRead(f, Pointer(buf)^, n);
      if n<0 then
         RaiseLastOSError
      else SetLength(buf, n);
   end;
   RawByteStringToScriptString(buf, Result);
end;

// ------------------
// ------------------ TFileRead4Func ------------------
// ------------------

// DoEvalAsString
//
procedure TFileRead4Func.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   LoadRawBytesAsScriptStringFromFile(args.AsFileName[0], Result);
end;

// ------------------
// ------------------ TFileWrite1Func ------------------
// ------------------

// DoEvalAsInteger
//
function TFileWrite1Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buf : RawByteString;
begin
   buf:=args.AsDataString[1];
   Result:=dwsXPlatform.FileWrite(GetFileHandle(args, 0), Pointer(buf), Length(buf));
end;

// ------------------
// ------------------ TFileWrite2Func ------------------
// ------------------

// DoEvalAsInteger
//
function TFileWrite2Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=SaveRawBytesToFile(args.AsFileName[0], args.AsDataString[1]);
end;

// ------------------
// ------------------ TFileSeekFunc ------------------
// ------------------

function TFileSeekFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=FileSeek(GetFileHandle(args, 0), args.AsInteger[1], args.AsInteger[2]);
end;

// ------------------
// ------------------ TFileFlushBuffersFunc ------------------
// ------------------

function TFileFlushBuffersFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := FileFlushBuffers(GetFileHandle(args, 0));
end;

// ------------------
// ------------------ TFileCloseFunc ------------------
// ------------------

procedure TFileCloseFunc.DoEvalProc(const args : TExprBaseListExec);
var
   v : Variant;
   i : IdwsFileHandle;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, v);
   if (TVarData(v).VType=varUnknown) and (TVarData(v).VUnknown<>nil) then begin
      i:=IdwsFileHandle(IUnknown(TVarData(v).VUnknown));
      CloseFileHandle(i.GetHandle);
      i.Clear;
   end;
end;

// ------------------
// ------------------ TFileSizeFileFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TFileSizeFileFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   p : Int64;
   h : THandle;
begin
   h := GetFileHandle(args, 0);
   p := FileSeek(h, Int64(0), soFromCurrent);
   Result := FileSeek(h, Int64(0), soFromEnd);
   FileSeek(h, p, soFromBeginning);
end;

// ------------------
// ------------------ TFileSizeFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TFileSetSizeFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   h : THandle;
begin
   h := GetFileHandle(args, 0);
   FileSeek(h, args.AsInteger[1], soFromBeginning);
   Result := SetEndOfFile(h)
end;

// ------------------
// ------------------ TFileSizeNameFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TFileSizeNameFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=FileSize(args.AsFileName[0]);
end;

// ------------------
// ------------------ TFileDateTimeFileFunc ------------------
// ------------------

// DoEvalAsFloat
//
procedure TFileDateTimeFileFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=FileDateToDateTime(FileGetDate(GetFileHandle(args, 0)));
end;

// ------------------
// ------------------ TFileDateTimeNameFunc ------------------
// ------------------

procedure TFileDateTimeNameFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := FileDateTime(args.AsFileName[0]).AsLocalDateTime;
end;

// ------------------
// ------------------ TFileAccessDateTimeFunc ------------------
// ------------------

procedure TFileAccessDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := FileDateTime(args.AsFileName[0], True).AsLocalDateTime;
end;

// ------------------
// ------------------ TFileSetDateTimeFileProc ------------------
// ------------------

procedure TFileSetDateTimeFileProc.DoEvalProc(const args : TExprBaseListExec);
begin
   FileSetDateTime(GetFileHandle(args, 0), TdwsDateTime.FromLocalDateTime(args.AsFloat[1]));
end;

// ------------------
// ------------------ TFileSetDateTimeNameProc ------------------
// ------------------

procedure TFileSetDateTimeNameProc.DoEvalProc(const args : TExprBaseListExec);
var
   f : THandle;
begin
   f := FileOpen(args.AsFileName[0], fmOpenWrite);
   if f = THandle(-1) then
      RaiseLastOSError;
   try
      FileSetDateTime(f, TdwsDateTime.FromLocalDateTime(args.AsFloat[1]));
   finally
      FileClose(f);
   end;
end;

// ------------------
// ------------------ TFileExistsFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TFileExistsFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileExists(args.AsFileName[0]);
end;

// ------------------
// ------------------ TDirectoryExistsFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TDirectoryExistsFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=DirectoryExists(args.AsFileName[0]);
end;

// ------------------
// ------------------ TExpandFileNameFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TExpandFileNameFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result:=args.AsFileName[0];
end;

// ------------------
// ------------------ TExtractFilePathFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TExtractFilePathFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result:=ExtractFilePath(args.AsString[0]);
end;

// ------------------
// ------------------ TExtractFileNameFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TExtractFileNameFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result:=ExtractFileName(args.AsString[0]);
end;

// ------------------
// ------------------ TExtractFileExtFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TExtractFileExtFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result:=ExtractFileExt(args.AsString[0]);
end;

// ------------------
// ------------------ ChangeFileExt ------------------
// ------------------

// DoEvalAsString
//
procedure TChangeFileExtFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result:=ChangeFileExt(args.AsString[0], args.AsString[1]);
end;

// ------------------
// ------------------ TDeleteFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TDeleteFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileDelete(args.AsFileName[0]);
end;

// ------------------
// ------------------ TCopyFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TCopyFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileCopy(args.AsFileName[0], args.AsFileName[1], args.AsBoolean[2]);
end;

// ------------------
// ------------------ TMoveFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TMoveFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileMove(args.AsString[0], args.AsString[1]);
end;

// ------------------
// ------------------ TRenameFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TRenameFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileRename(args.AsFileName[0], args.AsFileName[1]);
end;

// ------------------
// ------------------ TForceDirectoriesFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TForceDirectoriesFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=ForceDirectories(args.AsFileName[0]);
end;

// ------------------
// ------------------ TCreateDirFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TCreateDirFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=CreateDir(args.AsFileName[0]);
end;

// ------------------
// ------------------ TRemoveDirFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TRemoveDirFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   path : UnicodeString;
begin
   path:=args.AsFileName[0];
   if args.AsBoolean[1] then
      Result:=DeleteDirectory(path)
   else Result:=RemoveDir(path);
end;

// ------------------
// ------------------ TEnumerateDirFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TEnumerateDirFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   sl : TStringList;
   newArray : TScriptDynamicStringArray;
begin
   sl := TStringList.Create;
   try
      CollectFiles(args.AsFileName[0], args.AsString[1], sl, args.AsBoolean[2]);
      newArray := TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).CompilerContext.TypString) as TScriptDynamicStringArray;
      Result := IScriptDynArray(newArray);
      newArray.AddStrings(sl);
   finally
      sl.Free;
   end;
end;

// ------------------
// ------------------ TEnumerateSubDirsFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TEnumerateSubDirsFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   sl : TStringList;
   newArray : TScriptDynamicStringArray;
begin
   sl := TStringList.Create;
   try
      CollectSubDirs(args.AsFileName[0], sl);
      newArray := TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).CompilerContext.TypString) as TScriptDynamicStringArray;
      Result := IScriptDynArray(newArray);
      newArray.AddStrings(sl);
   finally
      sl.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterFileTypes);

   RegisterInternalFunction(TFileOpenFunc, 'FileOpen', ['name', SYS_STRING, 'mode', SYS_INTEGER], SYS_FILE, []);
   RegisterInternalFunction(TFileOpenReadFunc, 'FileOpenRead', ['name', SYS_STRING], SYS_FILE, []);
   RegisterInternalFunction(TFileCreateFunc, 'FileCreate', ['name', SYS_STRING], SYS_FILE, []);

   RegisterInternalBoolFunction(TFileIsValidFunc, 'FileIsValid', ['f', SYS_FILE], [], 'IsValid');
   RegisterInternalIntFunction(TFileRead1Func, 'FileRead', ['f', SYS_FILE, '@buf', SYS_STRING, 'n', SYS_INTEGER], [iffOverloaded], 'Read');
   RegisterInternalStringFunction(TFileRead2Func, 'FileRead', ['f', SYS_FILE, 'n', SYS_INTEGER], [iffOverloaded], 'Read');
   RegisterInternalStringFunction(TFileRead3Func, 'FileRead', ['f', SYS_FILE], [iffOverloaded], 'Read');
   RegisterInternalStringFunction(TFileRead4Func, 'FileRead', ['name', SYS_STRING], [iffOverloaded], '');
   RegisterInternalIntFunction(TFileWrite1Func, 'FileWrite', ['f', SYS_FILE, 'buf', SYS_STRING], [iffOverloaded], 'Write');
   RegisterInternalIntFunction(TFileWrite2Func, 'FileWrite', ['name', SYS_STRING, 'buf', SYS_STRING], [iffOverloaded], '');
   RegisterInternalIntFunction(TFileSeekFunc, 'FileSeek', ['f', SYS_FILE, 'offset', SYS_INTEGER, 'origin', SYS_INTEGER], [], 'Seek');
   RegisterInternalBoolFunction(TFileFlushBuffersFunc, 'FileFlushBuffers', ['f', SYS_FILE]);

   RegisterInternalProcedure(TFileCloseFunc, 'FileClose', ['f', SYS_FILE], 'Close');

   RegisterInternalIntFunction(TFileSizeFileFunc, 'FileSize', ['f', SYS_FILE], [iffOverloaded], 'Size');
   RegisterInternalIntFunction(TFileSizeNameFunc, 'FileSize', ['name', SYS_STRING], [iffOverloaded]);

   RegisterInternalBoolFunction(TFileSetSizeFileFunc, 'FileSetSize', ['f', SYS_FILE, 'newSize', SYS_INTEGER], [], 'SetSize');

   RegisterInternalFloatFunction(TFileDateTimeFileFunc, 'FileDateTime', ['f', SYS_FILE], [iffOverloaded], 'DateTime');
   RegisterInternalFloatFunction(TFileDateTimeNameFunc, 'FileDateTime', ['name', SYS_STRING], [iffOverloaded]);
   RegisterInternalFloatFunction(TFileAccessDateTimeFunc, 'FileAccessDateTime', ['name', SYS_STRING], []);
   RegisterInternalProcedure(TFileSetDateTimeFileProc, 'FileSetDateTime', ['f', SYS_FILE, 'dt', SYS_FLOAT], 'SetDateTime', [iffOverloaded]);
   RegisterInternalProcedure(TFileSetDateTimeNameProc, 'FileSetDateTime', ['name', SYS_STRING, 'dt', SYS_FLOAT], '', [iffOverloaded]);

   RegisterInternalBoolFunction(TFileExistsFunc, 'FileExists', ['name', SYS_STRING], []);
   RegisterInternalBoolFunction(TDirectoryExistsFunc, 'DirectoryExists', ['name', SYS_STRING], []);

   RegisterInternalStringFunction(TExpandFileNameFunc, 'ExpandFileName', ['pathName', SYS_STRING], []);
   RegisterInternalStringFunction(TExtractFilePathFunc, 'ExtractFilePath', ['pathName', SYS_STRING], [iffStateLess]);
   RegisterInternalStringFunction(TExtractFileNameFunc, 'ExtractFileName', ['pathName', SYS_STRING], [iffStateLess]);
   RegisterInternalStringFunction(TExtractFileExtFunc, 'ExtractFileExt', ['pathName', SYS_STRING], [iffStateLess]);
   RegisterInternalStringFunction(TChangeFileExtFunc, 'ChangeFileExt', ['fileName', SYS_STRING, 'ext', SYS_STRING], [iffStateLess]);

   RegisterInternalBoolFunction(TDeleteFileFunc, 'DeleteFile', ['name', SYS_STRING], []);
   RegisterInternalBoolFunction(TCopyFileFunc, 'CopyFile', ['existingName', SYS_STRING, 'newName', SYS_STRING, 'failIfExist', SYS_BOOLEAN], []);
   RegisterInternalBoolFunction(TMoveFileFunc, 'MoveFile', ['existingFileName', SYS_STRING, 'newFileName', SYS_STRING]);
   RegisterInternalBoolFunction(TRenameFileFunc, 'RenameFile', ['oldName', SYS_STRING, 'newName', SYS_STRING], []);

   RegisterInternalBoolFunction(TForceDirectoriesFunc, 'ForceDirectories', ['path', SYS_STRING], []);
   RegisterInternalBoolFunction(TCreateDirFunc, 'CreateDir', ['path', SYS_STRING], []);
   RegisterInternalBoolFunction(TRemoveDirFunc, 'RemoveDir', ['path', SYS_STRING, 'evenIfNotEmpty=False', SYS_BOOLEAN], []);

   RegisterInternalFunction(TEnumerateDirFunc, 'EnumerateDir', ['path', SYS_STRING, 'mask', SYS_STRING, 'recursive', SYS_BOOLEAN], SYS_ARRAY_OF_STRING, []);
   RegisterInternalFunction(TEnumerateSubDirsFunc, 'EnumerateSubDirs', ['path', SYS_STRING], SYS_ARRAY_OF_STRING, []);

end.

