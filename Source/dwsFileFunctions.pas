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
      function GetOSError : Integer;
      function IsValid : Boolean;

      function GetDateTime : TdwsDateTime;
      procedure SetDateTime(const dt : TdwsDateTime);

      function Seek(offset : Int64; origin : Integer) : Int64;
      function Size : Int64;
      function Position : Int64;

      function Read(buf : Pointer; nbBytes : Int64) : Int64;
      function ReadBuffer(nbBytes : Int64; var dest : String) : Int64;
      function Write(buf : Pointer; nbBytes : Int64) : Int64;

      function FlushBuffers : Boolean;
      function SetEndOfFile : Boolean;

      procedure Close;
   end;

   TdwsFileHandle = class (TInterfacedSelfObject, IdwsFileHandle)
      private
         FHandle : THandle;
         FOSError : Integer;

      public
         constructor Create(aHandle : THandle; osError : Integer);
         destructor Destroy; override;

         function GetOSError : Integer;
         function IsValid : Boolean;

         function GetDateTime : TdwsDateTime;
         procedure SetDateTime(const dt : TdwsDateTime);

         function Seek(offset : Int64; origin : Integer) : Int64;
         function Size : Int64;
         function Position : Int64;

         function Read(buf : Pointer; nbBytes : Int64) : Int64;
         function ReadBuffer(nbBytes : Int64; var dest : String) : Int64;
         function Write(buf : Pointer; nbBytes : Int64) : Int64;

         function FlushBuffers : Boolean;
         function SetEndOfFile : Boolean;

         procedure Close;
   end;

   TBaseFileSymbol = class (TBaseSymbol)
      public
         constructor Create;

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitDataContext(const data : IDataContext; offset : NativeInt); override;
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

   TFilePositionFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileFlushBuffersFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TFileCloseFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TFileReadLinesFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
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

   EdwsFileException = class (Exception);

function GetIdwsFileHandle(const args : TExprBaseListExec; index : Integer; checkValidity : Boolean = True) : IdwsFileHandle;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsDynamicArrays;

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

// CheckFileHandleValidity
//
procedure CheckFileHandleValidity(const ih : IdwsFileHandle);
var
   osError : Integer;
begin
   if ih = nil then
      raise EdwsFileException.Create('File is not open')
   else if not ih.IsValid then begin
      osError := ih.GetOSError;
      if osError = 0 then
         raise EdwsFileException.Create('File already closed')
      else raise EdwsFileException.CreateFmt('Invalid file handle (%d): %s',
                                     [ osError, SysErrorMessage(osError) ]);
   end;
end;

// GetIdwsFileHandle
//
function GetIdwsFileHandle(const args : TExprBaseListExec; index : Integer; checkValidity : Boolean = True) : IdwsFileHandle;
var
   v : Variant;
begin
   args.ExprBase[index].EvalAsVariant(args.Exec, v);
   if (TVarData(v).VType=varUnknown) and (TVarData(v).VUnknown<>nil) then begin
      Result := IdwsFileHandle(IUnknown(TVarData(v).VUnknown));
   end else Result := nil;

   if checkValidity then
      CheckFileHandleValidity(Result);
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
   if FHandle <> INVALID_HANDLE_VALUE then
      CloseFileHandle(FHandle);
end;

// GetOSError
//
function TdwsFileHandle.GetOSError : Integer;
begin
   Result:=FOSError;
end;

// IsValid
//
function TdwsFileHandle.IsValid : Boolean;
begin
   Result := FHandle <> INVALID_HANDLE_VALUE;
end;

// GetDateTime
//
function TdwsFileHandle.GetDateTime : TdwsDateTime;
begin
   Result := FileDateTime(FHandle);
end;

// SetDateTime
//
procedure TdwsFileHandle.SetDateTime(const dt : TdwsDateTime);
begin
   FileSetDateTime(FHandle, dt);
end;

// Seek
//
function TdwsFileHandle.Seek(offset : Int64; origin : Integer) : Int64;
begin
   Result := SysUtils.FileSeek(FHandle, offset, origin);
end;

// Size
//
function TdwsFileHandle.Size : Int64;
var
   p : Int64;
begin
   p := Seek(0, soFromCurrent);
   Result := Seek(0, soFromEnd);
   Seek(p, soFromBeginning);
end;

// Position
//
function TdwsFileHandle.Position : Int64;
begin
   Result := Seek(0, soFromCurrent);
end;

// Read
//
function TdwsFileHandle.Read(buf : Pointer; nbBytes : Int64) : Int64;
begin
   Result := FileRead(FHandle, buf, nbBytes);
end;

// ReadBuffer
//
function TdwsFileHandle.ReadBuffer(nbBytes : Int64; var dest : String) : Int64;

   function AdjustToRemainingSize(nbBytes : Int64) : Int64;
   var
      p, n : Int64;
   begin
      p := Seek(0, soFromCurrent);
      n := Seek(0, soFromEnd) - p;
      Seek(p, soFromBeginning);
      if nbBytes > n then
         Result := n
      else Result := nbBytes;
   end;

begin
   if nbBytes <= 0 then Exit(0);
   if nbBytes >= 100*1024 then begin
      // before alocating a crazy large buffer, check actual file size
      // but dont do it for smaller reads
      nbBytes := AdjustToRemainingSize(nbBytes);
   end;

   SetLength(dest, nbBytes);
   if Length(dest) <> nbBytes then
      raise EdwsFileException.CreateFmt('Buffer too large (%d)', [ nbBytes ]);

   Result := Read(Pointer(dest), nbBytes);
   if Result <> nbBytes then
      SetLength(dest, Result);
   BytesToWordsInPlace(Pointer(dest), Result);
end;

// Write
//
function TdwsFileHandle.Write(buf : Pointer; nbBytes : Int64) : Int64;
begin
   Result := dwsXPlatform.FileWrite(FHandle, buf, nbBytes);
end;

// FlushBuffers
//
function TdwsFileHandle.FlushBuffers : Boolean;
begin
   Result := dwsXPlatform.FileFlushBuffers(FHandle);
end;

// SetEndOfFile
//
function TdwsFileHandle.SetEndOfFile : Boolean;
begin
   Result := dwsXPlatform.SetEndOfFile(FHandle);
end;

// Close
//
procedure TdwsFileHandle.Close;
begin
   FOSError := 0;
   if FHandle <> INVALID_HANDLE_VALUE then begin
      if not CloseFileHandle(FHandle) then
         FOSError := GetLastError;
      FHandle := INVALID_HANDLE_VALUE;
   end;
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

// InitDataContext
//
procedure TBaseFileSymbol.InitDataContext(const data : IDataContext; offset : NativeInt);
begin
   data.SetNilInterface(offset);
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
   h := FileOpen(args.AsFileName[0], args.AsInteger[1]);
   i := TdwsFileHandle.Create(h, GetLastError);
   CheckFileHandleValidity(i);
   Result := IUnknown(i);
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
   h := FileOpen(args.AsFileName[0], fmOpenRead+fmShareDenyNone);
   i := TdwsFileHandle.Create(h, GetLastError);
   CheckFileHandleValidity(i);
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
   h := FileCreate(args.AsFileName[0]);
   i := TdwsFileHandle.Create(h, GetLastError);
   CheckFileHandleValidity(i);
   Result := IUnknown(i);
end;

// ------------------
// ------------------ TFileIsValidFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TFileIsValidFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   ih : IdwsFileHandle;
begin
   ih := GetIdwsFileHandle(args, 0, False);
   Result := (ih <> nil) and ih.IsValid;
end;

// ------------------
// ------------------ TFileRead1Func ------------------
// ------------------

// DoEvalAsInteger
//
function TFileRead1Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buf : String;
   ih : IdwsFileHandle;
begin
   ih := GetIdwsFileHandle(args, 0);
   Result := ih.ReadBuffer(args.AsInteger[2], buf);
   args.AsString[1] := buf;
end;

// ------------------
// ------------------ TFileRead2Func ------------------
// ------------------

// DoEvalAsString
//
procedure TFileRead2Func.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   GetIdwsFileHandle(args, 0).ReadBuffer(args.AsInteger[1], Result);
end;

// ------------------
// ------------------ TFileRead2Func ------------------
// ------------------

// DoEvalAsString
//
procedure TFileRead3Func.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
var
   p, n : Int64;
   ih : IdwsFileHandle;
begin
   ih := GetIdwsFileHandle(args, 0);
   p := ih.Seek(0, soFromCurrent);
   n := ih.Seek(0, soFromEnd)-p;
   if (p < 0) or (n < 0) or (ih.Seek(p, 0) < 0) then
      RaiseLastOSError;

   ih.ReadBuffer(n, Result);
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
   buf := args.AsDataString[1];
   Result := GetIdwsFileHandle(args, 0).Write(Pointer(buf), Length(buf));
end;

// ------------------
// ------------------ TFileWrite2Func ------------------
// ------------------

function TFileWrite2Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=SaveRawBytesToFile(args.AsFileName[0], args.AsDataString[1]);
end;

// ------------------
// ------------------ TFileSeekFunc ------------------
// ------------------

function TFileSeekFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := GetIdwsFileHandle(args, 0).Seek(args.AsInteger[1], args.AsInteger[2]);
end;

// ------------------
// ------------------ TFilePositionFunc ------------------
// ------------------

function TFilePositionFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := GetIdwsFileHandle(args, 0).Position;
end;

// ------------------
// ------------------ TFileFlushBuffersFunc ------------------
// ------------------

function TFileFlushBuffersFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := GetIdwsFileHandle(args, 0).FlushBuffers;
end;

// ------------------
// ------------------ TFileCloseFunc ------------------
// ------------------

procedure TFileCloseFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   GetIdwsFileHandle(args, 0).Close;
end;

// ------------------
// ------------------ TFileReadLinesFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TFileReadLinesFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   buf : String;
   bytes : TBytes;
   dyn : IScriptDynArray;
   capacity, count : NativeInt;
   encoding : TEncoding;

   procedure FlushLine(i, i0 : NativeInt);
   var
      rawLength, decodedLength : NativeInt;
   begin
      if (i > 0) and (bytes[i-1] = 13) then
         rawLength := i - i0 - 1
      else rawLength := i - i0;
      if rawLength > 0 then begin
         if encoding = TEncoding.UTF8 then begin
            SetLength(buf, rawLength);
            decodedLength := Utf8ToUnicode(Pointer(buf), rawLength+1, @bytes[i0], rawLength)-1;
            if decodedLength <> rawLength then
               SetLength(buf, decodedLength);
         end else begin
            buf := encoding.GetString(bytes, i0, rawLength);
         end;
         dyn.AsString[count] := buf;
      end; // entries are already empty by default, no need for an "else"
      Inc(count);
      if count = capacity then begin
         // grow by 37.5%
         capacity := capacity + (capacity shr 2) + (capacity shr 3);
         dyn.ArrayLength := capacity;
      end;
   end;

var
   fileName : String;
   p : PAnsiChar;
   i, i0, n : NativeInt;
begin
   fileName := args.AsFileName[0];
   CreateNewDynamicArray((args.Exec as TdwsProgramExecution).CompilerContext.TypString, dyn);
   VarCopySafe(result, dyn);

   bytes := LoadDataFromFile(fileName);
   n := Length(bytes);
   if n = 0 then Exit;

   count := 0;
   // start by assuming average line length of 128 bytes, this is conservative
   // in terms of array element size, in case the file actually contains huge lines
   // (such as in minified js files)
   capacity := 8 + n div 128;
   dyn.ArrayLength := capacity;

   encoding := nil;
   i := TEncoding.GetBufferEncoding(bytes, encoding);
   if i = 0 then
      encoding := TEncoding.UTF8;
   i0 := i;
   p := PAnsiChar(bytes);
   while i < n do begin
      if p[i] = #10 then begin
         FlushLine(i, i0);
         i0 := i + 1;
      end;
      Inc(i);
   end;
   FlushLine(i, i0);
   dyn.ArrayLength := count;
end;

// ------------------
// ------------------ TFileSizeFileFunc ------------------
// ------------------

function TFileSizeFileFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := GetIdwsFileHandle(args, 0).Size;
end;

// ------------------
// ------------------ TFileSizeFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TFileSetSizeFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   ih : IdwsFileHandle;
begin
   ih := GetIdwsFileHandle(args, 0);
   ih.Seek(args.AsInteger[1], soFromBeginning);
   Result := ih.SetEndOfFile;
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
   Result := GetIdwsFileHandle(args, 0).GetDateTime.AsLocalDateTime;
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
   GetIdwsFileHandle(args, 0).SetDateTime(TdwsDateTime.FromLocalDateTime(args.AsFloat[1]));
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
var
   fileName : String;
begin
   fileName := args.AsFileName[0];
   Result := (fileName <> '') and FileExists(fileName)
end;

// ------------------
// ------------------ TDirectoryExistsFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TDirectoryExistsFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   directoryName : String;
begin
   directoryName := args.AsFileName[0];
   Result := (directoryName <> '') and DirectoryExists(args.AsFileName[0]);
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
   base : IScriptDynArray;
begin
   sl := TStringList.Create;
   try
      CollectFiles(args.AsFileName[0], args.AsString[1], sl, args.AsBoolean[2]);
      CreateNewDynamicArray((args.Exec as TdwsProgramExecution).CompilerContext.TypString, base);
      VarCopySafe(result, base);
      base.AddStrings(sl);
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
   newArray : IScriptDynArray;
begin
   sl := TStringList.Create;
   try
      CollectSubDirs(args.AsFileName[0], sl);
      CreateNewDynamicArray((args.Exec as TdwsProgramExecution).CompilerContext.TypString, newArray);
      VarCopySafe(Result, newArray);
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
   RegisterInternalIntFunction(TFilePositionFunc, 'FilePos', ['f', SYS_FILE], [], 'Position');
   RegisterInternalBoolFunction(TFileFlushBuffersFunc, 'FileFlushBuffers', ['f', SYS_FILE], [], 'FlushBuffers');

   RegisterInternalProcedure(TFileCloseFunc, 'FileClose', ['f', SYS_FILE], 'Close');

   RegisterInternalFunction(TFileReadLinesFunc, 'FileReadLines', ['name', SYS_STRING], SYS_ARRAY_OF_STRING, [], '');

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

