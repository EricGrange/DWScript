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
   Classes, SysUtils,
   dwsXPlatform, dwsUtils, dwsStrings,
   dwsFunctions, dwsSymbols, dwsExprs, dwsCoreExprs, dwsExprList, dwsUnitSymbols,
   dwsConstExprs, dwsMagicExprs, dwsDataContext;

const
   SYS_FILE = 'File';

type

   IdwsFileHandle = interface
      procedure SetHandle(h : THandle);
      function GetHandle : THandle;
   end;

   TdwsFileHandle = class (TInterfacedSelfObject, IdwsFileHandle)
      private
         FHandle : THandle;

      protected
         procedure SetHandle(h : THandle);
         function GetHandle : THandle;

      public
         constructor Create(aHandle : THandle);
         destructor Destroy; override;

         property Handle : THandle read FHandle write FHandle;
   end;

   TBaseFileSymbol = class (TBaseVariantSymbol)
      public
         constructor Create;

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;
   end;

   TFileOpenFunc = class(TInternalMagicVariantFunction)
      function DoEvalAsVariant(const args : TExprBaseListExec) : Variant; override;
   end;

   TFileCreateFunc = class(TInternalMagicVariantFunction)
      function DoEvalAsVariant(const args : TExprBaseListExec) : Variant; override;
   end;

   TFileRead1Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileRead2Func = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TFileWriteFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileSeekFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFileCloseFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TFileExistsFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TDirectoryExistsFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TDeleteFileFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TCopyFileFunc = class(TInternalMagicBoolFunction)
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
   typFile : TBaseVariantSymbol;
begin
   typFile:=TBaseFileSymbol.Create;

   systemTable.AddSymbol(typFile);
end;

// GetFileHandle
//
function GetFileHandle(const args : TExprBaseListExec; index : Integer) : THandle;
var
   v : Variant;
begin
   args.ExprBase[index].EvalAsVariant(args.Exec, v);
   if (TVarData(v).VType=varUnknown) and (TVarData(v).VUnknown<>nil) then
      Result:=IdwsFileHandle(IUnknown(TVarData(v).VUnknown)).GetHandle
   else Result:=0;
end;

// ------------------
// ------------------ TdwsFileHandle ------------------
// ------------------

// Create
//
constructor TdwsFileHandle.Create(aHandle : THandle);
begin
   FHandle:=aHandle;
end;

// Destroy
//
destructor TdwsFileHandle.Destroy;
begin
   if FHandle<>0 then
      CloseFileHandle(FHandle);
end;

// SetHandle
//
procedure TdwsFileHandle.SetHandle(h : THandle);
begin
   FHandle:=h;
end;

// GetHandle
//
function TdwsFileHandle.GetHandle : THandle;
begin
   Result:=FHandle;
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
function TFileOpenFunc.DoEvalAsVariant(const args : TExprBaseListExec) : Variant;
var
   h : THandle;
   i : IdwsFileHandle;
begin
   h:=FileOpen(args.AsString[0], args.AsInteger[1]);
   i:=TdwsFileHandle.Create(h);
   Result:=IUnknown(i);
end;

// ------------------
// ------------------ TFileCreateFunc ------------------
// ------------------

// DoEvalAsVariant
//
function TFileCreateFunc.DoEvalAsVariant(const args : TExprBaseListExec) : Variant;
var
   h : THandle;
   i : IdwsFileHandle;
begin
   h:=FileCreate(args.AsString[0]);
   i:=TdwsFileHandle.Create(h);
   Result:=IUnknown(i);
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
      SetLength(buf, n);
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
      SetLength(buf, n);
   end;
   RawByteStringToScriptString(buf, Result);
end;

// ------------------
// ------------------ TFileWriteFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TFileWriteFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buf : RawByteString;
begin
   buf:=args.AsDataString[1];
   Result:=FileWrite(GetFileHandle(args, 0), Pointer(buf)^, Length(buf));
end;

// ------------------
// ------------------ TFileSeekFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TFileSeekFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=FileSeek(GetFileHandle(args, 0), args.AsInteger[1], args.AsInteger[2]);
end;

// ------------------
// ------------------ TFileCloseFunc ------------------
// ------------------

// DoEvalProc
//
procedure TFileCloseFunc.DoEvalProc(const args : TExprBaseListExec);
var
   v : Variant;
   i : IdwsFileHandle;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, v);
   if (TVarData(v).VType=varUnknown) and (TVarData(v).VUnknown<>nil) then begin
      i:=IdwsFileHandle(IUnknown(TVarData(v).VUnknown));
      CloseFileHandle(i.GetHandle);
      i.SetHandle(0);
   end;
end;

// ------------------
// ------------------ TFileExistsFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TFileExistsFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileExists(args.AsString[0]);
end;

// ------------------
// ------------------ TDirectoryExistsFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TDirectoryExistsFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=DirectoryExists(args.AsString[0]);
end;

// ------------------
// ------------------ TDeleteFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TDeleteFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileDelete(args.AsString[0]);
end;

// ------------------
// ------------------ TCopyFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TCopyFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileCopy(args.AsString[0], args.AsString[1], args.AsBoolean[2]);
end;

// ------------------
// ------------------ TRenameFileFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TRenameFileFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=FileRename(args.AsString[0], args.AsString[1]);
end;

// ------------------
// ------------------ TForceDirectoriesFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TForceDirectoriesFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=ForceDirectories(args.AsString[0]);
end;

// ------------------
// ------------------ TCreateDirFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TCreateDirFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=CreateDir(args.AsString[0]);
end;

// ------------------
// ------------------ TRemoveDirFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TRemoveDirFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=RemoveDir(args.AsString[0]);
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
   RegisterInternalFunction(TFileOpenFunc, 'FileCreate', ['name', SYS_STRING], SYS_FILE, []);

   RegisterInternalIntFunction(TFileRead1Func, 'FileRead', ['f', SYS_FILE, '@buf', SYS_STRING, 'n', SYS_INTEGER], [iffOverloaded]);
   RegisterInternalStringFunction(TFileRead2Func, 'FileRead', ['f', SYS_FILE, 'n', SYS_INTEGER], [iffOverloaded]);
   RegisterInternalIntFunction(TFileWriteFunc, 'FileWrite', ['f', SYS_FILE, 'buf', SYS_STRING], []);
   RegisterInternalIntFunction(TFileSeekFunc, 'FileSeek', ['f', SYS_FILE, 'offset', SYS_INTEGER, 'origin', SYS_INTEGER], []);

   RegisterInternalProcedure(TFileSeekFunc, 'FileClose', ['f', SYS_FILE]);

   RegisterInternalBoolFunction(TFileExistsFunc, 'FileExists', ['name', SYS_STRING], []);
   RegisterInternalBoolFunction(TDirectoryExistsFunc, 'DirectoryExists', ['name', SYS_STRING], []);

   RegisterInternalBoolFunction(TDeleteFileFunc, 'DeleteFile', ['name', SYS_STRING], []);
   RegisterInternalBoolFunction(TCopyFileFunc, 'CopyFile', ['existingName', SYS_STRING, 'newName', SYS_STRING, 'failIfExist', SYS_BOOLEAN], []);
   RegisterInternalBoolFunction(TRenameFileFunc, 'RenameFile', ['oldName', SYS_STRING, 'newName', SYS_STRING], []);

   RegisterInternalBoolFunction(TForceDirectoriesFunc, 'ForceDirectories', ['path', SYS_STRING], []);
   RegisterInternalBoolFunction(TCreateDirFunc, 'CreateDir', ['path', SYS_STRING], []);
   RegisterInternalBoolFunction(TRemoveDirFunc, 'RemoveDir', ['path', SYS_STRING], []);

end.

