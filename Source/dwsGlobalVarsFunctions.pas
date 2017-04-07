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
{: GlobalVariables for DWS<p>

   This unit implements global variables functions, that allow scripts to read
   and write to variables across a script's context.<br>
   Details:<ul>
   <li>Variables can be declared and read from any script, or from Delphi code
   <li>Read/Write access is thread-safe
   <li>Variables names are <b>case sensitive</b>
   </ul><p>

   The global vars can be saved/restored as a whole from Delphi code (delphi
   code only as of now, mainly for security reasons) to a file, string or stream.
}
unit dwsGlobalVarsFunctions;

{$I dws.inc}

interface

uses
   Variants, Windows, Classes, SysUtils, Masks,
   dwsXPlatform, dwsUtils, dwsStrings, dwsExprList, dwsConstExprs, dwsErrors,
   dwsFunctions, dwsExprs, dwsSymbols, dwsMagicExprs, dwsDataContext,
   dwsGlobalVars, dwsScriptSource;

type

   TReadGlobalVarFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TReadGlobalVarDefFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TTryReadGlobalVarFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TWriteGlobalVarFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TWriteGlobalVarExpireFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TIncrementGlobalVarFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TCompareExchangeGlobalVarFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TDeleteGlobalVarFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TCleanupGlobalVarsFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TGlobalVarsNamesFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TGlobalVarsNamesCommaText = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   TSaveGlobalVarsToString = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   TLoadGlobalVarsFromString = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TGlobalQueuePushFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TGlobalQueueInsertFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TGlobalQueuePullFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TGlobalQueuePopFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TGlobalQueueLengthFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TCleanupGlobalQueuesFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TdwsGlobalVarsFunctions = class(TComponent)
   end;

   EGlobalVarError = class (Exception)
   end;

   TReadPrivateVarFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TWritePrivateVarFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TCleanupPrivateVarsFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TCompareExchangePrivateVarFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TPrivateVarsNamesFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

{: Directly write a global var.<p> }
function WriteGlobalVar(const aName: String; const aValue: Variant; expirationSeconds : Double) : Boolean;
{: Directly read a global var.<p> }
function ReadGlobalVar(const aName: String): Variant; inline;
function TryReadGlobalVar(const aName: String; var value: Variant): Boolean;
{: Directly read a global var, using a default value if variable does not exists.<p> }
function ReadGlobalVarDef(const aName: String; const aDefault: Variant): Variant;
{: Increments an integer global var. If not an integer, conversion is attempted.<p>
   Returns the value after the incrementation }
function IncrementGlobalVar(const aName : String; const delta : Int64) : Int64;
{: Compares aName with comparand, if equal exchanges with value, returns initial value of aName }
function CompareExchangeGlobalVar(const aName : String; const value, comparand : Variant) : Variant;
{: Delete specified global var if it exists. }
function DeleteGlobalVar(const aName : String) : Boolean;
{: Resets all global vars.<p> }
procedure CleanupGlobalVars(const filter : String = '*');

{: Save current global vars and their values to a String. }
function SaveGlobalVarsToString : RawByteString;
{: Load global vars and their values to a file. }
procedure LoadGlobalVarsFromString(const srcString : RawByteString);
{: Save current global vars and their values to a file. }
procedure SaveGlobalVarsToFile(const destFileName : String);
{: Load global vars and their values to a file. }
procedure LoadGlobalVarsFromFile(const srcFileName : String);
{: Save current global vars and their values to a file. }
procedure SaveGlobalVarsToStream(destStream : TStream);
{: Load global vars and their values to a file. }
procedure LoadGlobalVarsFromStream(srcStream : TStream);

{: Push to global queue and return count (after push) }
function GlobalQueuePush(const aName : String; const aValue : Variant) : Integer;
{: Insert to global queue and return count (after insert) }
function GlobalQueueInsert(const aName : String; const aValue : Variant) : Integer;
function GlobalQueuePull(const aName : String; var aValue : Variant) : Boolean;
function GlobalQueuePop(const aName : String; var aValue : Variant) : Boolean;
function GlobalQueueLength(const aName : String) : Integer;
procedure CleanupGlobalQueues(const filter : String = '*');

function InternalGlobalVars : PGlobalVars;
function InternalPrivateVars : PGlobalVars;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type

   TGlobalQueue = TSimpleQueue<Variant>;

   TNameGlobalQueueHash = class(TSimpleNameObjectHash<TGlobalQueue>)
      function GetOrCreate(const aName : String) : TGlobalQueue;
   end;

var
   vGlobalVars : TGlobalVars;
   vPrivateVars : TGlobalVars;
   vGlobalQueuesCS : TMultiReadSingleWrite;
   vGlobalQueues : TNameGlobalQueueHash;

const
   cGlobalVarsFiles : AnsiString = 'GBF 2.0';

function PrivateVarPrefix(const args : TExprBaseListExec) : String;
var
   scriptPos : TScriptPos;
   expr : TPosDataExpr;
begin
   expr:=(args.Expr as TPosDataExpr);
   scriptPos:=expr.ScriptPos;
   if scriptPos.IsMainModule then
      raise Exception.Create('Private variables cannot be referred from main module');
   Result:=scriptPos.SourceName+' ';
end;

function PrivateVarName(const args : TExprBaseListExec) : String;
begin
   Result := PrivateVarPrefix(args) + args.AsString[0];
end;

// ------------------
// ------------------ stubs ------------------
// ------------------

// WriteGlobalVar
//
function WriteGlobalVar(const aName : String; const aValue : Variant; expirationSeconds : Double) : Boolean;
begin
   Result:=vGlobalVars.Write(aName, aValue, expirationSeconds);
end;

// ReadGlobalVarDef
//
function ReadGlobalVarDef(const aName : String; const aDefault : Variant) : Variant;
begin
   if not vGlobalVars.TryRead(aName, Result) then
      Result:=aDefault;
end;

// IncrementGlobalVar
//
function IncrementGlobalVar(const aName : String; const delta : Int64) : Int64;
begin
   Result:=vGlobalVars.Increment(aName, delta);
end;

// CompareExchangeGlobalVar
//
function CompareExchangeGlobalVar(const aName : String; const value, comparand : Variant) : Variant;
begin
   Result:=vGlobalVars.CompareExchange(aName, value, comparand);
end;

// ReadGlobalVar
//
function ReadGlobalVar(const aName : String) : Variant;
begin
   // Result (empty) is our default value when calling...
   if not TryReadGlobalVar(aName, Result) then
      VarClearSafe(Result);
end;

// TryReadGlobalVar
//
function TryReadGlobalVar(const aName: String; var value: Variant): Boolean;
begin
   Result:=vGlobalVars.TryRead(aName, value);
end;

// DeleteGlobalVar
//
function DeleteGlobalVar(const aName : String) : Boolean;
begin
   Result:=vGlobalVars.Delete(aName);
end;

// CleanupGlobalVars
//
procedure CleanupGlobalVars(const filter : String = '*');
begin
   vGlobalVars.Cleanup(filter);
end;

// SaveGlobalVarsToString
//
function SaveGlobalVarsToString : RawByteString;
var
   wobs : TWriteOnlyBlockStream;
begin
   wobs:=TWriteOnlyBlockStream.Create;
   try
      SaveGlobalVarsToStream(wobs);
      Result:=wobs.ToRawBytes;
   finally
      wobs.Free;
   end;
end;

// LoadGlobalVarsFromString
//
procedure LoadGlobalVarsFromString(const srcString : RawByteString);
var
  ms : TMemoryStream;
begin
   if srcString='' then
      CleanupGlobalVars
   else begin
      ms:=TMemoryStream.Create;
      try
         ms.SetSize(Length(srcString));
         Move(srcString[1], ms.Memory^, Length(srcString));
         LoadGlobalVarsFromStream(ms);
      finally
         ms.Free;
      end;
   end;
end;

procedure SaveGlobalVarsToFile(const destFileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(destFileName, fmCreate);
   try
      SaveGlobalVarsToStream(fs);
   finally
      fs.Free;
   end;
end;

procedure LoadGlobalVarsFromFile(const srcFileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(srcFileName, fmOpenRead+fmShareDenyWrite);
   try
      LoadGlobalVarsFromStream(fs);
   finally
      fs.Free;
   end;
end;

// SaveGlobalVarsToStream
//
procedure SaveGlobalVarsToStream(destStream : TStream);
var
   writer : TWriter;
begin
   writer:=TWriter.Create(destStream, 16384);
   try
      writer.Write(cGlobalVarsFiles[1], Length(cGlobalVarsFiles));
      vGlobalVars.SaveToFiler(writer);
   finally
      writer.Free;
   end;
end;

// LoadGlobalVarsFromStream
//
procedure LoadGlobalVarsFromStream(srcStream : TStream);
var
   reader : TReader;
   fileTag : AnsiString;
begin
   reader:=TReader.Create(srcStream, 16384);
   try
      SetLength(fileTag, Length(cGlobalVarsFiles));
      if (srcStream.Size-srcStream.Position)>=Length(cGlobalVarsFiles) then
         reader.Read(fileTag[1], Length(cGlobalVarsFiles))
      else fileTag:='';
      if fileTag<>cGlobalVarsFiles then
         raise EGlobalVarError.Create('Invalid file tag');

      vGlobalVars.LoadFromFiler(reader);
   finally
      reader.Free;
   end;
end;

// GetOrCreate
//
function TNameGlobalQueueHash.GetOrCreate(const aName : String) : TGlobalQueue;
begin
   Result:=Objects[aName];
   if Result=nil then begin
      Result:=TGlobalQueue.Create;
      Objects[aName]:=Result;
   end;
end;

// GlobalQueuePush
//
function GlobalQueuePush(const aName : String; const aValue : Variant) : Integer;
var
   gq : TGlobalQueue;
begin
   vGlobalQueuesCS.BeginWrite;
   try
      gq:=vGlobalQueues.GetOrCreate(aName);
      gq.Push(aValue);
      Result:=gq.Count;
   finally
      vGlobalQueuesCS.EndWrite;
   end;
end;

// GlobalQueueInsert
//
function GlobalQueueInsert(const aName : String; const aValue : Variant) : Integer;
var
   gq : TGlobalQueue;
begin
   vGlobalQueuesCS.BeginWrite;
   try
      gq:=vGlobalQueues.GetOrCreate(aName);
      gq.Insert(aValue);
      Result:=gq.Count;
   finally
      vGlobalQueuesCS.EndWrite;
   end;
end;

// GlobalQueuePull
//
function GlobalQueuePull(const aName : String; var aValue : Variant) : Boolean;
var
   gq : TGlobalQueue;
begin
   vGlobalQueuesCS.BeginWrite;
   try
      gq:=vGlobalQueues.Objects[aName];
      if gq<>nil then
         Result:=gq.Pull(aValue)
      else Result:=False;
   finally
      vGlobalQueuesCS.EndWrite;
   end;
end;

// GlobalQueuePop
//
function GlobalQueuePop(const aName : String; var aValue : Variant) : Boolean;
var
   gq : TGlobalQueue;
begin
   vGlobalQueuesCS.BeginWrite;
   try
      gq:=vGlobalQueues.Objects[aName];
      if gq<>nil then
         Result:=gq.Pop(aValue)
      else Result:=False;
   finally
      vGlobalQueuesCS.EndWrite;
   end;
end;

// GlobalQueueLength
//
function GlobalQueueLength(const aName : String) : Integer;
var
   gq : TGlobalQueue;
begin
   vGlobalQueuesCS.BeginRead;
   try
      gq:=vGlobalQueues.Objects[aName];
      if gq<>nil then
         Result:=gq.Count
      else Result:=0;
   finally
      vGlobalQueuesCS.EndRead;
   end;
end;

// CleanupGlobalQueues
//
procedure CleanupGlobalQueues(const filter : String = '*');
var
   i : Integer;
   mask : TMask;
   gq : TGlobalQueue;
begin
   if filter='*' then begin
      vGlobalQueuesCS.BeginWrite;
      try
         vGlobalQueues.Clean;
      finally
         vGlobalQueuesCS.EndWrite;
      end;
   end else begin
      mask:=TMask.Create(filter);
      vGlobalQueuesCS.BeginWrite;
      try
         for i:=0 to vGlobalQueues.HighIndex do begin
            gq:=vGlobalQueues.BucketObject[i];
            if (gq<>nil) and mask.Matches(vGlobalQueues.BucketName[i]) then begin
               gq.Free;
               vGlobalQueues.BucketObject[i]:=nil;
            end;
         end;
      finally
         vGlobalQueuesCS.EndWrite;
         mask.Free;
      end;
   end;
end;

// InternalGlobalVars
//
function InternalGlobalVars : PGlobalVars;
begin
   Result := @vGlobalVars;
end;

// InternalPrivateVars
//
function InternalPrivateVars : PGlobalVars;
begin
   Result := @vPrivateVars;
end;

{ TReadGlobalVarFunc }

procedure TReadGlobalVarFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   if not vGlobalVars.TryRead(args.AsString[0], Result) then
      VarClearSafe(Result);
end;

{ TReadGlobalVarDefFunc }

procedure TReadGlobalVarDefFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   if not vGlobalVars.TryRead(args.AsString[0], Result) then
      args.ExprBase[1].EvalAsVariant(args.Exec, Result);
end;

{ TTryReadGlobalVarFunc }

function TTryReadGlobalVarFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   v : Variant;
begin
   Result:=vGlobalVars.TryRead(args.AsString[0], v);
   if Result then
      args.ExprBase[1].AssignValue(args.Exec, v);
end;

{ TWriteGlobalVarFunc }

function TWriteGlobalVarFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   buf : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, buf);
   Result:=vGlobalVars.Write(args.AsString[0], buf, 0);
end;

{ TWriteGlobalVarExpireFunc }

function TWriteGlobalVarExpireFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   buf : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, buf);
   Result:=vGlobalVars.Write(args.AsString[0], buf, args.AsFloat[2]);
end;

// ------------------
// ------------------ TIncrementGlobalVarFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TIncrementGlobalVarFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=vGlobalVars.Increment(args.AsString[0], args.AsInteger[1]);
end;

{ TCompareExchangeGlobalVarFunc }

procedure TCompareExchangeGlobalVarFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   value, comparand : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, value);
   args.ExprBase[2].EvalAsVariant(args.Exec, comparand);
   result:=vGlobalVars.CompareExchange(args.AsString[0], value, comparand);
end;

{ TDeleteGlobalVarFunc }

function TDeleteGlobalVarFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=vGlobalVars.Delete(args.AsString[0]);
end;

{ TCleanupGlobalVarsFunc }

procedure TCleanupGlobalVarsFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   vGlobalVars.Cleanup(args.AsString[0]);
end;

// ------------------
// ------------------ TGlobalVarsNamesFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TGlobalVarsNamesFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   newArray : TScriptDynamicStringArray;
   typString : TTypeSymbol;
begin
   typString:=(args.Exec as TdwsProgramExecution).CompilerContext.TypString;
   newArray:=TScriptDynamicArray.CreateNew(typString) as TScriptDynamicStringArray;
   result:=IScriptDynArray(newArray);
   vGlobalVars.EnumerateNames(args.AsString[0], newArray.Add);
end;

{ TGlobalVarsNamesCommaText }

procedure TGlobalVarsNamesCommaText.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result:=vGlobalVars.NamesCommaText;
end;

{ TSaveGlobalVarsToString }

// DoEvalAsString
//
procedure TSaveGlobalVarsToString.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result:=RawByteStringToScriptString(SaveGlobalVarsToString);
end;

{ TLoadGlobalVarsFromString }

procedure TLoadGlobalVarsFromString.DoEvalProc(const args : TExprBaseListExec);
begin
   LoadGlobalVarsFromString(args.AsDataString[0]);
end;

// ------------------
// ------------------ TGlobalQueuePushFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TGlobalQueuePushFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buf : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, buf);
   Result:=GlobalQueuePush(args.AsString[0], buf);
end;

// ------------------
// ------------------ TGlobalQueueInsertFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TGlobalQueueInsertFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   buf : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, buf);
   Result:=GlobalQueueInsert(args.AsString[0], buf);
end;

// ------------------
// ------------------ TGlobalQueuePullFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TGlobalQueuePullFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   v : Variant;
begin
   Result:=GlobalQueuePull(args.AsString[0], v);
   if Result then
      args.ExprBase[1].AssignValue(args.Exec, v);
end;

// ------------------
// ------------------ TGlobalQueuePopFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TGlobalQueuePopFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   v : Variant;
begin
   Result:=GlobalQueuePop(args.AsString[0], v);
   if Result then
      args.ExprBase[1].AssignValue(args.Exec, v);
end;

// ------------------
// ------------------ TCleanupGlobalQueuesFunc ------------------
// ------------------

// DoEvalProc
//
procedure TCleanupGlobalQueuesFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   CleanupGlobalQueues(args.AsString[0]);
end;

// ------------------
// ------------------ TGlobalQueueLengthFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TGlobalQueueLengthFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=GlobalQueueLength(args.AsString[0]);
end;

// ------------------
// ------------------ TReadPrivateVarFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TReadPrivateVarFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   if not vPrivateVars.TryRead(PrivateVarName(args), Result) then
      args.ExprBase[1].EvalAsVariant(args.Exec, Result);
end;

// ------------------
// ------------------ TWritePrivateVarFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TWritePrivateVarFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   buf : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, buf);
   Result:=vPrivateVars.Write(PrivateVarName(args), buf, args.AsFloat[2]);
end;

{ TCleanupPrivateVarsFunc }

procedure TCleanupPrivateVarsFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   vPrivateVars.Cleanup(PrivateVarName(args));
end;

{ TCompareExchangePrivateVarFunc }

procedure TCompareExchangePrivateVarFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   value, comparand : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, value);
   args.ExprBase[2].EvalAsVariant(args.Exec, comparand);
   result := vPrivateVars.CompareExchange(PrivateVarName(args), value, comparand);
end;

{ TPrivateVarsNamesFunc }

type
   TPrivateVarEnumerator = class
      FArray : TScriptDynamicStringArray;
      FOffset : Integer;
      procedure Add(const s : String);
   end;
procedure TPrivateVarEnumerator.Add(const s : String);
begin
   FArray.Add(Copy(s, FOffset));
end;
procedure TPrivateVarsNamesFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   typString : TTypeSymbol;
   filter, prefix : String;
   enum : TPrivateVarEnumerator;
begin
   typString:=(args.Exec as TdwsProgramExecution).CompilerContext.TypString;
   enum := TPrivateVarEnumerator.Create;
   try
      enum.FArray := TScriptDynamicArray.CreateNew(typString) as TScriptDynamicStringArray;
      result := IScriptDynArray(enum.FArray);

      filter := args.AsString[0];
      if filter = '' then
         filter := '*';
      prefix := PrivateVarPrefix(args);
      enum.FOffset := Length(prefix)+1;
      vPrivateVars.EnumerateNames(prefix + filter, enum.Add);
   finally
      enum.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vGlobalVars.Initialize;
   vPrivateVars.Initialize;
   vGlobalQueuesCS:=TMultiReadSingleWrite.Create;
   vGlobalQueues:=TNameGlobalQueueHash.Create;

   RegisterInternalFunction(TReadGlobalVarFunc, 'ReadGlobalVar', ['n', SYS_STRING], SYS_VARIANT);
   RegisterInternalFunction(TReadGlobalVarDefFunc, 'ReadGlobalVarDef', ['n', SYS_STRING, 'd', SYS_VARIANT], SYS_VARIANT);
   RegisterInternalBoolFunction(TTryReadGlobalVarFunc, 'TryReadGlobalVar', ['n', SYS_STRING, '@v', SYS_VARIANT]);
   RegisterInternalBoolFunction(TWriteGlobalVarFunc, 'WriteGlobalVar', ['n', SYS_STRING, 'v', SYS_VARIANT], [iffOverloaded]);
   RegisterInternalBoolFunction(TWriteGlobalVarExpireFunc, 'WriteGlobalVar', ['n', SYS_STRING, 'v', SYS_VARIANT, 'e', SYS_FLOAT], [iffOverloaded]);
   RegisterInternalIntFunction(TIncrementGlobalVarFunc, 'IncrementGlobalVar', ['n', SYS_STRING, 'i=1', SYS_INTEGER]);
   RegisterInternalFunction(TCompareExchangeGlobalVarFunc, 'CompareExchangeGlobalVar', ['n', SYS_STRING, 'v', SYS_VARIANT, 'c', SYS_VARIANT], SYS_VARIANT);
   RegisterInternalBoolFunction(TDeleteGlobalVarFunc, 'DeleteGlobalVar', ['n', SYS_STRING]);
   RegisterInternalProcedure(TCleanupGlobalVarsFunc, 'CleanupGlobalVars', ['filter=*', SYS_STRING]);
   RegisterInternalStringFunction(TGlobalVarsNamesCommaText, 'GlobalVarsNamesCommaText', []);
   RegisterInternalFunction(TGlobalVarsNamesFunc, 'GlobalVarsNames', ['filter', SYS_STRING], SYS_ARRAY_OF_STRING);
   RegisterInternalStringFunction(TSaveGlobalVarsToString, 'SaveGlobalVarsToString', []);
   RegisterInternalProcedure(TLoadGlobalVarsFromString, 'LoadGlobalVarsFromString', ['s', SYS_STRING]);

   RegisterInternalFunction(TReadPrivateVarFunc, 'ReadPrivateVar', ['n', SYS_STRING, 'd=Unassigned', SYS_VARIANT], SYS_VARIANT);
   RegisterInternalBoolFunction(TWritePrivateVarFunc, 'WritePrivateVar', ['n', SYS_STRING, 'v', SYS_VARIANT, 'e=0', SYS_FLOAT]);
   RegisterInternalProcedure(TCleanupPrivateVarsFunc, 'CleanupPrivateVars', ['filter=*', SYS_STRING]);
   RegisterInternalFunction(TCompareExchangePrivateVarFunc, 'CompareExchangePrivateVar', ['n', SYS_STRING, 'v', SYS_VARIANT, 'c', SYS_VARIANT], SYS_VARIANT);
   RegisterInternalFunction(TPrivateVarsNamesFunc, 'PrivateVarsNames', ['filter', SYS_STRING], SYS_ARRAY_OF_STRING);

   RegisterInternalIntFunction(TGlobalQueuePushFunc, 'GlobalQueuePush', ['n', SYS_STRING, 'v', SYS_VARIANT]);
   RegisterInternalIntFunction(TGlobalQueueInsertFunc, 'GlobalQueueInsert', ['n', SYS_STRING, 'v', SYS_VARIANT]);
   RegisterInternalBoolFunction(TGlobalQueuePullFunc, 'GlobalQueuePull', ['n', SYS_STRING, '@v', SYS_VARIANT]);
   RegisterInternalBoolFunction(TGlobalQueuePopFunc, 'GlobalQueuePop', ['n', SYS_STRING, '@v', SYS_VARIANT]);
   RegisterInternalIntFunction(TGlobalQueueLengthFunc, 'GlobalQueueLength', ['n', SYS_STRING]);
   RegisterInternalProcedure(TCleanupGlobalQueuesFunc, 'CleanupGlobalQueues', ['filter=*', SYS_STRING]);

finalization

   vGlobalVars.Finalize;
   vPrivateVars.Finalize;

   vGlobalQueuesCS.Free;
   vGlobalQueuesCS:=nil;
   vGlobalQueues.Clean;
   vGlobalQueues.Free;
   vGlobalQueues:=nil;
  
end.
