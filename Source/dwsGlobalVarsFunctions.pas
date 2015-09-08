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
   dwsXPlatform, dwsUtils, dwsStrings, dwsExprList, dwsConstExprs,
   dwsFunctions, dwsExprs, dwsSymbols, dwsMagicExprs, dwsDataContext;

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
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TSaveGlobalVarsToString = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TLoadGlobalVarsFromString = class(TInternalFunction)
      procedure Execute(info : TProgramInfo); override;
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

{: Directly write a global var.<p> }
function WriteGlobalVar(const aName: UnicodeString; const aValue: Variant; expirationSeconds : Double) : Boolean;
{: Directly read a global var.<p> }
function ReadGlobalVar(const aName: UnicodeString): Variant; inline;
function TryReadGlobalVar(const aName: UnicodeString; var value: Variant): Boolean;
{: Directly read a global var, using a default value if variable does not exists.<p> }
function ReadGlobalVarDef(const aName: UnicodeString; const aDefault: Variant): Variant; inline;
{: Increments an integer global var. If not an integer, conversion is attempted.<p>
   Returns the value after the incrementation }
function IncrementGlobalVar(const aName : UnicodeString; const delta : Int64) : Int64;
{: Compares aName with comparand, if equal exchanges with value, returns initial value of aName }
function CompareExchangeGlobalVar(const aName : UnicodeString; const value, comparand : Variant) : Variant;
{: Delete specified global var if it exists. }
function DeleteGlobalVar(const aName : UnicodeString) : Boolean;
{: Resets all global vars.<p> }
procedure CleanupGlobalVars(const filter : String = '*');

{: Save current global vars and their values to a UnicodeString. }
function SaveGlobalVarsToString : RawByteString;
{: Load global vars and their values to a file. }
procedure LoadGlobalVarsFromString(const srcString : RawByteString);
{: Save current global vars and their values to a file. }
procedure SaveGlobalVarsToFile(const destFileName : UnicodeString);
{: Load global vars and their values to a file. }
procedure LoadGlobalVarsFromFile(const srcFileName : UnicodeString);
{: Save current global vars and their values to a file. }
procedure SaveGlobalVarsToStream(destStream : TStream);
{: Load global vars and their values to a file. }
procedure LoadGlobalVarsFromStream(srcStream : TStream);

{: CommaText of the names of all global vars. }
procedure CollectGlobalVarsNames(const filter : String; dest : TStrings);
{: CommaText of the names of all global vars. }
function GlobalVarsNamesCommaText : UnicodeString;

{: Push to global queue and return count (after push) }
function GlobalQueuePush(const aName : String; const aValue : Variant) : Integer;
{: Insert to global queue and return count (after insert) }
function GlobalQueueInsert(const aName : String; const aValue : Variant) : Integer;
function GlobalQueuePull(const aName : String; var aValue : Variant) : Boolean;
function GlobalQueuePop(const aName : String; var aValue : Variant) : Boolean;
function GlobalQueueLength(const aName : String) : Integer;
procedure CleanupGlobalQueues(const filter : String = '*');

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type

   TGlobalVar = class(TObject)
      private
         Value: Variant;
         Expire: Int64;

         procedure WriteToFiler(writer: TWriter; const Name : UnicodeString);
         procedure ReadFromFiler(reader: TReader; var Name : UnicodeString);
   end;

   TNameGlobalVarHash = TSimpleNameObjectHash<TGlobalVar>;

   TGlobalQueue = TSimpleQueue<Variant>;

   TNameGlobalQueueHash = class(TSimpleNameObjectHash<TGlobalQueue>)
      function GetOrCreate(const aName : String) : TGlobalQueue;
   end;

var
   vGlobalVarsCS : TMultiReadSingleWrite;
   vGlobalVars : TNameGlobalVarHash;
   vGlobalVarsNamesCache : UnicodeString;
   vGlobalQueuesCS : TMultiReadSingleWrite;
   vGlobalQueues : TNameGlobalQueueHash;

const
   cGlobalVarsFiles : AnsiString = 'GBF 2.0';
   cGlobalVarsLarge = 1024;

// WriteGlobalVar
//
function WriteGlobalVar(const aName : UnicodeString; const aValue : Variant; expirationSeconds : Double) : Boolean;
var
   gv : TGlobalVar;
   expire : Int64;
begin
   if expirationSeconds>0 then
      expire:=GetSystemMilliseconds+Round(expirationSeconds*1000)
   else expire:=0;
   vGlobalVarsCS.BeginWrite;
   try
      gv:=vGlobalVars.Objects[aName];
      if gv=nil then begin
         gv:=TGlobalVar.Create;
         vGlobalVars.Objects[aName]:=gv;
         gv.Value:=aValue;
         vGlobalVarsNamesCache:='';
         Result:=True;
      end else begin
         Result:=(VarType(gv.Value)<>VarType(aValue)) or (gv.Value<>aValue);
         if Result then
            gv.Value:=aValue;
      end;
      gv.Expire:=expire;
   finally
      vGlobalVarsCS.EndWrite;
   end;
end;

// ReadGlobalVarDef
//
function ReadGlobalVarDef(const aName : UnicodeString; const aDefault : Variant) : Variant;
begin
   if not TryReadGlobalVar(aName, Result) then
      Result:=aDefault;
end;

// IncrementGlobalVar
//
function IncrementGlobalVar(const aName : UnicodeString; const delta : Int64) : Int64;
var
   gv : TGlobalVar;
begin
   vGlobalVarsCS.BeginWrite;
   try
      gv:=vGlobalVars.Objects[aName];
      if gv=nil then begin
         vGlobalVarsNamesCache:='';
         gv:=TGlobalVar.Create;
         vGlobalVars.Objects[aName]:=gv;
         Result:=delta;
      end else begin
         if (gv.Expire=0) or (gv.Expire>GetSystemMilliseconds) then
            Result:=delta+gv.Value
         else Result:=delta;
      end;
      gv.Value:=Result;
   finally
      vGlobalVarsCS.EndWrite;
   end;
end;

// CompareExchangeGlobalVar
//
function CompareExchangeGlobalVar(const aName : UnicodeString; const value, comparand : Variant) : Variant;
var
   gv : TGlobalVar;
begin
   vGlobalVarsCS.BeginWrite;
   try
      gv:=vGlobalVars.Objects[aName];
      if (gv<>nil) and ((gv.Expire=0) or (gv.Expire>GetSystemMilliseconds)) then
         Result:=gv.Value
      else Result:=Unassigned;

      if (VarType(Result)=VarType(comparand)) and (Result=comparand) then begin
         if gv=nil then begin
            vGlobalVarsNamesCache:='';
            gv:=TGlobalVar.Create;
            vGlobalVars.Objects[aName]:=gv;
         end;
         gv.Value:=value;
      end;
   finally
      vGlobalVarsCS.EndWrite;
   end;
end;

// ReadGlobalVar
//
function ReadGlobalVar(const aName : UnicodeString) : Variant;
begin
   // Result (empty) is our default value when calling...
   if not TryReadGlobalVar(aName, Result) then
      VarClearSafe(Result);
end;

// TryReadGlobalVar
//
function TryReadGlobalVar(const aName: UnicodeString; var value: Variant): Boolean;
var
   gv : TGlobalVar;
begin
   vGlobalVarsCS.BeginRead;
   try
      gv:=vGlobalVars.Objects[aName];
      if     (gv<>nil)
         and ((gv.Expire=0) or (gv.Expire>GetSystemMilliseconds)) then begin
         value:=gv.Value;
         Exit(True);
      end;
   finally
      vGlobalVarsCS.EndRead;
   end;
   Result:=False;
end;

// DeleteGlobalVar
//
function DeleteGlobalVar(const aName : UnicodeString) : Boolean;
var
   gv : TGlobalVar;
   i : Integer;
begin
   gv:=nil;
   vGlobalVarsCS.BeginWrite;
   try
      i:=vGlobalVars.BucketIndex[aName];
      if i>=0 then begin
         gv:=vGlobalVars.BucketObject[i];
         if gv<>nil then begin
            vGlobalVars.BucketObject[i]:=nil;
            vGlobalVarsNamesCache:='';
         end;
      end
   finally
      vGlobalVarsCS.EndWrite;
   end;
   if gv<>nil then begin
      gv.Destroy;
      Result:=True;
   end else Result:=False;
end;

// CleanupGlobalVars
//
procedure CleanupGlobalVars(const filter : String = '*');
var
   i, n : Integer;
   mask : TMask;
   gv : TGlobalVar;
   rehash : TNameGlobalVarHash;
   expire : Int64;
begin
   if filter='*' then begin
      vGlobalVarsCS.BeginWrite;
      try
         vGlobalVars.Clean;
         vGlobalVarsNamesCache:='';
      finally
         vGlobalVarsCS.EndWrite;
      end;
   end else begin
      expire:=GetSystemMilliseconds;
      if filter='' then
         mask:=nil
      else mask:=TMask.Create(filter);
      vGlobalVarsCS.BeginWrite;
      try
         n:=0;
         for i:=0 to vGlobalVars.HighIndex do begin
            gv:=vGlobalVars.BucketObject[i];
            if gv=nil then
               Inc(n)
            else if    ((gv.Expire>0) and (gv.Expire<expire))
                    or ((mask<>nil) and mask.Matches(vGlobalVars.BucketName[i])) then begin
               gv.Free;
               vGlobalVars.BucketObject[i]:=nil;
               Inc(n);
            end;
         end;
         // if hash is large and 75% or more of the hash slots are nil, then rehash
         if (vGlobalVars.HighIndex>cGlobalVarsLarge) and (4*n>3*vGlobalVars.HighIndex) then begin
            // compute required capacity after rehash taking into account a 25% margin
            // (or 33%, depending on which way you look at the percentage)
            n:=vGlobalVars.HighIndex-n;
            n:=2*(n+n div 3)+1;
            i:=vGlobalVars.HighIndex+1;
            while i>n do
               i:=i shr 1;
            if i<=vGlobalVars.HighIndex then begin
               rehash:=TNameGlobalVarHash.Create(i);
               for i:=0 to vGlobalVars.HighIndex do begin
                  gv:=vGlobalVars.BucketObject[i];
                  if gv<>nil then
                     rehash.Objects[vGlobalVars.BucketName[i]]:=gv;
               end;
               vGlobalVars.Free;
               vGlobalVars:=rehash;
            end;
         end;
         vGlobalVarsNamesCache:='';
      finally
         vGlobalVarsCS.EndWrite;
         mask.Free;
      end;
   end;
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

procedure SaveGlobalVarsToFile(const destFileName : UnicodeString);
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

procedure LoadGlobalVarsFromFile(const srcFileName : UnicodeString);
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
   i : Integer;
   writer : TWriter;
   gv : TGlobalVar;
   expire : Int64;
begin
   expire:=GetSystemMilliseconds;
   writer:=TWriter.Create(destStream, 16384);
   try
      writer.Write(cGlobalVarsFiles[1], Length(cGlobalVarsFiles));
      writer.WriteListBegin;

      vGlobalVarsCS.BeginRead;
      try
         for i:=0 to vGlobalVars.HighIndex do begin
            gv:=vGlobalVars.BucketObject[i];
            if     (gv<>nil)
               and ((gv.Expire=0) or (gv.Expire<expire)) then
               gv.WriteToFiler(writer, vGlobalVars.BucketName[i]);
         end;
      finally
         vGlobalVarsCS.EndRead;
      end;

      writer.WriteListEnd;
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
   name : UnicodeString;
   gv : TGlobalVar;
begin
   reader:=TReader.Create(srcStream, 16384);
   try
      SetLength(fileTag, Length(cGlobalVarsFiles));
      if (srcStream.Size-srcStream.Position)>=Length(cGlobalVarsFiles) then
         reader.Read(fileTag[1], Length(cGlobalVarsFiles))
      else fileTag:='';
      if fileTag<>cGlobalVarsFiles then
         raise EGlobalVarError.Create('Invalid file tag');

      vGlobalVarsCS.BeginWrite;
      try
         vGlobalVars.Clean;
         vGlobalVarsNamesCache:='';

         reader.ReadListBegin;
         while not reader.EndOfList do begin
            gv:=TGlobalVar.Create;
            gv.ReadFromFiler(reader, name);
            vGlobalVars.AddObject(name, gv);
         end;
         reader.ReadListEnd;

         vGlobalVarsNamesCache:='';
      finally
         vGlobalVarsCS.EndWrite;
      end;
   finally
      reader.Free;
   end;
end;

// CollectGlobalVarsNames
//
procedure CollectGlobalVarsNames(const filter : String; dest : TStrings);
var
   list : TStringList;
   mask : TMask;
   item : String;
begin
   mask:=TMask.Create(filter);
   list:=TStringList.Create;
   try
      list.CommaText:=GlobalVarsNamesCommaText;
      if filter='*' then
         dest.AddStrings(list)
      else begin
         for item in list do begin
            if mask.Matches(item) then
               dest.Add(item);
         end;
      end;
   finally
      list.Free;
      mask.Free;
   end;
end;

// GlobalVarsNamesCommaText
//
function GlobalVarsNamesCommaText : UnicodeString;
var
   i : Integer;
   list : TStringList;
begin
   list:=TStringList.Create;
   vGlobalVarsCS.BeginWrite;
   try
      if vGlobalVarsNamesCache='' then begin
         for i:=0 to vGlobalVars.HighIndex do begin
            if vGlobalVars.BucketObject[i]<>nil then
               list.Add(vGlobalVars.BucketName[i]);
         end;
         vGlobalVarsNamesCache:=list.CommaText;
      end;
      Result:=vGlobalVarsNamesCache;
   finally
      vGlobalVarsCS.EndWrite;
      list.Free;
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

// WriteVariant
//
procedure WriteVariant(writer: TWriter; const value: Variant);

   procedure WriteValue(const value: TValueType);
   begin
      writer.Write(value, SizeOf(value));
   end;

begin
   case VarType(Value) of
      varInt64 :
         writer.WriteInteger(PVarData(@value).VInt64);
      varUString :
         {$ifdef FPC}
         writer.WriteString(UnicodeString(PVarData(@value).VString));
         {$else}
         writer.WriteString(UnicodeString(PVarData(@value).VUString));
         {$endif}
      varDouble :
         writer.WriteFloat(PVarData(@value).VDouble);
      varBoolean :
         writer.WriteBoolean(PVarData(@value).VBoolean);
      varEmpty :
         WriteValue(vaNil);
      varNull :
         WriteValue(vaNull);
      varByte, varSmallInt, varInteger :
         writer.WriteInteger(value);
      varString, varOleStr :
         writer.WriteString(value);
      varSingle :
         writer.WriteSingle(value);
      varCurrency :
         writer.WriteCurrency(value);
      varDate :
         writer.WriteDate(value);
   else
      try
         writer.WriteString(Value);
      except
         raise EWriteError.Create('Streaming not supported');
      end;
   end;
end;

// ReadVariant
//
function ReadVariant(reader: TReader): Variant;

  function ReadValue: TValueType;
  begin
    reader.Read(Result, SizeOf(Result));
  end;

const
   {$ifdef FPC}
   cValTtoVarT: array[TValueType] of Integer = (
      varNull, varError, varByte, varSmallInt, varInteger, varDouble,
      varString, varError, varBoolean, varBoolean, varError, varError, varString,
      varEmpty, varError, varSingle, varCurrency, varDate, varOleStr,
      varUInt64, varString, varDouble{$ifdef FPC}, varQWord{$endif}
    );
   {$else}
   cValTtoVarT: array[TValueType] of Integer = (
      varNull, varError, varByte, varSmallInt, varInteger, varDouble,
      varUString, varError, varBoolean, varBoolean, varError, varError, varUString,
      varEmpty, varError, varSingle, varCurrency, varDate, varOleStr,
      varUInt64, varUString, varDouble{$ifdef FPC}, varQWord{$endif}
    );
   {$endif}

var
  valType: TValueType;
begin
  valType := reader.NextValue;
  case valType of
    vaNil, vaNull:
      begin
        if ReadValue = vaNil then
          VarClearSafe(Result)
        else
          Result := NULL;
      end;
    vaInt8: TVarData(Result).VByte := Byte(reader.ReadInteger);
    vaInt16: TVarData(Result).VSmallint := Smallint(reader.ReadInteger);
    vaInt32: TVarData(Result).VInteger := reader.ReadInteger;
    vaInt64: TVarData(Result).VInt64 := reader.ReadInt64;
    vaExtended: TVarData(Result).VDouble := reader.ReadFloat;
    vaSingle: TVarData(Result).VSingle := reader.ReadSingle;
    vaCurrency: TVarData(Result).VCurrency := reader.ReadCurrency;
    vaDate: TVarData(Result).VDate := reader.ReadDate;
    vaString, vaLString, vaUTF8String:
       Result := UnicodeString(reader.ReadString);
    vaWString: Result := reader.ReadString;
    vaFalse, vaTrue:
       TVarData(Result).VBoolean := (reader.ReadValue = vaTrue);
  else
    raise EReadError.Create('Invalid variant stream');
  end;
  TVarData(Result).VType := cValTtoVarT[ValType];
end;

{ TGlobalVar }

procedure TGlobalVar.WriteToFiler(writer: TWriter; const Name : UnicodeString);
begin
   writer.WriteString(Name);
   dwsGlobalVarsFunctions.WriteVariant(writer, Value);
end;

procedure TGlobalVar.ReadFromFiler(reader: TReader; var Name : UnicodeString);
begin
   Name:=reader.ReadString;
   Value:=dwsGlobalVarsFunctions.ReadVariant(reader);
end;

{ TReadGlobalVarFunc }

procedure TReadGlobalVarFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   if not TryReadGlobalVar(args.AsString[0], Result) then
      VarClearSafe(Result);
end;

{ TReadGlobalVarDefFunc }

procedure TReadGlobalVarDefFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   if not TryReadGlobalVar(args.AsString[0], Result) then
      args.ExprBase[1].EvalAsVariant(args.Exec, Result);
end;

{ TTryReadGlobalVarFunc }

function TTryReadGlobalVarFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   v : Variant;
begin
   Result:=TryReadGlobalVar(args.AsString[0], v);
   if Result then
      args.ExprBase[1].AssignValue(args.Exec, v);
end;

{ TWriteGlobalVarFunc }

function TWriteGlobalVarFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   buf : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, buf);
   Result:=WriteGlobalVar(args.AsString[0], buf, 0);
end;

{ TWriteGlobalVarExpireFunc }

function TWriteGlobalVarExpireFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   buf : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, buf);
   Result:=WriteGlobalVar(args.AsString[0], buf, args.AsFloat[2]);
end;

// ------------------
// ------------------ TIncrementGlobalVarFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TIncrementGlobalVarFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=IncrementGlobalVar(args.AsString[0], args.AsInteger[1]);
end;

// ------------------
// ------------------ TCompareExchangeGlobalVarFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TCompareExchangeGlobalVarFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   value, comparand : Variant;
begin
   args.ExprBase[1].EvalAsVariant(args.Exec, value);
   args.ExprBase[2].EvalAsVariant(args.Exec, comparand);
   result:=CompareExchangeGlobalVar(args.AsString[0], value, comparand);
end;

{ TDeleteGlobalVarFunc }

function TDeleteGlobalVarFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=DeleteGlobalVar(args.AsString[0]);
end;

{ TCleanupGlobalVarsFunc }

procedure TCleanupGlobalVarsFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   CleanupGlobalVars(args.AsString[0]);
end;

// ------------------
// ------------------ TGlobalVarsNamesFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TGlobalVarsNamesFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   sl : TStringList;
   newArray : TScriptDynamicArray;
   i : Integer;
begin
   sl:=TStringList.Create;
   try
      CollectGlobalVarsNames(args.AsString[0], sl);
      newArray:=TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).Prog.SystemTable.SymbolTable.TypString);
      Result:=IScriptDynArray(newArray);
      newArray.ArrayLength:=sl.Count;
      for i:=0 to newArray.ArrayLength-1 do
         newArray.AsString[i]:=sl[i];
   finally
      sl.Free;
   end;
end;

{ TGlobalVarsNamesCommaText }

procedure TGlobalVarsNamesCommaText.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result:=GlobalVarsNamesCommaText;
end;

{ TSaveGlobalVarsToString }

// DoEvalAsString
//
procedure TSaveGlobalVarsToString.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result:=RawByteStringToScriptString(SaveGlobalVarsToString);
end;

{ TLoadGlobalVarsFromString }

procedure TLoadGlobalVarsFromString.Execute;
begin
   LoadGlobalVarsFromString(Info.ValueAsDataString['s']);
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vGlobalVarsCS:=TMultiReadSingleWrite.Create;
   vGlobalVars:=TNameGlobalVarHash.Create;
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
   RegisterInternalFunction(TGlobalVarsNamesFunc, 'GlobalVarsNames', ['filter', SYS_STRING], 'array of string');
   RegisterInternalStringFunction(TSaveGlobalVarsToString, 'SaveGlobalVarsToString', []);
   RegisterInternalProcedure(TLoadGlobalVarsFromString, 'LoadGlobalVarsFromString', ['s', SYS_STRING]);

   RegisterInternalIntFunction(TGlobalQueuePushFunc, 'GlobalQueuePush', ['n', SYS_STRING, 'v', SYS_VARIANT]);
   RegisterInternalIntFunction(TGlobalQueueInsertFunc, 'GlobalQueueInsert', ['n', SYS_STRING, 'v', SYS_VARIANT]);
   RegisterInternalBoolFunction(TGlobalQueuePullFunc, 'GlobalQueuePull', ['n', SYS_STRING, '@v', SYS_VARIANT]);
   RegisterInternalBoolFunction(TGlobalQueuePopFunc, 'GlobalQueuePop', ['n', SYS_STRING, '@v', SYS_VARIANT]);
   RegisterInternalIntFunction(TGlobalQueueLengthFunc, 'GlobalQueueLength', ['n', SYS_STRING]);
   RegisterInternalProcedure(TCleanupGlobalQueuesFunc, 'CleanupGlobalQueues', ['filter=*', SYS_STRING]);

finalization

   CleanupGlobalVars;
   vGlobalVarsCS.Free;
   vGlobalVarsCS:=nil;
   vGlobalVars.Clean;
   vGlobalVars.Free;
   vGlobalVars:=nil;

   CleanupGlobalQueues;
   vGlobalQueuesCS.Free;
   vGlobalQueuesCS:=nil;
   vGlobalQueues.Clean;
   vGlobalQueues.Free;
   vGlobalQueues:=nil;
  
end.
