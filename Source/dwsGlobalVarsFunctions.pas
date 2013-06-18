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
   Variants, Windows, Classes, SysUtils,
   dwsUtils, dwsStrings, dwsExprList,
   dwsFunctions, dwsExprs, dwsSymbols, dwsMagicExprs;

type

   TReadGlobalVarFunc = class(TInternalMagicVariantFunction)
      function DoEvalAsVariant(const args : TExprBaseList) : Variant; override;
   end;

   TReadGlobalVarDefFunc = class(TInternalMagicVariantFunction)
      function DoEvalAsVariant(const args : TExprBaseList) : Variant; override;
   end;

   TWriteGlobalVarFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseList) : Boolean; override;
   end;

   TDeleteGlobalVarFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseList) : Boolean; override;
   end;

   TCleanupGlobalVarsFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseList); override;
   end;

   TGlobalVarsNamesCommaText = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseList; var Result : UnicodeString); override;
   end;

   TSaveGlobalVarsToString = class(TInternalFunction)
      procedure Execute(info : TProgramInfo); override;
   end;

   TLoadGlobalVarsFromString = class(TInternalFunction)
      procedure Execute(info : TProgramInfo); override;
   end;

   TdwsGlobalVarsFunctions = class(TComponent)
   end;

   EGlobalVarError = class (Exception)
   end;

{: Directly write a global var.<p> }
function WriteGlobalVar(const aName: UnicodeString; const aValue: Variant) : Boolean;
{: Directly read a global var.<p> }
function ReadGlobalVar(const aName: UnicodeString): Variant;
{: Directly read a global var, using a default value if variable does not exists.<p> }
function ReadGlobalVarDef(const aName: UnicodeString; const aDefault: Variant): Variant;
{: Delete specified global var if it exists. }
function DeleteGlobalVar(const aName : UnicodeString) : Boolean;
{: Resets all global vars.<p> }
procedure CleanupGlobalVars;

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
function GlobalVarsNamesCommaText : UnicodeString;

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

         procedure WriteToFiler(writer: TWriter; const Name : UnicodeString);
         procedure ReadFromFiler(reader: TReader; var Name : UnicodeString);
   end;

   TNameGlobalVarHash = TSimpleNameObjectHash<TGlobalVar>;

var
   vGlobalVarsCS : TRTLCriticalSection;
   vGlobalVars : TNameGlobalVarHash;
   vGlobalVarsNamesCache : UnicodeString;

const
   cGlobalVarsFiles : AnsiString = 'GBF 2.0';

// WriteGlobalVar
//
function WriteGlobalVar(const aName : UnicodeString; const aValue : Variant) : Boolean;
var
   gv : TGlobalVar;
begin
   EnterCriticalSection(vGlobalVarsCS);
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
   finally
      LeaveCriticalSection(vGlobalVarsCS);
   end;
end;

// ReadGlobalVar
//
function ReadGlobalVar(const aName : UnicodeString) : Variant;
begin
   // Result (empty) is our default value when calling...
   Result:=ReadGlobalVarDef(aName, Result);
end;

// ReadGlobalVarDef
//
function ReadGlobalVarDef(const aName : UnicodeString; const aDefault : Variant) : Variant;
var
   gv : TGlobalVar;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      gv:=vGlobalVars.Objects[aName];
      if gv<>nil then
         Result:=gv.Value
      else Result:=aDefault;
   finally
      LeaveCriticalSection(vGlobalVarsCS);
   end;
end;

// DeleteGlobalVar
//
function DeleteGlobalVar(const aName : UnicodeString) : Boolean;
var
   gv : TGlobalVar;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      gv:=vGlobalVars.Objects[aName];
      if gv<>nil then begin
         gv.Free;
         vGlobalVars.Objects[aName]:=nil;
         vGlobalVarsNamesCache:='';
         Result:=True;
      end else Result:=False;
   finally
      LeaveCriticalSection(vGlobalVarsCS);
   end;
end;

// CleanupGlobalVars
//
procedure CleanupGlobalVars;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      vGlobalVars.Clean;
      vGlobalVarsNamesCache:='';
   finally
      LeaveCriticalSection(vGlobalVarsCS);
   end;
end;

// SaveGlobalVarsToString
//
function SaveGlobalVarsToString : RawByteString;
var
   ms : TMemoryStream;
begin
   ms:=TMemoryStream.Create;
   try
      SaveGlobalVarsToStream(ms);
      SetLength(Result, ms.Position);
      if Result<>'' then
         Move(ms.Memory^, Result[1], Length(Result));
   finally
      ms.Free;
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
   list : TStringList;
begin
   list:=TStringList.Create;
   writer:=TWriter.Create(destStream, 16384);
   try
      writer.Write(cGlobalVarsFiles[1], Length(cGlobalVarsFiles));
      writer.WriteListBegin;

      EnterCriticalSection(vGlobalVarsCS);
      try
         vGlobalVars.Enumerate(list);
         for i:=0 to list.Count-1 do begin
            gv:=TGlobalVar(list.Objects[i]);
            if gv<>nil then
               gv.WriteToFiler(writer, list[i]);
         end;
      finally
         LeaveCriticalSection(vGlobalVarsCS);
      end;

      writer.WriteListEnd;
   finally
      writer.Free;
      list.Free;
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

      EnterCriticalSection(vGlobalVarsCS);
      try
         CleanupGlobalVars;

         reader.ReadListBegin;
         while not reader.EndOfList do begin
            gv:=TGlobalVar.Create;
            gv.ReadFromFiler(reader, name);
            vGlobalVars.AddObject(name, gv);
         end;
         reader.ReadListEnd;

         vGlobalVarsNamesCache:='';
      finally
         LeaveCriticalSection(vGlobalVarsCS);
      end;
   finally
      reader.Free;
   end;
end;

// GlobalVarsNamesCommaText
//
function GlobalVarsNamesCommaText : UnicodeString;
var
   i : Integer;
   list : TStringList;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      if vGlobalVarsNamesCache='' then begin
         list:=TStringList.Create;
         try
            vGlobalVars.Enumerate(list);
            for i:=list.Count-1 downto 0 do begin
               if list.Objects[i]=nil then
                  list.Delete(i);
            end;
            vGlobalVarsNamesCache:=list.CommaText;
         finally
            list.Free;
         end;
      end;
      Result:=vGlobalVarsNamesCache;
   finally
      LeaveCriticalSection(vGlobalVarsCS);
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
          VarClear(Result)
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

function TReadGlobalVarFunc.DoEvalAsVariant(const args : TExprBaseList) : Variant;
begin
   Result:=ReadGlobalVar(args.AsString[0]);
end;

{ TReadGlobalVarDefFunc }

function TReadGlobalVarDefFunc.DoEvalAsVariant(const args : TExprBaseList) : Variant;
begin
   Result:=ReadGlobalVarDef(args.AsString[0], args.ExprBase[1].Eval(args.Exec));
end;

{ TWriteGlobalVarFunc }

function TWriteGlobalVarFunc.DoEvalAsBoolean(const args : TExprBaseList) : Boolean;
begin
   Result:=WriteGlobalVar(args.AsString[0], args.ExprBase[1].Eval(args.Exec));
end;

{ TDeleteGlobalVarFunc }

function TDeleteGlobalVarFunc.DoEvalAsBoolean(const args : TExprBaseList) : Boolean;
begin
   Result:=DeleteGlobalVar(args.AsString[0]);
end;

{ TCleanupGlobalVarsFunc }

procedure TCleanupGlobalVarsFunc.DoEvalProc(const args : TExprBaseList);
begin
   CleanupGlobalVars;
end;

{ TGlobalVarsNamesCommaText }

procedure TGlobalVarsNamesCommaText.DoEvalAsString(const args : TExprBaseList; var Result : UnicodeString);
begin
   Result:=GlobalVarsNamesCommaText;
end;

{ TSaveGlobalVarsToString }

procedure TSaveGlobalVarsToString.Execute;
begin
   Info.ResultAsDataString:=SaveGlobalVarsToString;
end;

{ TLoadGlobalVarsFromString }

procedure TLoadGlobalVarsFromString.Execute;
begin
   LoadGlobalVarsFromString(Info.ValueAsDataString['s']);
end;

initialization

   InitializeCriticalSection(vGlobalVarsCS);
   vGlobalVars:=TNameGlobalVarHash.Create;

   RegisterInternalFunction(TReadGlobalVarFunc, 'ReadGlobalVar', ['n', SYS_STRING], SYS_VARIANT);
   RegisterInternalFunction(TReadGlobalVarDefFunc, 'ReadGlobalVarDef', ['n', SYS_STRING, 'd', SYS_VARIANT], SYS_VARIANT);
   RegisterInternalFunction(TWriteGlobalVarFunc, 'WriteGlobalVar', ['n', SYS_STRING, 'v', SYS_VARIANT], SYS_BOOLEAN);
   RegisterInternalFunction(TDeleteGlobalVarFunc, 'DeleteGlobalVar', ['n', SYS_STRING], SYS_BOOLEAN);
   RegisterInternalFunction(TCleanupGlobalVarsFunc, 'CleanupGlobalVars', [], '');
   RegisterInternalFunction(TGlobalVarsNamesCommaText, 'GlobalVarsNamesCommaText', [], SYS_STRING);
   RegisterInternalFunction(TSaveGlobalVarsToString, 'SaveGlobalVarsToString', [], SYS_STRING);
   RegisterInternalFunction(TLoadGlobalVarsFromString, 'LoadGlobalVarsFromString', ['s', SYS_STRING], '');

finalization

   CleanupGlobalVars;
   DeleteCriticalSection(vGlobalVarsCS);
   vGlobalVars.Clean;
   vGlobalVars.Free;
   vGlobalVars:=nil;
  
end.
