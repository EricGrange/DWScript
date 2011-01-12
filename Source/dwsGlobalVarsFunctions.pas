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
   code only as of now, mainly for security reasons) to a file, string or stream.<br>
   Be aware DWS will require special care to run in a multi-threaded
   environment.
}
{$I dws.inc}
unit dwsGlobalVarsFunctions;

interface

uses
  Variants, Windows, Classes, SysUtils, dwsFunctions, dwsExprs, dwsSymbols, dwsUtils;

type

  TReadGlobalVarFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TReadGlobalVarDefFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TWriteGlobalVarFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TDeleteGlobalVarFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TCleanupGlobalVarsFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TGlobalVarsNamesCommaText = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
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
function WriteGlobalVar(const aName: String; const aValue: Variant) : Boolean;
{: Directly read a global var.<p> }
function ReadGlobalVar(const aName: String): Variant;
{: Directly read a global var, using a default value if variable does not exists.<p> }
function ReadGlobalVarDef(const aName: String; const aDefault: Variant): Variant;
{: Delete specified global var if it exists. }
function DeleteGlobalVar(const aName : String) : Boolean;
{: Resets all global vars.<p> }
procedure CleanupGlobalVars;

{: Save current global vars and their values to a string. }
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

{: CommaText of the names of all global vars. }
function GlobalVarsNamesCommaText : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vGlobalVarsCS : TRTLCriticalSection;
   vGlobalVars : TStringList;
   vGlobalVarsNamesCache : String;

const // type constants to make sure strings get reused by the compiler
   cFloat = 'Float';
   cInteger = 'Integer';
   cString = 'String';
   cBoolean = 'Boolean';
   cVariant = 'Variant';

   cGlobalVarsFiles : AnsiString = 'GBF 2.0';

type

   TGlobalVar = class(TObject)
      Value: Variant;

      procedure WriteToFiler(writer: TWriter; const Name : String);
      procedure ReadFromFiler(reader: TReader; var Name : String);
   end;

// WriteGlobalVar
//
function WriteGlobalVar(const aName : string; const aValue : Variant) : Boolean;
var
   gv : TGlobalVar;
   i : Integer;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      i:=vGlobalVars.IndexOf(aName);
      if i<0 then begin
         gv:=TGlobalVar.Create;
         vGlobalVars.AddObject(aName, gv);
         gv.Value:=aValue;
         vGlobalVarsNamesCache:='';
         Result:=True;
      end else begin
         gv:=TGlobalVar(vGlobalVars.Objects[i]);
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
function ReadGlobalVar(const aName : String) : Variant;
begin
   // Result (empty) is our default value when calling...
   Result:=ReadGlobalVarDef(aName, Result);
end;

// ReadGlobalVarDef
//
function ReadGlobalVarDef(const aName : String; const aDefault : Variant) : Variant;
var
   i : Integer;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      i:=vGlobalVars.IndexOf(aName);
      if i<0 then
         Result:=aDefault
      else Result:=TGlobalVar(vGlobalVars.Objects[i]).Value;
   finally
      LeaveCriticalSection(vGlobalVarsCS);
   end;
end;

// DeleteGlobalVar
//
function DeleteGlobalVar(const aName : String) : Boolean;
var
   i : Integer;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      i:=vGlobalVars.IndexOf(aName);
      Result:=(i>=0);
      if Result then begin
         vGlobalVars.Objects[i].Free;
         vGlobalVars.Delete(i);
         vGlobalVarsNamesCache:='';
      end;
   finally
      LeaveCriticalSection(vGlobalVarsCS);
   end;
end;

// CleanupGlobalVars
//
procedure CleanupGlobalVars;
var
  i: Integer;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      for i:=0 to vGlobalVars.Count-1 do
         vGlobalVars.Objects[i].Free;
      vGlobalVars.Clear;
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
   ms := TMemoryStream.Create;
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
   i : Integer;
   writer : TWriter;
   gv : TGlobalVar;
begin
   writer:=TWriter.Create(destStream, 16384);
   try
      writer.Write(cGlobalVarsFiles[1], Length(cGlobalVarsFiles));
      writer.WriteListBegin;

      EnterCriticalSection(vGlobalVarsCS);
      try
         for i:=0 to vGlobalVars.Count-1 do begin
            gv:=TGlobalVar(vGlobalVars.Objects[i]);
            gv.WriteToFiler(writer, vGlobalVars[i]);
         end;
      finally
         LeaveCriticalSection(vGlobalVarsCS);
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
   name : string;
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
         vGlobalVars.Sorted:=False;
         while not reader.EndOfList do begin
            gv:=TGlobalVar.Create;
            gv.ReadFromFiler(reader, name);
            vGlobalVars.AddObject(name, gv);
         end;
         vGlobalVars.Sorted:=True;
         reader.ReadListEnd;

      finally
         LeaveCriticalSection(vGlobalVarsCS);
      end;
   finally
      reader.Free;
   end;
end;

// GlobalVarsNamesCommaText
//
function GlobalVarsNamesCommaText : String;
begin
   EnterCriticalSection(vGlobalVarsCS);
   try
      if vGlobalVarsNamesCache='' then
         vGlobalVarsNamesCache:=vGlobalVars.CommaText;
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
         writer.WriteInteger(PVarData(@value).VUInt64);
      varUString :
         writer.WriteString(String(PVarData(@value).VUString));
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

   cValTtoVarT: array[TValueType] of Integer = (
      varNull, varError, varByte, varSmallInt, varInteger, varDouble,
      varUString, varError, varBoolean, varBoolean, varError, varError, varUString,
      varEmpty, varError, varSingle, varCurrency, varDate, varOleStr,
      varUInt64, varUString, varDouble
    );

var
  valType: TValueType;
begin
  valType := reader.NextValue;
  with reader do
    case valType of
      vaNil, vaNull:
        begin
          if ReadValue = vaNil then
            VarClear(Result)
          else
            Result := NULL;
        end;
      vaInt8: TVarData(Result).VByte := Byte(ReadInteger);
      vaInt16: TVarData(Result).VSmallint := Smallint(ReadInteger);
      vaInt32: TVarData(Result).VInteger := ReadInteger;
      vaInt64: TVarData(Result).VUInt64 := ReadInt64;
      vaExtended: TVarData(Result).VDouble := ReadFloat;
      vaSingle: TVarData(Result).VSingle := ReadSingle;
      vaCurrency: TVarData(Result).VCurrency := ReadCurrency;
      vaDate: TVarData(Result).VDate := ReadDate;
      vaString, vaLString, vaUTF8String:
         Result := String(ReadString);
      vaWString: Result := ReadWideString;
      vaFalse, vaTrue:
         TVarData(Result).VBoolean := (ReadValue = vaTrue);
    else
      raise EReadError.Create('Invalid variant stream');
    end;
  TVarData(Result).VType := cValTtoVarT[ValType];
end;

{ TGlobalVar }

procedure TGlobalVar.WriteToFiler(writer: TWriter; const Name : String);
begin
   writer.WriteString(Name);
   dwsGlobalVarsFunctions.WriteVariant(writer, Value);
end;

procedure TGlobalVar.ReadFromFiler(reader: TReader; var Name : String);
begin
   Name:=reader.ReadString;
   Value:=dwsGlobalVarsFunctions.ReadVariant(reader);
end;

{ TReadGlobalVarFunc }

procedure TReadGlobalVarFunc.Execute;
begin
  Info.ResultAsVariant := ReadGlobalVar(Info.ValueAsString['n']);
end;

{ TReadGlobalVarDefFunc }

procedure TReadGlobalVarDefFunc.Execute;
begin
  Info.ResultAsVariant := ReadGlobalVarDef(Info.ValueAsString['n'], Info.ValueAsVariant['d']);
end;

{ TWriteGlobalVarFunc }

procedure TWriteGlobalVarFunc.Execute;
begin
  Info.ResultAsBoolean:=WriteGlobalVar(Info.ValueAsString['n'], Info.ValueAsVariant['v']);
end;

{ TDeleteGlobalVarFunc }

procedure TDeleteGlobalVarFunc.Execute;
begin
  Info.ResultAsBoolean:=DeleteGlobalVar(Info.ValueAsString['n']);
end;

{ TCleanupGlobalVarsFunc }

procedure TCleanupGlobalVarsFunc.Execute;
begin
  CleanupGlobalVars;
end;

{ TGlobalVarsNamesCommaText }

procedure TGlobalVarsNamesCommaText.Execute;
begin
   Info.ResultAsString:=GlobalVarsNamesCommaText;
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
   vGlobalVars:=TStringList.Create;
   vGlobalVars.CaseSensitive:=True;
   vGlobalVars.Sorted:=True;

   RegisterInternalFunction(TReadGlobalVarFunc, 'ReadGlobalVar', ['n', cString], cVariant);
   RegisterInternalFunction(TReadGlobalVarDefFunc, 'ReadGlobalVarDef', ['n', cString, 'd', cVariant], cVariant);
   RegisterInternalFunction(TWriteGlobalVarFunc, 'WriteGlobalVar', ['n', cString, 'v', cVariant], cBoolean);
   RegisterInternalFunction(TDeleteGlobalVarFunc, 'DeleteGlobalVar', ['n', cString], cBoolean);
   RegisterInternalFunction(TCleanupGlobalVarsFunc, 'CleanupGlobalVars', [], '');
   RegisterInternalFunction(TGlobalVarsNamesCommaText, 'GlobalVarsNamesCommaText', [], cString);
   RegisterInternalFunction(TSaveGlobalVarsToString, 'SaveGlobalVarsToString', [], cString);
   RegisterInternalFunction(TLoadGlobalVarsFromString, 'LoadGlobalVarsFromString', ['s', cString], '');

finalization

   CleanupGlobalVars;
   DeleteCriticalSection(vGlobalVarsCS);
   FreeAndNil(vGlobalVars);
  
end.
