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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsJSON;

interface

uses
   Classes, SysUtils, Variants, Math,
   dwsUtils, dwsXPlatform;

type

   TdwsJSONArray = class;
   TdwsJSONObject = class;
   TdwsJSONImmediate = class;

   TdwsJSONWriterState = (wsNone, wsObject, wsObjectName, wsObjectValue, wsArray, wsArrayValue, wsDone);

   // TdwsJSONWriter
   //
   TdwsJSONWriter = class
      private
         FStream : TWriteOnlyBlockStream;
         FOwnsStream : Boolean;
         FStateStack : TTightStack;
         FState : TdwsJSONWriterState;

      protected
         procedure BeforeWriteImmediate; virtual;
         procedure AfterWriteImmediate;

      public
         constructor Create(aStream : TWriteOnlyBlockStream);
         destructor Destroy; override;

         procedure BeginObject; virtual;
         procedure EndObject; virtual;

         procedure BeginArray; virtual;
         procedure EndArray; virtual;

         procedure WriteName(const aName : String); virtual;
         procedure WriteString(const str : String);
         procedure WriteNumber(const n : Double);
         procedure WriteInteger(const n : Int64);
         procedure WriteBoolean(b : Boolean);
         procedure WriteNull;

         // ISO 8601 Date Time
         procedure WriteDate(dt : TDateTime); overload;

         procedure WriteStrings(const str : TStrings); overload;
         procedure WriteStrings(const str : array of String); overload;

         function ToString : String; override;

         property Stream : TWriteOnlyBlockStream read FStream write FStream;
   end;

   // TdwsJSONBeautifiedWriter
   //
   TdwsJSONBeautifiedWriter = class (TdwsJSONWriter)
      private
         FTabs : Integer;
         FIndent : Integer;

         procedure WriteIndents;
         procedure EnterIndent;
         procedure LeaveIndent;

      protected
         procedure BeforeWriteImmediate; override;

      public
         constructor Create(aStream : TWriteOnlyBlockStream; initialTabs, indentTabs : Integer);

         procedure BeginObject; override;
         procedure EndObject; override;

         procedure BeginArray; override;
         procedure EndArray; override;

         procedure WriteName(const aName : String); override;
   end;

   TdwsJSONDuplicatesOptions = (jdoAccept, jdoOverwrite);

   // TdwsJSONParserState
   //
   // Internal utility parser for TdwsJSON
   TdwsJSONParserState = class
      private
         Str : UnicodeString;
         Ptr, ColStart : PWideChar;
         Line : Integer;
         TrailCharacter : WideChar;
         DuplicatesOption : TdwsJSONDuplicatesOptions;

      public
         constructor Create(const aStr : String);

         function Location : String;

         function NeedChar : WideChar; inline;
         function SkipBlanks(currentChar : WideChar) : WideChar; inline;

         function ParseJSONString(initialChar : WideChar) : UnicodeString;
         function ParseHugeJSONNumber(initialChars : PChar; initialCharCount : Integer) : Double;
         function ParseJSONNumber(initialChar : WideChar) : Double;
   end;

   TdwsJSONValueType = (jvtUndefined, jvtNull, jvtObject, jvtArray, jvtString, jvtNumber, jvtBoolean);

   // TdwsJSONValue
   //
   TdwsJSONValue = class (TRefCountedObject)
      private
         FOwner : TdwsJSONValue;

      protected
         FValueType : TdwsJSONValueType;

         procedure DetachChild(child : TdwsJSONValue); virtual;

         function GetValueType : TdwsJSONValueType;
         function GetName(index : Integer) : String;
         function DoGetName(index : Integer) : String; virtual;
         function GetElement(index : Integer) : TdwsJSONValue; inline;
         function DoGetElement(index : Integer) : TdwsJSONValue; virtual;
         function GetItem(const name : String) : TdwsJSONValue; inline;
         function DoGetItem(const name : String) : TdwsJSONValue; virtual;
         procedure SetItem(const name : String; const value : TdwsJSONValue); inline;
         procedure DoSetItem(const name : String; const value : TdwsJSONValue); virtual; abstract;
         function DoElementCount : Integer; virtual;
         function GetValue(const index : Variant) : TdwsJSONValue;

         function GetAsString : String; inline;
         procedure SetAsString(const val : String); inline;
         function GetIsNull : Boolean; inline;
         procedure SetIsNull(const val : Boolean);
         function GetIsDefined : Boolean; inline;
         function GetAsBoolean : Boolean; inline;
         procedure SetAsBoolean(const val : Boolean); inline;
         function GetAsNumber : Double;
         procedure SetAsNumber(const val : Double); inline;
         function GetIsNaN : Boolean;
         function GetAsInteger : Int64; inline;
         procedure SetAsInteger(const val : Int64); inline;

         procedure DoParse(initialChar : WideChar; parserState : TdwsJSONParserState); virtual; abstract;

         function DoClone : TdwsJSONValue; virtual; abstract;
         procedure DoExtend(other : TdwsJSONValue); virtual; abstract;

         class procedure RaiseJSONException(const msg : String); static;
         class procedure RaiseJSONParseError(const msg : String; c : WideChar = #0); static;

         class function Parse(parserState : TdwsJSONParserState) : TdwsJSONValue; static;

      public
         destructor Destroy; override;

         class function ParseString(const json : String;
                                    duplicatesOption : TdwsJSONDuplicatesOptions = jdoOverwrite) : TdwsJSONValue; static;
         class function ParseFile(const fileName : String) : TdwsJSONValue; static;

         function Clone : TdwsJSONValue;
         procedure Extend(other : TdwsJSONValue);

         procedure WriteTo(writer : TdwsJSONWriter); virtual; abstract;
         function ToString : String; reintroduce;
         function ToBeautifiedString(initialTabs : Integer = 0; indentTabs : Integer = 1) : String;
         procedure Detach;

         property Owner : TdwsJSONValue read FOwner;
         property ValueType : TdwsJSONValueType read GetValueType;
         property Items[const name : String] : TdwsJSONValue read GetItem write SetItem;
         property Names[index : Integer] : String read GetName;
         property Elements[index : Integer] : TdwsJSONValue read GetElement;
         function ElementCount : Integer;
         property Values[const index : Variant] : TdwsJSONValue read GetValue; default;

         function IsImmediateValue : Boolean; inline;
         function Value : TdwsJSONImmediate; inline;

         property AsString : String read GetAsString write SetAsString;
         property IsNull : Boolean read GetIsNull write SetIsNull;
         property IsDefined : Boolean read GetIsDefined;
         property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
         property AsNumber : Double read GetAsNumber write SetAsNumber;
         property IsNaN : Boolean read GetIsNaN;
         property AsInteger : Int64 read GetAsInteger write SetAsInteger;

         const ValueTypeStrings : array [TdwsJSONValueType] of String = (
            'Undefined', 'Null', 'Object', 'Array', 'String', 'Number', 'Boolean'
            );
   end;

   TdwsJSONPair = record
      Name : String;
      Hash : Cardinal;
      Value : TdwsJSONValue;
   end;
   TdwsJSONPairArray = array [0..MaxInt shr 5] of TdwsJSONPair;
   PdwsJSONPairArray = ^TdwsJSONPairArray;

   // TdwsJSONObject
   //
   TdwsJSONObject = class sealed (TdwsJSONValue)
      private
         FItems : PdwsJSONPairArray;
         FCapacity : Integer;
         FCount : Integer;

      protected
         procedure Grow;
         procedure SetCapacity(newCapacity : Integer);
         function IndexOfName(const name : String) : Integer;
         function IndexOfValue(const aValue : TdwsJSONValue) : Integer;
         procedure DetachChild(child : TdwsJSONValue); override;
         procedure DetachIndex(i : Integer);

         function DoGetName(index : Integer) : String; override;
         function DoGetElement(index : Integer) : TdwsJSONValue; override;
         function DoGetItem(const name : String) : TdwsJSONValue; override;
         procedure DoSetItem(const name : String; const value : TdwsJSONValue); override;
         function DoElementCount : Integer; override;

         procedure DoParse(initialChar : WideChar; parserState : TdwsJSONParserState); override;

         function DoClone : TdwsJSONValue; override;
         procedure DoExtend(other : TdwsJSONValue); override;

      public
         constructor Create;
         destructor Destroy; override;

         function Clone : TdwsJSONObject;

         procedure Clear;

         procedure Add(const aName : String; aValue : TdwsJSONValue);

         function AddObject(const name : String) : TdwsJSONObject;

         function AddArray(const name : String) : TdwsJSONArray;

         function AddValue(const name : String) : TdwsJSONImmediate; overload;
         function AddValue(const name, value : String) : TdwsJSONImmediate; overload;
         function AddValue(const name : String; const value : Double) : TdwsJSONImmediate; overload;
         function AddValue(const name : String; const value : Boolean) : TdwsJSONImmediate; overload;

         procedure WriteTo(writer : TdwsJSONWriter); override;

         procedure MergeDuplicates;
   end;

   PdwsJSONValueArray = ^TdwsJSONValueArray;
   TdwsJSONValueArray = array [0..MaxInt shr 4] of TdwsJSONValue;

   // TdwsJSONArray
   //
   TdwsJSONArray = class (TdwsJSONValue)
      private
         FElements : PdwsJSONValueArray;
         FCapacity : Integer;
         FCount : Integer;

      protected
         procedure Grow;
         procedure SetCapacity(newCapacity : Integer);
         procedure DetachChild(child : TdwsJSONValue); override;
         procedure DeleteIndex(idx : Integer);

         function DoGetName(index : Integer) : String; override;
         function DoGetElement(index : Integer) : TdwsJSONValue; override;
         function DoGetItem(const name : String) : TdwsJSONValue; override;
         procedure DoSetItem(const name : String; const value : TdwsJSONValue); override;
         function DoElementCount : Integer; override;

         procedure DoParse(initialChar : WideChar; parserState : TdwsJSONParserState); override;

         function DoClone : TdwsJSONValue; override;
         procedure DoExtend(other : TdwsJSONValue); override;

      public
         constructor Create;
         destructor Destroy; override;

         function Clone : TdwsJSONArray;

         procedure Clear;

         procedure Add(aValue : TdwsJSONValue);
         function AddObject : TdwsJSONObject;
         function AddArray : TdwsJSONArray;
         function AddValue : TdwsJSONImmediate;

         procedure WriteTo(writer : TdwsJSONWriter); override;
   end;

   // TdwsJSONImmediate
   //
   TdwsJSONImmediate = class (TdwsJSONValue)
      private
         FValue : Variant;

      protected
         procedure DoSetItem(const name : String; const value : TdwsJSONValue); override;

         function GetAsString : String; inline;
         procedure SetAsString(const val : String); inline;
         function GetIsNull : Boolean; inline;
         procedure SetIsNull(const val : Boolean);
         function GetAsBoolean : Boolean;
         procedure SetAsBoolean(const val : Boolean); inline;
         function GetAsNumber : Double;
         procedure SetAsNumber(const val : Double); inline;
         function GetAsInteger : Int64;
         procedure SetAsInteger(const val : Int64);

         procedure DoParse(initialChar : WideChar; parserState : TdwsJSONParserState); override;

         function DoClone : TdwsJSONValue; override;
         procedure DoExtend(other : TdwsJSONValue); override;

      public
         class function ParseString(const json : String) : TdwsJSONImmediate; static;
         class function FromVariant(const v : Variant) : TdwsJSONImmediate; static;

         function Clone : TdwsJSONImmediate;

         procedure WriteTo(writer : TdwsJSONWriter); override;

         property RawValue : Variant read FValue write FValue;

         property AsString : String read GetAsString write SetAsString;
         property IsNull : Boolean read GetIsNull write SetIsNull;
         property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
         property AsNumber : Double read GetAsNumber write SetAsNumber;
         property AsInteger : Int64 read GetAsInteger write SetAsInteger;
   end;

   EdwsJSONException = class (Exception);
   EdwsJSONParseError = class (EdwsJSONException);
   EdwsJSONWriterError = class (EdwsJSONException);

procedure WriteJavaScriptString(destStream : TWriteOnlyBlockStream; const str : String);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vJSONFormatSettings : TFormatSettings;

// Create
//
constructor TdwsJSONParserState.Create(const aStr : String);
begin
   {$ifdef FPC}
   Str:=UTF8Decode(aStr);
   {$else}
   Str:=aStr;
   {$endif}
   Ptr:=PWideChar(Str);
   ColStart:=Ptr;
end;

// NeedChar
//
function TdwsJSONParserState.NeedChar : WideChar;
var
   p : PWideChar;
begin
   p:=Ptr;
   Inc(Ptr);
   if p^=#10 then begin
      ColStart:=p;
      Inc(Line);
   end;
   Result:=p^;
end;

// Location
//
function TdwsJSONParserState.Location : String;
begin
   if (Line=0) then begin
      Result:=Format('line 1, col %d',
                     [(NativeInt(Ptr)-NativeInt(ColStart)) div SizeOf(Char)]);
   end else begin
      Result:=Format('line %d, col %d (offset %d)',
                     [Line+1,
                      (NativeInt(Ptr)-NativeInt(ColStart)) div SizeOf(Char),
                      (NativeInt(Ptr)-NativeInt(PChar(Str))) div SizeOf(Char)]);
   end;
end;

// SkipBlanks
//
function TdwsJSONParserState.SkipBlanks(currentChar : WideChar) : WideChar;
begin
   Result:=currentChar;
   repeat
      case Result of
         #9..#13, ' ' : ;
      else
         Break;
      end;
      Result:=NeedChar();
   until False;
end;

// ParseJSONString
//
function TdwsJSONParserState.ParseJSONString(initialChar : WideChar) : UnicodeString;
var
   c : WideChar;
   wobs : TWriteOnlyBlockStream;
   hexBuf, hexCount, n, nw : Integer;
   localBufferPtr, startPr : PWideChar;
   localBuffer : array [0..59] of WideChar; // range adjusted to have a stack space of 128 for the proc
begin
   startPr:=Ptr;
   wobs:=nil;
   try
      localBufferPtr:=@localBuffer[0];
      repeat
         c:=NeedChar();
         case c of
            #0..#31 :
               if c=#0 then begin
                  Ptr:=startPr;
                  TdwsJSONValue.RaiseJSONParseError('Unterminated string', c)
               end else TdwsJSONValue.RaiseJSONParseError('Invalid string character %s', c);
            '"' : Break;
            '\' : begin
               c:=NeedChar();
               case c of
                  '"', '\', '/' : localBufferPtr^:=c;
                  'n' : localBufferPtr^:=#10;
                  'r' : localBufferPtr^:=#13;
                  't' : localBufferPtr^:=#9;
                  'b' : localBufferPtr^:=#8;
                  'f' : localBufferPtr^:=#12;
                  'u' : begin
                     hexBuf:=0;
                     for hexCount:=1 to 4 do begin
                        c:=NeedChar();
                        case c of
                           '0'..'9' :
                              hexBuf:=(hexBuf shl 4)+Ord(c)-Ord('0');
                           'a'..'f' :
                              hexBuf:=(hexBuf shl 4)+Ord(c)-(Ord('a')-10);
                           'A'..'F' :
                              hexBuf:=(hexBuf shl 4)+Ord(c)-(Ord('A')-10);
                        else
                           TdwsJSONValue.RaiseJSONParseError('Invalid unicode hex character "%s"', c);
                        end;
                     end;
                     localBufferPtr^:=WideChar(hexBuf);
                  end;
               else
                  TdwsJSONValue.RaiseJSONParseError('Invalid character "%s" after escape', c);
               end;
            end;
         else
            localBufferPtr^:=c;
         end;
         if localBufferPtr=@localBuffer[High(localBuffer)] then begin
            if wobs=nil then
               wobs:=TWriteOnlyBlockStream.Create;
            wobs.Write(localBuffer[0], Length(localBuffer)*SizeOf(WideChar));
            localBufferPtr:=@localBuffer[0];
         end else Inc(localBufferPtr);
      until False;
      n:=(NativeInt(localBufferPtr)-NativeInt(@localBuffer[0])) shr (SizeOf(WideChar)-1);
      if wobs<>nil then begin
         nw:=(wobs.Size div SizeOf(WideChar));
         SetLength(Result, n+nw);
         localBufferPtr:=PWideChar(Pointer(Result));
         wobs.StoreData(localBufferPtr^);
         Move(localBuffer[0], localBufferPtr[nw], n*SizeOf(WideChar));
      end else begin
         if n>0 then begin
            SetLength(Result, n);
            localBufferPtr:=PWideChar(Pointer(Result));
            Move(localBuffer[0], localBufferPtr^, n*SizeOf(WideChar));
         end else Result:='';
      end;
   finally
      wobs.Free;
   end;
end;

// ParseJSONNumber
//
function TdwsJSONParserState.ParseHugeJSONNumber(initialChars : PChar; initialCharCount : Integer) : Double;
var
   buf : String;
   c : WideChar;
begin
   SetString(buf, initialChars, initialCharCount);
   repeat
      c:=NeedChar();
      case c of
         '0'..'9', '-', '+', 'e', 'E', '.' : buf:=buf+Char(c);
      else
         TrailCharacter:=c;
         Break;
      end;
   until False;
   Result:=StrToFloat(buf, vJSONFormatSettings);
end;

// ParseJSONNumber
//
function TdwsJSONParserState.ParseJSONNumber(initialChar : WideChar) : Double;
var
   bufPtr : PChar;
   c : WideChar;
   resultBuf : Extended;
   buf : array [0..40] of Char;
begin
   buf[0]:=initialChar;
   bufPtr:=@buf[1];
   repeat
      c:=NeedChar();
      case c of
         '0'..'9', '-', '+', 'e', 'E', '.' : begin
            bufPtr^:=c;
            Inc(bufPtr);
            if bufPtr=@buf[High(buf)] then
               Exit(ParseHugeJSONNumber(@buf[0], Length(buf)-1));
         end;
      else
         TrailCharacter:=c;
         Break;
      end;
   until False;
   bufPtr^:=#0;
   TryTextToFloat(PChar(@buf[0]), resultBuf, vJSONFormatSettings);
   Result:=resultBuf;
end;

// WriteJavaScriptString
//
procedure WriteJavaScriptString(destStream : TWriteOnlyBlockStream; const str : String);

   procedure WriteUTF16(destStream : TWriteOnlyBlockStream; c : Integer);
   const
      cIntToHex : array [0..15] of Char = (
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
   var
      hex : array [0..5] of Char;
   begin
      hex[0]:='\';
      hex[1]:='u';
      hex[2]:=cIntToHex[c shr 12];
      hex[3]:=cIntToHex[(c shr 8) and $F];
      hex[4]:=cIntToHex[(c shr 4) and $F];
      hex[5]:=cIntToHex[c and $F];
      destStream.Write(hex[0], 6*SizeOf(Char));
   end;

const
   cQUOTE : Char = '"';
var
   c : Char;
   p : PChar;
begin
   destStream.Write(cQUOTE, SizeOf(Char));
   p:=PChar(Pointer(str));
   if p<>nil then while True do begin
      c:=p^;
      case c of
         #0..#31 :
            case c of
               #0 : Break;
               #8 : destStream.WriteString('\b');
               #9 : destStream.WriteString('\t');
               #10 : destStream.WriteString('\n');
               #12 : destStream.WriteString('\f');
               #13 : destStream.WriteString('\r');
            else
               WriteUTF16(destStream, Ord(c));
            end;
         '"' :
            destStream.WriteString('\"');
         '\' :
            destStream.WriteString('\\');
         {$ifndef FPC}
         #255..#65535 :
            WriteUTF16(destStream, Ord(c));
         {$endif}
      else
         destStream.Write(p^, SizeOf(Char));
      end;
      Inc(p);
   end;
   destStream.Write(cQUOTE, SizeOf(Char));
end;

// ------------------
// ------------------ TdwsJSONValue ------------------
// ------------------

// Destroy
//
destructor TdwsJSONValue.Destroy;
begin
   if FOwner<>nil then
      Detach;
   inherited;
end;

// Parse
//
class function TdwsJSONValue.Parse(parserState : TdwsJSONParserState) : TdwsJSONValue;
var
   c : WideChar;
begin
   Result:=nil;
   repeat
      c:=parserState.NeedChar();
      case c of
         #0 : Break;
         #9..#13, ' ' : ;
         '{' : Result:=TdwsJSONObject.Create;
         '[' : Result:=TdwsJSONArray.Create;
         '0'..'9', '"', '-', 't', 'f', 'n' :
            Result:=TdwsJSONImmediate.Create;
         ']', '}' : begin
            // empty array or object
            parserState.TrailCharacter:=c;
            Exit(nil);
         end;
      else
         RaiseJSONParseError('Invalid value start character "%s"', c);
      end;
   until Result<>nil;
   if Result<>nil then begin
      try
         Result.DoParse(c, parserState);
      except
         Result.Free;
         raise;
      end;
   end;
end;

// ParseString
//
class function TdwsJSONValue.ParseString(const json : String;
                                         duplicatesOption : TdwsJSONDuplicatesOptions = jdoOverwrite) : TdwsJSONValue;
var
   parserState : TdwsJSONParserState;
begin
   Result:=nil;
   parserState:=TdwsJSONParserState.Create(json);
   try
      try
         parserState.DuplicatesOption:=duplicatesOption;
         Result:=TdwsJSONValue.Parse(parserState);
      except
         on e : EdwsJSONParseError do
            raise EdwsJSONParseError.CreateFmt('%s, at %s',
                                               [e.Message, parserState.Location]);
      else
         raise;
      end;
   finally
      parserState.Free;
   end;
end;

// ParseFile
//
class function TdwsJSONValue.ParseFile(const fileName : String) : TdwsJSONValue;
begin
   Result:=ParseString(LoadTextFromFile(fileName));
end;

// Clone
//
function TdwsJSONValue.Clone : TdwsJSONValue;
begin
   if Self<>nil then
      Result:=DoClone
   else Result:=nil;
end;

// Extend
//
procedure TdwsJSONValue.Extend(other : TdwsJSONValue);
begin
   if Self=nil then
      RaiseJSONException('Cannot extend undefined object')
   else if other<>nil then
      DoExtend(other);
end;

// ToString
//
function TdwsJSONValue.ToString : String;
var
   writer : TdwsJSONWriter;
begin
   if Self=nil then Exit('');
   writer:=TdwsJSONWriter.Create(nil);
   try
      WriteTo(writer);
      Result:=writer.Stream.ToString;
   finally
      writer.Free;
   end;
end;

// ToBeautifiedString
//
function TdwsJSONValue.ToBeautifiedString(initialTabs, indentTabs : Integer) : String;
var
   writer : TdwsJSONBeautifiedWriter;
begin
   if Self=nil then Exit('');
   writer:=TdwsJSONBeautifiedWriter.Create(nil, initialTabs, indentTabs);
   try
      WriteTo(writer);
      Result:=writer.Stream.ToString;
   finally
      writer.Free;
   end;
end;

// Detach
//
procedure TdwsJSONValue.Detach;
var
   oldOwner : TdwsJSONValue;
begin
   oldOwner:=FOwner;
   if oldOwner<>nil then begin
      FOwner:=nil;
      oldOwner.DetachChild(Self);
   end;
end;

// DoElementCount
//
function TdwsJSONValue.DoElementCount : Integer;
begin
   Result:=0;
end;

// ElementCount
//
function TdwsJSONValue.ElementCount : Integer;
begin
   if Assigned(Self) then
      Result:=DoElementCount
   else Result:=0;
end;

// IsImmediateValue
//
function TdwsJSONValue.IsImmediateValue : Boolean;
begin
   Result:=Assigned(Self) and not (FValueType in [jvtObject, jvtArray]);
end;

// Value
//
function TdwsJSONValue.Value : TdwsJSONImmediate;
begin
   if FValueType in [jvtObject, jvtArray] then
      RaiseJSONException('Not a value');
   Result:=TdwsJSONImmediate(Self);
end;

// GetValue
//
function TdwsJSONValue.GetValue(const index : Variant) : TdwsJSONValue;
begin
   if Assigned(Self) then begin
      if VarIsOrdinal(index) then
         Result:=Elements[index]
      else Result:=Items[index];
   end else Result:=nil;
end;

// GetAsString
//
function TdwsJSONValue.GetAsString : String;
begin
   if Assigned(Self) then
      Result:=Value.AsString
   else Result:='undefined';
end;

// SetAsString
//
procedure TdwsJSONValue.SetAsString(const val : String);
begin
   Value.AsString:=val;
end;

// GetIsNull
//
function TdwsJSONValue.GetIsNull : Boolean;
begin
   if Assigned(Self) then
      Result:=(ValueType=jvtNull)
   else Result:=False;
end;

// SetIsNull
//
procedure TdwsJSONValue.SetIsNull(const val : Boolean);
begin
   Value.IsNull:=val;
end;

// GetIsDefined
//
function TdwsJSONValue.GetIsDefined : Boolean;
begin
   Result:=Assigned(Self) and (FValueType<>jvtUndefined);
end;

// GetAsBoolean
//
function TdwsJSONValue.GetAsBoolean : Boolean;
begin
   if Assigned(Self) then
      Result:=Value.AsBoolean
   else Result:=False;
end;

// SetAsBoolean
//
procedure TdwsJSONValue.SetAsBoolean(const val : Boolean);
begin
   Value.AsBoolean:=val;
end;

// GetAsNumber
//
function TdwsJSONValue.GetAsNumber : Double;
begin
   if Assigned(Self) then
      Result:=Value.AsNumber
   else Result:=NaN;
end;

// SetAsNumber
//
procedure TdwsJSONValue.SetAsNumber(const val : Double);
begin
   Value.AsNumber:=val;
end;

// GetIsNaN
//
function TdwsJSONValue.GetIsNaN : Boolean;
begin
   Result:=not (    Assigned(Self)
                and (FValueType in [jvtNumber])
                and Math.IsNan(Value.AsNumber));
end;

// GetAsInteger
//
function TdwsJSONValue.GetAsInteger : Int64;
begin
   if Assigned(Self) then
      Result:=Value.AsInteger
   else Result:=0;
end;

// SetAsInteger
//
procedure TdwsJSONValue.SetAsInteger(const val : Int64);
begin
   Value.AsInteger:=val;
end;

// DetachChild
//
procedure TdwsJSONValue.DetachChild(child : TdwsJSONValue);
begin
   Assert(False);
end;

// GetValueType
//
function TdwsJSONValue.GetValueType : TdwsJSONValueType;
begin
   if Assigned(Self) then
      Result:=FValueType
   else Result:=jvtUndefined;
end;

// GetName
//
function TdwsJSONValue.GetName(index : Integer) : String;
begin
   if Assigned(Self) then
      Result:=DoGetName(index)
   else Result:='';
end;

// DoGetName
//
function TdwsJSONValue.DoGetName(index : Integer) : String;
begin
   Result:='';
end;

// GetElement
//
function TdwsJSONValue.GetElement(index : Integer) : TdwsJSONValue;
begin
   if Assigned(Self) then
      Result:=DoGetElement(index)
   else Result:=nil;
end;

// DoGetElement
//
function TdwsJSONValue.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   Result:=nil;
end;

// GetItem
//
function TdwsJSONValue.GetItem(const name : String) : TdwsJSONValue;
begin
   if Assigned(Self) then
      Result:=DoGetItem(name)
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONValue.DoGetItem(const name : String) : TdwsJSONValue;
begin
   Result:=nil;
end;

// SetItem
//
procedure TdwsJSONValue.SetItem(const name : String; const value : TdwsJSONValue);
begin
   if Assigned(Self) then
      DoSetItem(name, value)
   else raise EdwsJSONException.CreateFmt('Can''t set member "%s" of Undefined', [name]);
end;

// RaiseJSONException
//
class procedure TdwsJSONValue.RaiseJSONException(const msg : String);
begin
   raise EdwsJSONException.Create(msg);
end;

// RaiseJSONParseError
//
class procedure TdwsJSONValue.RaiseJSONParseError(const msg : String; c : WideChar = #0);
begin
   if c<=#31 then
      raise EdwsJSONParseError.CreateFmt(msg, [IntToStr(Ord(c))])
   else if c>#127 then
      raise EdwsJSONParseError.CreateFmt(msg, ['U+'+IntToHex(Ord(c), 4)])
   else raise EdwsJSONParseError.CreateFmt(msg, [String(c)]);
end;

// ------------------
// ------------------ TdwsJSONObject ------------------
// ------------------

// Create
//
constructor TdwsJSONObject.Create;
begin
   FValueType:=jvtObject;
end;

// Destroy
//
destructor TdwsJSONObject.Destroy;
begin
   Clear;
   inherited;
end;

// Clone
//
function TdwsJSONObject.Clone : TdwsJSONObject;
begin
   if Self<>nil then
      Result:=(DoClone as TdwsJSONObject)
   else Result:=nil;
end;

// WriteTo
//
procedure TdwsJSONObject.WriteTo(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginObject;
   for i:=0 to ElementCount-1 do begin
      writer.WriteName(FItems^[i].Name);
      FItems^[i].Value.WriteTo(writer);
   end;
   writer.EndObject;
end;

// DoElementCount
//
function TdwsJSONObject.DoElementCount : Integer;
begin
   Result:=FCount;
end;

// Clear
//
procedure TdwsJSONObject.Clear;
var
   i : Integer;
   v : TdwsJSONValue;
begin
   for i:=0 to FCount-1 do begin
      v:=FItems^[i].Value;
      v.FOwner:=nil;
      v.DecRefCount;
      FItems^[i].Name:='';
   end;
   FreeMem(FItems);
   FItems:=nil;
   FCount:=0;
   FCapacity:=0;
end;

// Grow
//
procedure TdwsJSONObject.Grow;
begin
   SetCapacity(FCapacity+8+(FCapacity shr 2));
end;

// SetCapacity
//
procedure TdwsJSONObject.SetCapacity(newCapacity : Integer);
begin
   FCapacity:=newCapacity;
   ReallocMem(FItems, FCapacity*SizeOf(TdwsJSONPair));
   FillChar(FItems[FCount], (FCapacity-FCount)*SizeOf(TdwsJSONPair), 0);
end;

// Add
//
procedure TdwsJSONObject.Add(const aName : String; aValue : TdwsJSONValue);
begin
   Assert(aValue.Owner=nil);
   aValue.FOwner:=Self;
   if FCount=FCapacity then Grow;
   FItems^[FCount].Value:=aValue;
   FItems^[FCount].Name:=aName;
   FItems^[FCount].Hash:=SimpleStringHash(aName);
   Inc(FCount);
end;

// AddObject
//
function TdwsJSONObject.AddObject(const name : String) : TdwsJSONObject;
begin
   Result:=TdwsJSONObject.Create;
   Add(name, Result);
end;

// AddArray
//
function TdwsJSONObject.AddArray(const name : String) : TdwsJSONArray;
begin
   Result:=TdwsJSONArray.Create;
   Add(name, Result);
end;

// AddValue
//
function TdwsJSONObject.AddValue(const name : String) : TdwsJSONImmediate;
begin
   Result:=TdwsJSONImmediate.Create;
   Add(name, Result);
end;

// MergeDuplicates
//
procedure TdwsJSONObject.MergeDuplicates;
var
   i, j : Integer;
   h : Cardinal;
begin
   for i:=FCount-1 downto 1 do begin
      h:=FItems[i].Hash;
      for j:=i-1 downto 0 do begin
         if (FItems[j].Hash=h) and (FItems[j].Name=FItems[i].Name) then
            DetachIndex(j);
      end;
   end;
end;

// AddValue (string)
//
function TdwsJSONObject.AddValue(const name, value : String) : TdwsJSONImmediate;
begin
   Result:=AddValue(name);
   Result.AsString:=value;
end;

// AddValue (number)
//
function TdwsJSONObject.AddValue(const name : String; const value : Double) : TdwsJSONImmediate;
begin
   Result:=AddValue(name);
   Result.AsNumber:=value;
end;

// AddValue (bool)
//
function TdwsJSONObject.AddValue(const name : String; const value : Boolean) : TdwsJSONImmediate;
begin
   Result:=AddValue(name);
   Result.AsBoolean:=value;
end;

// DetachChild
//
procedure TdwsJSONObject.DetachChild(child : TdwsJSONValue);
var
   i : Integer;
begin
   i:=IndexOfValue(child);
   if i>=0 then
      DetachIndex(i);
end;

// DetachIndex
//
procedure TdwsJSONObject.DetachIndex(i : Integer);
var
   n : Integer;
   child : TdwsJSONValue;
begin
   child:=FItems[i].Value;
   if child.FOwner=Self then begin
      child.FOwner:=nil;
      child.DecRefCount;
   end;
   Finalize(FItems[i]);
   n:=FCount-1;
   if i<n then
      Move(FItems[i+1], FItems[i], (n-i)*SizeOf(TdwsJSONPair));
   FillChar(FItems[n], SizeOf(TdwsJSONPair), 0);
   FCount:=n;
end;

// DoGetName
//
function TdwsJSONObject.DoGetName(index : Integer) : String;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=FItems^[index].Name
   else Result:='';
end;

// DoGetElement
//
function TdwsJSONObject.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=FItems^[index].Value
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONObject.DoGetItem(const name : String) : TdwsJSONValue;
var
   i : Integer;
begin
   i:=IndexOfName(name);
   if i>=0 then
      Result:=FItems^[i].Value
   else Result:=nil;
end;

// DoSetItem
//
procedure TdwsJSONObject.DoSetItem(const name : String; const value : TdwsJSONValue);
var
   i : Integer;
   member : TdwsJSONValue;
begin
   i:=IndexOfName(name);
   if i>=0 then begin

      if value<>nil then begin

         member:=FItems^[i].Value;
         member.FOwner:=nil;
         member.DecRefCount;

         FItems^[i].Value:=value;
         value.Detach;
         value.FOwner:=Self;

      end else DetachIndex(i);

   end else if value<>nil then begin

      Add(name, value);

   end;
end;

// DoParse
//
procedure TdwsJSONObject.DoParse(initialChar : WideChar; parserState : TdwsJSONParserState);
var
   c : WideChar;
   name : String;
   locValue : TdwsJSONValue;
begin
   Assert(initialChar='{');
   repeat
      c:=parserState.SkipBlanks(parserState.NeedChar());
      if c<>'"' then begin
         if FCount=0 then Break;
         RaiseJSONParseError('Invalid object pair name start character "%s"', c)
      end;
      {$ifdef FPC}
      name:=UTF8Encode(parserState.ParseJSONString(c));
      {$else}
      name:=parserState.ParseJSONString(c);
      {$endif}
      c:=parserState.SkipBlanks(parserState.NeedChar());
      if c<>':' then
         RaiseJSONParseError('Invalid object pair name separator character "%s"', c);
      locValue:=TdwsJSONValue.Parse(parserState);
      if locValue=nil then
         RaiseJSONParseError('Missing element value');
      Add(name, locValue);
      c:=parserState.SkipBlanks(parserState.TrailCharacter);
   until c<>',';
   if c<>'}' then
      RaiseJSONParseError('Invalid object termination character "%s"', c);
   if parserState.DuplicatesOption=jdoOverwrite then
      MergeDuplicates;
   parserState.TrailCharacter:=' ';
end;

// DoClone
//
function TdwsJSONObject.DoClone : TdwsJSONValue;
var
   obj : TdwsJSONObject;
   member : TdwsJSONValue;
   i : Integer;
begin
   obj:=TdwsJSONObject.Create;
   obj.SetCapacity(FCount);
   obj.FCount:=FCount;
   for i:=0 to FCount-1 do begin
      obj.FItems[i].Name:=FItems[i].Name;
      obj.FItems[i].Hash:=FItems[i].Hash;
      member:=FItems[i].Value.Clone;
      member.FOwner:=Self;
      obj.FItems[i].Value:=member;
   end;
   Result:=obj;
end;

// DoExtend
//
procedure TdwsJSONObject.DoExtend(other : TdwsJSONValue);
var
   i, k : Integer;
   otherObj : TdwsJSONObject;
   member : TdwsJSONValue;
begin
   if other.ClassType<>TdwsJSONObject then
      RaiseJSONException('Can only extend Object with Object');
   otherObj:=TdwsJSONObject(other);
   for i:=0 to otherObj.FCount-1 do begin
      k:=IndexOfName(otherObj.FItems[i].Name);
      if k>=0 then begin
         member:=FItems[k].Value;
         member.FOwner:=nil;
         member.DecRefCount;
         member:=otherObj.FItems[i].Value.Clone;
         member.FOwner:=Self;
         FItems[k].Value:=member;
      end else begin
         Add(otherObj.FItems[i].Name, otherObj.FItems[i].Value.Clone);
      end;
   end;
end;

// IndexOfName
//
function TdwsJSONObject.IndexOfName(const name : String) : Integer;
var
   i : Integer;
   h : Cardinal;
begin
   h:=SimpleStringHash(name);
   for i:=0 to FCount-1 do
      if (FItems^[i].Hash=h) and (FItems^[i].Name=name) then
         Exit(i);
   Result:=-1;
end;

// IndexOfValue
//
function TdwsJSONObject.IndexOfValue(const aValue : TdwsJSONValue) : Integer;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      if FItems^[i].Value=aValue then
         Exit(i);
   Result:=-1;
end;

// ------------------
// ------------------ TdwsJSONArray ------------------
// ------------------

// Create
//
constructor TdwsJSONArray.Create;
begin
   FValueType:=jvtArray;
end;

// Destroy
//
destructor TdwsJSONArray.Destroy;
begin
   Clear;
   inherited;
end;

// Clone
//
function TdwsJSONArray.Clone : TdwsJSONArray;
begin
   if Self<>nil then
      Result:=(DoClone as TdwsJSONArray)
   else Result:=nil;
end;

// WriteTo
//
procedure TdwsJSONArray.WriteTo(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i:=0 to ElementCount-1 do
      Elements[i].WriteTo(writer);
   writer.EndArray;
end;

// Grow
//
procedure TdwsJSONArray.Grow;
begin
   SetCapacity(FCapacity+8+(FCapacity shr 2));
end;

// SetCapacity
//
procedure TdwsJSONArray.SetCapacity(newCapacity : Integer);
begin
   FCapacity:=newCapacity;
   ReallocMem(FElements, FCapacity*SizeOf(Pointer));
end;

// DetachChild
//
procedure TdwsJSONArray.DetachChild(child : TdwsJSONValue);
var
   i : Integer;
begin
   for i:=0 to FCount-1 do begin
      if FElements^[i]=child then begin
         DeleteIndex(i);
         Break;
      end;
   end;
end;

// DeleteIndex
//
procedure TdwsJSONArray.DeleteIndex(idx : Integer);
var
   child : TdwsJSONValue;
begin
   child:=FElements[idx];
   if child.FOwner=Self then begin
      child.FOwner:=nil;
      child.DecRefCount;
   end;
   Move(FElements[idx+1], FElements[idx], (FCount-1-idx)*SizeOf(Pointer));
   Dec(FCount);
end;

// DoElementCount
//
function TdwsJSONArray.DoElementCount : Integer;
begin
   Result:=FCount;
end;

// Clear
//
procedure TdwsJSONArray.Clear;
var
   i : Integer;
   v : TdwsJSONValue;
begin
   for i:=0 to FCount-1 do begin
      v:=FElements^[i];
      v.FOwner:=nil;
      v.DecRefCount;
   end;
   FreeMem(FElements);
   FElements:=nil;
   FCount:=0;
   FCapacity:=0;
end;

// Add
//
procedure TdwsJSONArray.Add(aValue : TdwsJSONValue);
begin
   Assert(aValue.Owner=nil);
   aValue.FOwner:=Self;
   if FCount=FCapacity then Grow;
   FElements^[FCount]:=aValue;
   Inc(FCount);
end;

// AddObject
//
function TdwsJSONArray.AddObject : TdwsJSONObject;
begin
   Result:=TdwsJSONObject.Create;
   Add(Result);
end;

// AddArray
//
function TdwsJSONArray.AddArray : TdwsJSONArray;
begin
   Result:=TdwsJSONArray.Create;
   Add(Result);
end;

// AddValue
//
function TdwsJSONArray.AddValue : TdwsJSONImmediate;
begin
   Result:=TdwsJSONImmediate.Create;
   Add(Result);
end;

// DoGetName
//
function TdwsJSONArray.DoGetName(index : Integer) : String;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=IntToStr(index)
   else Result:='';
end;

// DoGetElement
//
function TdwsJSONArray.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=TdwsJSONValue(FElements^[index])
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONArray.DoGetItem(const name : String) : TdwsJSONValue;
var
   i : Integer;
begin
   i:=StrToIntDef(name, -1);
   Result:=DoGetElement(i);
end;

// DoSetItem
//
procedure TdwsJSONArray.DoSetItem(const name : String; const value : TdwsJSONValue);
var
   i : Integer;
begin
   i:=StrToIntDef(name, -1);
   if i<0 then
      raise EdwsJSONException.CreateFmt('Invalid array member "%s"', [name]);

   if i<FCount then begin

      if value <> nil then begin

         FElements[i].FOwner:=nil;
         FElements[i].DecRefCount;
         FElements[i]:=value;
         value.Detach;
         value.FOwner:=Self;

      end else DeleteIndex(i);

   end else if value<>nil then begin

      raise EdwsJSONException.Create('extending array by index not supported yet');

   end;
end;

// DoParse
//
procedure TdwsJSONArray.DoParse(initialChar : WideChar; parserState : TdwsJSONParserState);
var
   locValue : TdwsJSONValue;
begin
   Assert(initialChar='[');
   repeat
      locValue:=TdwsJSONValue.Parse(parserState);
      if locValue=nil then Break;
      Add(locValue);
      parserState.TrailCharacter:=parserState.SkipBlanks(parserState.TrailCharacter);
   until parserState.TrailCharacter<>',';
   if parserState.TrailCharacter<>']' then
      RaiseJSONParseError('Invalid array termination character "%s"', parserState.TrailCharacter);
   parserState.TrailCharacter:=' ';
end;

// DoClone
//
function TdwsJSONArray.DoClone : TdwsJSONValue;
var
   arr : TdwsJSONArray;
   elem : TdwsJSONValue;
   i : Integer;
begin
   arr:=TdwsJSONArray.Create;
   arr.SetCapacity(FCount);
   arr.FCount:=FCount;
   for i:=0 to FCount-1 do begin
      elem:=FElements^[i].Clone;
      elem.FOwner:=Self;
      arr.FElements^[i]:=elem;
   end;
   Result:=arr;
end;

// DoExtend
//
procedure TdwsJSONArray.DoExtend(other : TdwsJSONValue);
begin
   RaiseJSONException('Cannot extend arrays (yet)');
end;

// ------------------
// ------------------ TdwsJSONImmediate ------------------
// ------------------

// GetAsString
//
function TdwsJSONImmediate.GetAsString : String;
begin
   if Assigned(Self) then
      Result:=FValue
   else Result:='undefined';
end;

// SetAsString
//
procedure TdwsJSONImmediate.SetAsString(const val : String);
begin
   FValue:=val;
   FValueType:=jvtString;
end;

// GetIsNull
//
function TdwsJSONImmediate.GetIsNull : Boolean;
begin
   if Assigned(Self) then
      Result:=(FValueType=jvtNull)
   else Result:=True;
end;

// SetIsNull
//
procedure TdwsJSONImmediate.SetIsNull(const val : Boolean);
begin
   if val<>GetIsNull then begin
      if val then begin
         VarClear(FValue);
         FValueType:=jvtNull;
      end else AsString:='';
   end;
end;

// GetAsBoolean
//
function TdwsJSONImmediate.GetAsBoolean : Boolean;
begin
   if not Assigned(Self) then Exit(False);
   case VarType(FValue) of
      varEmpty : Result:=False;
      varBoolean : Result:=FValue;
      varDouble : Result:=(FValue<>0);
   else
      Result:=(FValue='true');
   end;
end;

// SetAsBoolean
//
procedure TdwsJSONImmediate.SetAsBoolean(const val : Boolean);
begin
   FValue:=val;
   FValueType:=jvtBoolean;
end;

// GetAsNumber
//
function TdwsJSONImmediate.GetAsNumber : Double;
begin
   if not Assigned(Self) then Exit(NaN);
   case VarType(FValue) of
      varEmpty : Result:=0;
      varBoolean : if FValue then Result:=-1 else Result:=0;
      varDouble : Result:=FValue;
   else
      Result:=StrToFloatDef(FValue, 0);
   end;
end;

// SetAsNumber
//
procedure TdwsJSONImmediate.SetAsNumber(const val : Double);
begin
   FValue:=val;
   FValueType:=jvtNumber;
end;

// GetAsInteger
//
function TdwsJSONImmediate.GetAsInteger : Int64;
begin
   Result:=Round(GetAsNumber);
end;

// SetAsInteger
//
procedure TdwsJSONImmediate.SetAsInteger(const val : Int64);
begin
   AsNumber:=val;
end;

// DoParse
//
procedure TdwsJSONImmediate.DoParse(initialChar : WideChar; parserState : TdwsJSONParserState);
begin
   parserState.TrailCharacter:=' ';
   case initialChar of
      '"' :
         {$ifdef FPC}
         AsString:=UTF8Encode(parserState.ParseJSONString(initialChar));
         {$else}
         AsString:=parserState.ParseJSONString(initialChar);
         {$endif}
      '0'..'9', '-' :
         AsNumber:=parserState.ParseJSONNumber(initialChar);
      't' :
         if     (parserState.NeedChar()='r')
            and (parserState.NeedChar()='u')
            and (parserState.NeedChar()='e') then
            AsBoolean:=True
         else RaiseJSONParseError('Invalid immediate value');
      'f' :
         if     (parserState.NeedChar()='a')
            and (parserState.NeedChar()='l')
            and (parserState.NeedChar()='s')
            and (parserState.NeedChar()='e') then
            AsBoolean:=False
         else RaiseJSONParseError('Invalid immediate value');
      'n' :
         if     (parserState.NeedChar()='u')
            and (parserState.NeedChar()='l')
            and (parserState.NeedChar()='l') then
            IsNull:=True
         else RaiseJSONParseError('Invalid immediate value');
   else
      RaiseJSONParseError('Invalid immediate value');
   end;
end;

// DoClone
//
function TdwsJSONImmediate.DoClone : TdwsJSONValue;
begin
   Result:=TdwsJSONImmediate.Create;
   TdwsJSONImmediate(Result).FValue:=FValue;
   TdwsJSONImmediate(Result).FValueType:=FValueType;
end;

// DoExtend
//
procedure TdwsJSONImmediate.DoExtend(other : TdwsJSONValue);
begin
   RaiseJSONException('Cannot extend immediate values');
end;

// Clone
//
function TdwsJSONImmediate.Clone : TdwsJSONImmediate;
begin
   if Self<>nil then
      Result:=(DoClone as TdwsJSONImmediate)
   else Result:=nil;
end;

// WriteTo
//
procedure TdwsJSONImmediate.WriteTo(writer : TdwsJSONWriter);
begin
   case VarType(FValue) of
      varEmpty :
         writer.WriteNull;
      varBoolean :
         writer.WriteBoolean(TVarData(FValue).VBoolean);
      varDouble :
         writer.WriteNumber(TVarData(FValue).VDouble);
      varUString :
         {$ifdef FPC}
         writer.WriteString(String(TVarData(FValue).VString));
         {$else}
         writer.WriteString(String(TVarData(FValue).VUString));
         {$endif}
   else
      Assert(False, 'Unsupported type');
   end;
end;

// ParseString
//
class function TdwsJSONImmediate.ParseString(const json : String) : TdwsJSONImmediate;
var
   locValue : TdwsJSONValue;
begin
   locValue:=TdwsJSONValue.ParseString(json);
   if locValue is TdwsJSONImmediate then
      Result:=TdwsJSONImmediate(locValue)
   else begin
      locValue.Free;
      Result:=nil;
   end;
end;

// FromVariant
//
class function TdwsJSONImmediate.FromVariant(const v : Variant) : TdwsJSONImmediate;
begin
   Result:=TdwsJSONImmediate.Create;
   case VarType(v) of
      varNull, varUString, varDouble, varBoolean :
         Result.FValue:=v;
   else
      if VarIsNumeric(v) then
         Result.FValue:=Double(v)
      else if VarIsStr(v) then
         Result.FValue:=String(v)
      else raise EdwsJSONException.CreateFmt('Unsupported VarType in FromVariant (%d)',
                                             [VarType(v)]);
   end;
end;

// DoSetItem
//
procedure TdwsJSONImmediate.DoSetItem(const name : String; const value : TdwsJSONValue);
begin
   raise EdwsJSONException.CreateFmt('Can''t set member "%s" of immediate value', [name]);
end;

// ------------------
// ------------------ TdwsJSONWriter ------------------
// ------------------

// Create
//
constructor TdwsJSONWriter.Create(aStream : TWriteOnlyBlockStream);
begin
   inherited Create;
   FOwnsStream:=(aStream=nil);
   if FOwnsStream then
      FStream:=TWriteOnlyBlockStream.Create
   else FStream:=aStream;
end;

// Destroy
//
destructor TdwsJSONWriter.Destroy;
begin
   if FOwnsStream then
      FStream.Free;
   FStateStack.Free;
   inherited;
end;

// BeginObject
//
procedure TdwsJSONWriter.BeginObject;
begin
   Assert(FState in [wsNone, wsObjectValue, wsArray, wsArrayValue]);
   FStateStack.Push(TRefCountedObject(FState));
   BeforeWriteImmediate;
   FState:=wsObject;
   FStream.WriteChar('{');
end;

// EndObject
//
procedure TdwsJSONWriter.EndObject;
begin
   if FState in [wsObject, wsObjectName] then begin
      Assert(FStateStack.Count>0);
      FState:=TdwsJSONWriterState(FStateStack.Peek);
      FStateStack.Pop;
      FStream.WriteChar('}');
      AfterWriteImmediate;
   end else raise EdwsJSONWriterError.Create('Value expected');
end;

// BeginArray
//
procedure TdwsJSONWriter.BeginArray;
begin
   Assert(FState in [wsNone, wsObjectValue, wsArray, wsArrayValue]);
   FStateStack.Push(TRefCountedObject(FState));
   BeforeWriteImmediate;
   FState:=wsArray;
   FStream.WriteChar('[');
end;

// EndArray
//
procedure TdwsJSONWriter.EndArray;
begin
   Assert(FState in [wsArray, wsArrayValue]);
   Assert(FStateStack.Count>0);
   FState:=TdwsJSONWriterState(FStateStack.Peek);
   FStateStack.Pop;
   FStream.WriteChar(']');
   AfterWriteImmediate;
end;

// WriteName
//
procedure TdwsJSONWriter.WriteName(const aName : String);
begin
   case FState of
      wsObject : ;
      wsObjectName : begin
         FStream.WriteChar(',');
      end;
   else
      Assert(False);
   end;
   WriteJavaScriptString(FStream, aName);
   FStream.WriteChar(':');
   FState:=wsObjectValue;
end;

// WriteString
//
procedure TdwsJSONWriter.WriteString(const str : String);
begin
   BeforeWriteImmediate;
   WriteJavaScriptString(FStream, str);
   AfterWriteImmediate;
end;

// WriteNumber
//
procedure TdwsJSONWriter.WriteNumber(const n : Double);
begin
   BeforeWriteImmediate;
   FStream.WriteString(FloatToStr(n, vJSONFormatSettings));
   AfterWriteImmediate;
end;

// WriteInteger
//
procedure TdwsJSONWriter.WriteInteger(const n : Int64);
var
   s : String;
begin
   BeforeWriteImmediate;
   FastInt64ToStr(n, s);
   FStream.WriteString(s);
   AfterWriteImmediate;
end;

// WriteBoolean
//
procedure TdwsJSONWriter.WriteBoolean(b : Boolean);
begin
   BeforeWriteImmediate;
   if b then
      FStream.WriteString('true')
   else FStream.WriteString('false');
   AfterWriteImmediate;
end;

// WriteNull
//
procedure TdwsJSONWriter.WriteNull;
begin
   BeforeWriteImmediate;
   FStream.WriteString('null');
   AfterWriteImmediate;
end;

// WriteDate
//
procedure TdwsJSONWriter.WriteDate(dt : TDateTime);
var
   y, m, d, h, n, s, z : Word;
begin
   BeforeWriteImmediate;

   FStream.WriteChar('"');

   DecodeDate(dt, y, m, d);
   FStream.WriteDigits(y, 4);
   FStream.WriteDigits(m, 2);
   FStream.WriteDigits(m, 2);

   DecodeTime(dt, h, n, s, z);
   if (h or n or s)<>0 then begin
      FStream.WriteChar('T');
      FStream.WriteDigits(h, 2);
      if (n or s)<>0 then begin
         FStream.WriteDigits(n, 2);
         if s<>0 then
            FStream.WriteDigits(s, 2);
      end;
   end;

   FStream.WriteChar('"');

   AfterWriteImmediate;
end;

// WriteStrings
//
procedure TdwsJSONWriter.WriteStrings(const str : TStrings);
var
   i : Integer;
begin
   BeginArray;
   for i:=0 to str.Count-1 do
      WriteString(str[i]);
   EndArray;
end;

// WriteStrings
//
procedure TdwsJSONWriter.WriteStrings(const str : array of String);
var
   i : Integer;
begin
   BeginArray;
   for i:=0 to High(str) do
      WriteString(str[i]);
   EndArray;
end;

// ToString
//
function TdwsJSONWriter.ToString : String;
begin
   Result:=FStream.ToString;
end;

// BeforeWriteImmediate
//
procedure TdwsJSONWriter.BeforeWriteImmediate;
begin
   case FState of
      wsArrayValue :
         FStream.WriteChar(',');
      wsObject :
         raise EdwsJSONWriterError.Create('Name expected');
      wsDone :
         Assert(False);
   end;
end;

// AfterWriteImmediate
//                                                            dwswebidltokenizer
procedure TdwsJSONWriter.AfterWriteImmediate;
begin
   case FState of
      wsNone :
         FState:=wsDone;
      wsArray :
         FState:=wsArrayValue;
      wsObjectValue :
         FState:=wsObjectName;
   end;
end;

// ------------------
// ------------------ TdwsJSONBeautifiedWriter ------------------
// ------------------

// Create
//
constructor TdwsJSONBeautifiedWriter.Create(aStream : TWriteOnlyBlockStream; initialTabs, indentTabs : Integer);
begin
   inherited Create(aStream);
   FTabs:=initialTabs;
   FIndent:=indentTabs;
end;

// WriteIndents
//
procedure TdwsJSONBeautifiedWriter.WriteIndents;
begin
   FStream.WriteString(StringOfChar(#9, FTabs));
end;

// EnterIndent
//
procedure TdwsJSONBeautifiedWriter.EnterIndent;
begin
   Inc(FTabs, FIndent);
end;

// LeaveIndent
//
procedure TdwsJSONBeautifiedWriter.LeaveIndent;
begin
   Dec(FTabs, FIndent);
   if FState in [wsObjectName, wsArrayValue] then begin
      FStream.WriteString(#13#10);
      WriteIndents;
   end else FStream.WriteChar(' ');
end;

// BeforeWriteImmediate
//
procedure TdwsJSONBeautifiedWriter.BeforeWriteImmediate;
begin
   inherited;
   case FState of
      wsArray, wsArrayValue : begin
         FStream.WriteString(#13#10);
         WriteIndents;
      end;
   end;
end;

// BeginObject
//
procedure TdwsJSONBeautifiedWriter.BeginObject;
begin
   inherited;
   EnterIndent;
end;

// EndObject
//
procedure TdwsJSONBeautifiedWriter.EndObject;
begin
   LeaveIndent;
   inherited;
end;

// BeginArray
//
procedure TdwsJSONBeautifiedWriter.BeginArray;
begin
   inherited;
   EnterIndent;
end;

// EndArray
//
procedure TdwsJSONBeautifiedWriter.EndArray;
begin
   LeaveIndent;
   inherited;
end;

// WriteName
//
procedure TdwsJSONBeautifiedWriter.WriteName(const aName : String);
begin
   case FState of
      wsObject :
         FStream.WriteString(#13#10);
      wsObjectName :
         FStream.WriteString(','#13#10);
   else
      Assert(False);
   end;
   WriteIndents;
   WriteJavaScriptString(FStream, aName);
   FStream.WriteString(' : ');
   FState:=wsObjectValue;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vJSONFormatSettings.DecimalSeparator:='.';

end.
