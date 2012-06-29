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

uses Classes, SysUtils, Variants, dwsUtils;

type

   TdwsJSONValueType = (jvtNull, jvtObject, jvtArray, jvtString, jvtNumber, jvtBoolean);

   TdwsJSONValue = class;
   TdwsJSONArray = class;
   TdwsJSONObject = class;
   TdwsJSONImmediate = class;
   TdwsJSONWriter = class;

   PdwsJSONBeautifyInfo = ^TdwsJSONBeautifyInfo;
   TdwsJSONBeautifyInfo = record
      Tabs : Integer;
      Indent : Integer;
   end;

   TdwsJSONNeedCharFunc = function : WideChar of object;

   // TdwsJSONValue
   //
   TdwsJSONValue = class
      private
         FOwner : TdwsJSONValue;

      protected
         FValueType : TdwsJSONValueType;

         procedure DetachChild(child : TdwsJSONValue); virtual;

         function GetValueType : TdwsJSONValueType;
         function GetName(index : Integer) : UnicodeString;
         function DoGetName(index : Integer) : UnicodeString; virtual;
         function GetElement(index : Integer) : TdwsJSONValue; inline;
         function DoGetElement(index : Integer) : TdwsJSONValue; virtual;
         function GetItem(const name : UnicodeString) : TdwsJSONValue; inline;
         function DoGetItem(const name : UnicodeString) : TdwsJSONValue; virtual;
         function DoElementCount : Integer; virtual;
         function GetValue(const index : Variant) : TdwsJSONValue;

         procedure DoParse(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc;
                           var trailCharacter : WideChar); virtual; abstract;
         procedure WriteTo(writer : TdwsJSONWriter); virtual; abstract;

         class procedure RaiseJSONException(const msg : UnicodeString); static;
         class procedure RaiseJSONParseError(const msg : UnicodeString; c : WideChar = #0); static;

      public
         destructor Destroy; override;

         class function Parse(const needChar : TdwsJSONNeedCharFunc;
                              var trailCharacter : WideChar) : TdwsJSONValue; static;
         class function ParseString(const json : UnicodeString) : TdwsJSONValue; static;

         function ToString : UnicodeString; reintroduce;
         function ToBeautifiedString(initialTabs, indentTabs : Integer) : UnicodeString;
         procedure Detach;

         property Owner : TdwsJSONValue read FOwner;
         property ValueType : TdwsJSONValueType read GetValueType;
         property Items[const name : UnicodeString] : TdwsJSONValue read GetItem;
         property Names[index : Integer] : UnicodeString read GetName;
         property Elements[index : Integer] : TdwsJSONValue read GetElement;
         function ElementCount : Integer;
         property Values[const index : Variant] : TdwsJSONValue read GetValue; default;

         function Value : TdwsJSONImmediate; inline;
   end;

   TdwsJSONPair = record
      Name : UnicodeString;
      Value : TdwsJSONValue;
   end;
   TdwsJSONPairArray = array [0..MaxInt shr 5] of TdwsJSONPair;
   PdwsJSONPairArray = ^TdwsJSONPairArray;

   // TdwsJSONObject
   //
   TdwsJSONObject = class (TdwsJSONValue)
      private
         FItems : PdwsJSONPairArray;
         FCapacity : Integer;
         FCount : Integer;

      protected
         procedure Grow;
         function IndexOfName(const name : UnicodeString) : Integer;
         function IndexOfValue(const value : TdwsJSONValue) : Integer;
         procedure DetachChild(child : TdwsJSONValue); override;

         function DoGetName(index : Integer) : UnicodeString; override;
         function DoGetElement(index : Integer) : TdwsJSONValue; override;
         function DoGetItem(const name : UnicodeString) : TdwsJSONValue; override;
         function DoElementCount : Integer; override;

         procedure DoParse(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc;
                           var trailCharacter : WideChar); override;
         procedure WriteTo(writer : TdwsJSONWriter); override;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear;

         procedure Add(const aName : UnicodeString; aValue : TdwsJSONValue);
         function AddObject(const name : UnicodeString) : TdwsJSONObject;
         function AddArray(const name : UnicodeString) : TdwsJSONArray;
         function AddValue(const name : UnicodeString) : TdwsJSONImmediate;
   end;

   PdwsJSONValueArray = ^TdwsJSONValueArray;
   TdwsJSONValueArray = array [0..MaxInt shr 3] of TdwsJSONValue;

   // TdwsJSONArray
   //
   TdwsJSONArray = class (TdwsJSONValue)
      private
         FElements : PdwsJSONValueArray;
         FCapacity : Integer;
         FCount : Integer;

      protected
         procedure Grow;
         procedure DetachChild(child : TdwsJSONValue); override;

         function DoGetName(index : Integer) : UnicodeString; override;
         function DoGetElement(index : Integer) : TdwsJSONValue; override;
         function DoGetItem(const name : UnicodeString) : TdwsJSONValue; override;
         function DoElementCount : Integer; override;

         procedure DoParse(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc;
                           var trailCharacter : WideChar); override;
         procedure WriteTo(writer : TdwsJSONWriter); override;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear;

         procedure Add(value : TdwsJSONValue);
         function AddObject : TdwsJSONObject;
         function AddArray : TdwsJSONArray;
         function AddValue : TdwsJSONImmediate;
   end;

   // TdwsJSONImmediate
   //
   TdwsJSONImmediate = class (TdwsJSONValue)
      private
         FValue : Variant;

      protected
         function GetAsString : UnicodeString; inline;
         procedure SetAsString(const val : UnicodeString); inline;
         function GetIsNull : Boolean; inline;
         procedure SetIsNull(const val : Boolean);
         function GetAsBoolean : Boolean;
         procedure SetAsBoolean(const val : Boolean); inline;
         function GetAsNumber : Double;
         procedure SetAsNumber(const val : Double); inline;

         procedure DoParse(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc;
                           var trailCharacter : WideChar); override;
         procedure WriteTo(writer : TdwsJSONWriter); override;

      public
         property AsString : UnicodeString read GetAsString write SetAsString;
         property IsNull : Boolean read GetIsNull write SetIsNull;
         property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
         property AsNumber : Double read GetAsNumber write SetAsNumber;

   end;

   TdwsJSONWriterState = (wsNone, wsObject, wsObjectValue, wsArray, wsArrayValue, wsDone);

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

         procedure WriteName(const aName : UnicodeString); virtual;
         procedure WriteString(const str : UnicodeString);
         procedure WriteNumber(const n : Double);
         procedure WriteInteger(const n : Integer);
         procedure WriteBoolean(b : Boolean);
         procedure WriteNull;

         procedure WriteStrings(const str : TStrings);

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

         procedure WriteName(const aName : UnicodeString); override;
   end;

   EdwsJSONException = class (Exception);
   EdwsJSONParseError = class (EdwsJSONException);

procedure WriteJavaScriptString(destStream : TWriteOnlyBlockStream; const str : UnicodeString);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vJSONFormatSettings : TFormatSettings;

// SkipBlanks
//
function SkipBlanks(currentChar : WideChar; const needChar : TdwsJSONNeedCharFunc) : WideChar; overload; inline;
begin
   Result:=currentChar;
   repeat
      case Result of
         #9..#13, ' ' : ;
      else
         Break;
      end;
      Result:=needChar;
   until False;
end;

// ParseJSONString
//
function ParseJSONString(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc) : UnicodeString;
var
   c : WideChar;
   wobs : TWriteOnlyBlockStream;
   hexBuf, hexCount, n, nw : Integer;
   localBufferPtr : PWideChar;
   localBuffer : array [0..59] of WideChar; // range adjusted to have a stack space of 128 for the proc
begin
   Assert(initialChar='"');
   wobs:=nil;
   try
      localBufferPtr:=@localBuffer[0];
      repeat
         c:=needChar;
         case c of
            #0..#31 :
               TdwsJSONValue.RaiseJSONParseError('Invalid string character %s', c);
            '"' : Break;
            '\' : begin
               c:=needChar;
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
                        c:=needChar;
                        case c of
                           '0'..'9' :
                              hexBuf:=(hexBuf shl 4)+Ord(c)-Ord('0');
                           'a'..'f' :
                              hexBuf:=(hexBuf shl 4)+Ord(c)-Ord('a')+10;
                           'A'..'F' :
                              hexBuf:=(hexBuf shl 4)+Ord(c)-Ord('A')+10;
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
      n:=(NativeInt(localBufferPtr)-NativeInt(@localBuffer[0])) div SizeOf(WideChar);
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
function ParseHugeJSONNumber(initialChars : PWideChar; initialCharCount : Integer;
                             const needChar : TdwsJSONNeedCharFunc;
                             var trailCharacter : WideChar) : Double;
var
   buf : UnicodeString;
   c : WideChar;
begin
   SetString(buf, initialChars, initialCharCount);
   repeat
      c:=needChar;
      case c of
         '0'..'9', '-', '+', 'e', 'E', '.' : buf:=buf+c;
      else
         trailCharacter:=c;
         Break;
      end;
   until False;
   Result:=StrToFloat(buf, vJSONFormatSettings);
end;

// ParseJSONNumber
//
function ParseJSONNumber(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc;
                         var trailCharacter : WideChar) : Double;
var
   bufPtr : PWideChar;
   c : WideChar;
   resultBuf : Extended;
   buf : array [0..40] of WideChar;
begin
   buf[0]:=initialChar;
   bufPtr:=@buf[1];
   repeat
      c:=needChar;
      case c of
         '0'..'9', '-', '+', 'e', 'E', '.' : begin
            bufPtr^:=c;
            Inc(bufPtr);
            if bufPtr=@buf[High(buf)] then
               Exit(ParseHugeJSONNumber(@buf[0], Length(buf)-1, needChar, trailCharacter));
         end;
      else
         trailCharacter:=c;
         Break;
      end;
   until False;
   bufPtr^:=#0;
   TextToFloat(PWideChar(@buf[0]), resultBuf, fvExtended, vJSONFormatSettings);
   Result:=resultBuf;
end;

// WriteJavaScriptString
//
procedure WriteJavaScriptString(destStream : TWriteOnlyBlockStream; const str : UnicodeString);
var
   c : WideChar;
begin
   destStream.WriteString('"');
   for c in str do begin
      case c of
         #0..#7, #9, #11, #12, #14..#31, #255..#65535 :
            destStream.WriteString(Format('\u%.04x', [Ord(c)]));
         #8 : destStream.WriteString('\t');
         #10 : destStream.WriteString('\n');
         #13 : destStream.WriteString('\r');
         '"' : destStream.WriteString('\"');
         '\' : destStream.WriteString('\\');
      else
         destStream.WriteChar(c);
      end;
   end;
   destStream.WriteChar('"');
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
class function TdwsJSONValue.Parse(const needChar : TdwsJSONNeedCharFunc; var trailCharacter : WideChar) : TdwsJSONValue;
var
   c : WideChar;
begin
   Result:=nil;
   if not Assigned(needChar) then Exit;
   repeat
      c:=needChar;
      case c of
         #0 : Break;
         #9..#13, ' ' : ;
         '{' : Result:=TdwsJSONObject.Create;
         '[' : Result:=TdwsJSONArray.Create;
         '0'..'9', '"', '-', 't', 'f', 'n' :
            Result:=TdwsJSONImmediate.Create;
         ']', '}' : begin
            // empty array or object
            trailCharacter:=c;
            Exit(nil);
         end;
      else
         RaiseJSONParseError('Invalid value start character "%s"', c);
      end;
   until Result<>nil;
   if Result<>nil then begin
      try
         Result.DoParse(c, needChar, trailCharacter);
      except
         Result.Free;
         raise;
      end;
   end;
end;

type
   TStringCharProvider = class
      Str : UnicodeString;
      Ptr, ColStart : PWideChar;
      Line : Integer;
      constructor Create(const aStr : UnicodeString);
      function NeedChar : WideChar;
      function Location : UnicodeString;
   end;

// Create
//
constructor TStringCharProvider.Create(const aStr : UnicodeString);
begin
   Str:=aStr;
   Ptr:=PWideChar(aStr);
end;

// NeedChar
//
function TStringCharProvider.NeedChar : WideChar;
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
function TStringCharProvider.Location : UnicodeString;
begin
   Result:=Format('line %d, col %d (offset %d)',
                  [Line+1,
                   (NativeInt(Ptr)-NativeInt(ColStart)) div SizeOf(WideChar)+1,
                   (NativeInt(Ptr)-NativeInt(PWideChar(Str))) div SizeOf(WideChar)+1]);
end;

// ParseString
//
class function TdwsJSONValue.ParseString(const json : UnicodeString) : TdwsJSONValue;
var
   c : WideChar;
   provider : TStringCharProvider;
begin
   Result:=nil;
   provider:=TStringCharProvider.Create(json);
   try
      try
         Result:=TdwsJSONValue.Parse(provider.NeedChar, c);
      except
         on e : EdwsJSONParseError do
            raise EdwsJSONParseError.CreateFmt('%s, at %s',
                                               [e.Message, provider.Location]);
      else
         raise;
      end;
   finally
      provider.Free;
   end;
end;

// ToString
//
function TdwsJSONValue.ToString : UnicodeString;
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
function TdwsJSONValue.ToBeautifiedString(initialTabs, indentTabs : Integer) : UnicodeString;
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
begin
   if FOwner<>nil then begin
      FOwner.DetachChild(Self);
      FOwner:=nil;
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
   else Result:=jvtNull;
end;

// GetName
//
function TdwsJSONValue.GetName(index : Integer) : UnicodeString;
begin
   if Assigned(Self) then
      Result:=DoGetName(index)
   else Result:='';
end;

// DoGetName
//
function TdwsJSONValue.DoGetName(index : Integer) : UnicodeString;
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
function TdwsJSONValue.GetItem(const name : UnicodeString) : TdwsJSONValue;
begin
   if Assigned(Self) then
      Result:=DoGetItem(name)
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONValue.DoGetItem(const name : UnicodeString) : TdwsJSONValue;
begin
   Result:=nil;
end;

// RaiseJSONException
//
class procedure TdwsJSONValue.RaiseJSONException(const msg : UnicodeString);
begin
   raise EdwsJSONException.Create(msg);
end;

// RaiseJSONParseError
//
class procedure TdwsJSONValue.RaiseJSONParseError(const msg : UnicodeString; c : WideChar = #0);
begin
   if c>#31 then
      raise EdwsJSONParseError.CreateFmt(msg, [c])
   else raise EdwsJSONParseError.CreateFmt(msg, ['U+'+IntToHex(Ord(c), 4)])
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

// WriteTo
//
procedure TdwsJSONObject.WriteTo(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginObject;
   for i:=0 to ElementCount-1 do begin
      writer.WriteName(FItems[i].Name);
      FItems[i].Value.WriteTo(writer);
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
      v:=FItems[i].Value;
      v.FOwner:=nil;
      v.Destroy;
      FItems[i].Name:='';
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
   FCapacity:=FCapacity+8+(FCapacity shr 2);
   ReallocMem(FItems, FCapacity*SizeOf(TdwsJSONPair));
   FillChar(FItems[FCount], (FCapacity-FCount)*SizeOf(TdwsJSONPair), 0);
end;

// Add
//
procedure TdwsJSONObject.Add(const aName : UnicodeString; aValue : TdwsJSONValue);
begin
   Assert(aValue.Owner=nil);
   aValue.FOwner:=Self;
   if FCount=FCapacity then Grow;
   FItems[FCount].Value:=aValue;
   FItems[FCount].Name:=aName;
   Inc(FCount);
end;

// AddObject
//
function TdwsJSONObject.AddObject(const name : UnicodeString) : TdwsJSONObject;
begin
   Result:=TdwsJSONObject.Create;
   Add(name, Result);
end;

// AddArray
//
function TdwsJSONObject.AddArray(const name : UnicodeString) : TdwsJSONArray;
begin
   Result:=TdwsJSONArray.Create;
   Add(name, Result);
end;

// AddValue
//
function TdwsJSONObject.AddValue(const name : UnicodeString) : TdwsJSONImmediate;
begin
   Result:=TdwsJSONImmediate.Create;
   Add(name, Result);
end;

// DetachChild
//
procedure TdwsJSONObject.DetachChild(child : TdwsJSONValue);
var
   i, n : Integer;
begin
   Assert(child.Owner=Self);
   i:=IndexOfValue(child);
   child.Detach;
   Finalize(FItems[i]);
   n:=FCount-1;
   if i<n then
      Move(FItems[i+1], FItems[i], (n-i)*SizeOf(TdwsJSONPair));
   FCount:=n;
end;

// DoGetName
//
function TdwsJSONObject.DoGetName(index : Integer) : UnicodeString;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=FItems[index].Name
   else Result:='';
end;

// DoGetElement
//
function TdwsJSONObject.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   if Cardinal(index)<Cardinal(FCount) then
      Result:=FItems[index].Value
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONObject.DoGetItem(const name : UnicodeString) : TdwsJSONValue;
var
   i : Integer;
begin
   i:=IndexOfName(name);
   if i>=0 then
      Result:=FItems[i].Value
   else Result:=nil;
end;

// DoParse
//
procedure TdwsJSONObject.DoParse(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc; var trailCharacter : WideChar);
var
   c : WideChar;
   name : UnicodeString;
   value : TdwsJSONValue;
begin
   Assert(initialChar='{');
   repeat
      c:=SkipBlanks(needChar(), needChar);
      if c<>'"' then
         RaiseJSONParseError('Invalid object pair name start character "%s"', c);
      name:=ParseJSONString(c, needChar);
      c:=SkipBlanks(needChar(), needChar);
      if c<>':' then
         RaiseJSONParseError('Invalid object pair name separator character "%s"', c);
      value:=TdwsJSONValue.Parse(needChar, c);
      Add(name, value);
      c:=SkipBlanks(c, needChar);
   until c<>',';
   if c<>'}' then
      RaiseJSONParseError('Invalid object termination character "%s"', c);
   trailCharacter:=' ';
end;

// IndexOfName
//
function TdwsJSONObject.IndexOfName(const name : UnicodeString) : Integer;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      if FItems[i].Name=name then
         Exit(i);
   Result:=-1;
end;

// IndexOfValue
//
function TdwsJSONObject.IndexOfValue(const value : TdwsJSONValue) : Integer;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      if FItems[i].Value=value then
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
   FCapacity:=FCapacity+8+(FCapacity shr 2);
   ReallocMem(FElements, FCapacity*SizeOf(Pointer));
end;

// DetachChild
//
procedure TdwsJSONArray.DetachChild(child : TdwsJSONValue);
var
   i : Integer;
begin
   Assert(child.Owner=Self);
   for i:=0 to FCount-1 do begin
      if FElements[i]=child then begin
         child.Detach;
         Move(FElements[i+1], FElements[i], (FCount-1-i)*SizeOf(Pointer));
         Dec(FCount);
      end;
   end;
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
      v:=FElements[i];
      v.FOwner:=nil;
      v.Free;
   end;
   FreeMem(FElements);
   FCount:=0;
   FCapacity:=0;
end;

// Add
//
procedure TdwsJSONArray.Add(value : TdwsJSONValue);
begin
   Assert(value.Owner=nil);
   value.FOwner:=Self;
   if FCount=FCapacity then Grow;
   FElements[FCount]:=value;
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
function TdwsJSONArray.DoGetName(index : Integer) : UnicodeString;
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
      Result:=TdwsJSONValue(FElements[index])
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONArray.DoGetItem(const name : UnicodeString) : TdwsJSONValue;
var
   i : Integer;
begin
   i:=StrToIntDef(name, -1);
   Result:=DoGetElement(i);
end;

// DoParse
//
procedure TdwsJSONArray.DoParse(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc;
                                var trailCharacter : WideChar);
var
   c : WideChar;
   value : TdwsJSONValue;
begin
   Assert(initialChar='[');
   repeat
      value:=TdwsJSONValue.Parse(needChar, c);
      if value=nil then Break;
      Add(value);
      c:=SkipBlanks(c, needChar);
   until c<>',';
   if c<>']' then
      RaiseJSONParseError('Invalid array termination character "%s"', c);
   trailCharacter:=' ';
end;

// ------------------
// ------------------ TdwsJSONImmediate ------------------
// ------------------

// GetAsString
//
function TdwsJSONImmediate.GetAsString : UnicodeString;
begin
   if Assigned(Self) then
      Result:=FValue
   else Result:='';
end;

// SetAsString
//
procedure TdwsJSONImmediate.SetAsString(const val : UnicodeString);
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
   if not Assigned(Self) then Exit(0);
   case VarType(FValue) of
      varEmpty : Result:=0;
      varBoolean : if FValue then Result:=-1 else Result:=0;
      varDouble : Result:=FValue;
   else
      Result:=StrToFloat(FValue);
   end;
end;

// SetAsNumber
//
procedure TdwsJSONImmediate.SetAsNumber(const val : Double);
begin
   FValue:=val;
   FValueType:=jvtNumber;
end;

// DoParse
//
procedure TdwsJSONImmediate.DoParse(initialChar : WideChar; const needChar : TdwsJSONNeedCharFunc;
                                    var trailCharacter : WideChar);
begin
   trailCharacter:=' ';
   case initialChar of
      '"' :
         AsString:=ParseJSONString(initialChar, needChar);
      '0'..'9', '-' :
         AsNumber:=ParseJSONNumber(initialChar, needChar, trailCharacter);
      't' :
         if (needChar='r') and (needChar='u') and (needChar='e') then
            AsBoolean:=True
         else RaiseJSONParseError('Invalid immediate value');
      'f' :
         if (needChar='a') and (needChar='l') and (needChar='s') and (needChar='e') then
            AsBoolean:=False
         else RaiseJSONParseError('Invalid immediate value');
      'n' :
         if (needChar='u') and (needChar='l') and (needChar='l') then
            IsNull:=True
         else RaiseJSONParseError('Invalid immediate value');
   else
      RaiseJSONParseError('Invalid immediate value');
   end;
end;

// WriteTo
//
procedure TdwsJSONImmediate.WriteTo(writer : TdwsJSONWriter);
begin
   case VarType(FValue) of
      varEmpty :
         writer.WriteNull;
      varBoolean :
         writer.WriteBoolean(FValue);
      varDouble :
         writer.WriteNumber(FValue);
   else
      writer.WriteString(FValue);
   end;
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
   Assert(FState in [wsNone, wsDone]);
   Assert(FStateStack.Count=0);
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
   FStateStack.Push(Pointer(FState));
   BeforeWriteImmediate;
   FState:=wsObject;
   FStream.WriteChar('{');
end;

// EndObject
//
procedure TdwsJSONWriter.EndObject;
begin
   Assert(FState in [wsObject, wsObjectValue]);
   Assert(FStateStack.Count>0);
   FState:=TdwsJSONWriterState(FStateStack.Peek);
   FStateStack.Pop;
   FStream.WriteChar('}');
   AfterWriteImmediate;
end;

// BeginArray
//
procedure TdwsJSONWriter.BeginArray;
begin
   Assert(FState in [wsNone, wsObjectValue, wsArray]);
   FStateStack.Push(Pointer(FState));
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
procedure TdwsJSONWriter.WriteName(const aName : UnicodeString);
begin
   case FState of
      wsObject : ;
      wsObjectValue : begin
         FStream.WriteChar(',');
      end;
   else
      Assert(False);
   end;
   WriteString(aName);
   FStream.WriteChar(':');
   FState:=wsObjectValue;
end;

// WriteString
//
procedure TdwsJSONWriter.WriteString(const str : UnicodeString);
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
procedure TdwsJSONWriter.WriteInteger(const n : Integer);
begin
   BeforeWriteImmediate;
   FStream.WriteString(IntToStr(n));
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
      wsDone :
         Assert(False);
   end;
end;

// AfterWriteImmediate
//
procedure TdwsJSONWriter.AfterWriteImmediate;
begin
   case FState of
      wsNone :
         FState:=wsDone;
      wsArray :
         FState:=wsArrayValue;
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
   if FState in [wsObjectValue, wsArrayValue] then begin
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
procedure TdwsJSONBeautifiedWriter.WriteName(const aName : UnicodeString);
begin
   case FState of
      wsObject :
         FStream.WriteString(#13#10);
      wsObjectValue :
         FStream.WriteString(','#13#10);
   else
      Assert(False);
   end;
   WriteIndents;
   WriteString(aName);
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
