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

   TdwsJSONNeedCharFunc = reference to function : Char;

   // TdwsJSONValue
   //
   TdwsJSONValue = class
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
         function DoElementCount : Integer; virtual;
         function GetValue(const index : Variant) : TdwsJSONValue;

         procedure DoParse(initialChar : Char; const needChar : TdwsJSONNeedCharFunc;
                           var trailCharacter : Char); virtual; abstract;
         procedure WriteTo(writer : TdwsJSONWriter); virtual; abstract;

      public
         constructor Create(aOwner : TdwsJSONValue); virtual;
         destructor Destroy; override;

         class function Parse(const needChar : TdwsJSONNeedCharFunc;
                              var trailCharacter : Char) : TdwsJSONValue; static;
         class function ParseString(const json : String) : TdwsJSONValue; static;

         function ToString : String; reintroduce;
         function ToBeautifiedString(initialTabs, indentTabs : Integer) : String;
         procedure Detach;

         property Owner : TdwsJSONValue read FOwner;
         property ValueType : TdwsJSONValueType read GetValueType;
         property Items[const name : String] : TdwsJSONValue read GetItem;
         property Names[index : Integer] : String read GetName;
         property Elements[index : Integer] : TdwsJSONValue read GetElement;
         function ElementCount : Integer;
         property Values[const index : Variant] : TdwsJSONValue read GetValue; default;
   end;

   // TdwsJSONObject
   //
   TdwsJSONObject = class (TdwsJSONValue)
      private
         FItems : TStringList;

      protected
         procedure DetachChild(child : TdwsJSONValue); override;

         function DoGetName(index : Integer) : String; override;
         function DoGetElement(index : Integer) : TdwsJSONValue; override;
         function DoGetItem(const name : String) : TdwsJSONValue; override;
         function DoElementCount : Integer; override;

         procedure DoParse(initialChar : Char; const needChar : TdwsJSONNeedCharFunc;
                           var trailCharacter : Char); override;
         procedure WriteTo(writer : TdwsJSONWriter); override;

      public
         constructor Create(aOwner : TdwsJSONValue); override;
         destructor Destroy; override;

         procedure Clear;

         procedure Add(const name : String; value : TdwsJSONValue);
         function AddObject(const name : String) : TdwsJSONObject;
         function AddArray(const name : String) : TdwsJSONArray;
         function AddValue(const name : String) : TdwsJSONImmediate;
   end;

   // TdwsJSONArray
   //
   TdwsJSONArray = class (TdwsJSONValue)
      private
         FElements : TTightList;

      protected
         procedure DetachChild(child : TdwsJSONValue); override;

         function DoGetName(index : Integer) : String; override;
         function DoGetElement(index : Integer) : TdwsJSONValue; override;
         function DoGetItem(const name : String) : TdwsJSONValue; override;
         function DoElementCount : Integer; override;

         procedure DoParse(initialChar : Char; const needChar : TdwsJSONNeedCharFunc;
                           var trailCharacter : Char); override;
         procedure WriteTo(writer : TdwsJSONWriter); override;

      public
         constructor Create(aOwner : TdwsJSONValue); override;
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
         function GetAsString : String; inline;
         procedure SetAsString(const val : String); inline;
         function GetIsNull : Boolean; inline;
         procedure SetIsNull(const val : Boolean);
         function GetAsBoolean : Boolean; inline;
         procedure SetAsBoolean(const val : Boolean); inline;
         function GetAsNumber : Double; inline;
         procedure SetAsNumber(const val : Double); inline;

         procedure DoParse(initialChar : Char; const needChar : TdwsJSONNeedCharFunc; var trailCharacter : Char); override;
         procedure WriteTo(writer : TdwsJSONWriter); override;

      public
         constructor Create(aOwner : TdwsJSONValue); override;

         property AsString : String read GetAsString write SetAsString;
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

         procedure WriteName(const aName : String); virtual;
         procedure WriteString(const str : String);
         procedure WriteNumber(const n : Double);
         procedure WriteBoolean(b : Boolean);
         procedure WriteNull;

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

   EdwsJSONParseError = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// WriteStringAsJSONString
//
function WriteStringAsJSONString(dest : TWriteOnlyBlockStream; const str : String) : String;
var
   c : Char;
begin
   dest.WriteString('"');
   for c in str do begin
      case c of
         #0..#7, #9, #11, #12, #14..#31 :
            dest.WriteString(Format('\u%.04x', [Ord(c)]));
         #8 : dest.WriteString('\t');
         #10 : dest.WriteString('\n');
         #13 : dest.WriteString('\r');
         '"' : dest.WriteString('\"');
         '\' : dest.WriteString('\\');
      else
         dest.WriteChar(c);
      end;
   end;
   dest.WriteChar('"');
end;

// SkipBlanks
//
function SkipBlanks(currentChar : Char; const needChar : TdwsJSONNeedCharFunc) : Char;
begin
   Result:=currentChar;
   repeat
      case Result of
         #13, #10, #9, ' ' : ;
      else
         Break;
      end;
      Result:=needChar;
   until False;
end;

// RaiseJSONParseError
//
procedure RaiseJSONParseError(const msg : String; const args : array of const);
begin
   raise EdwsJSONParseError.CreateFmt(msg, args);
end;

// ParseJSONString
//
function ParseJSONString(initialChar : Char; const needChar : TdwsJSONNeedCharFunc) : String;
var
   c : Char;
   wobs : TWriteOnlyBlockStream;
   hexBuf : String;
begin
   Assert(initialChar='"');
   wobs:=TWriteOnlyBlockStream.Create;
   try
      while True do begin
         c:=needChar;
         case c of
            #0..#31 : RaiseJSONParseError('Invalid string character %d', [Ord(c)]);
            '"' : Break;
            '\' : begin
               c:=needChar;
               case c of
                  '"', '\', '/' : wobs.WriteChar('c');
                  'b' : wobs.WriteChar(#8);
                  'f' : wobs.WriteChar(#12);
                  'n' : wobs.WriteChar(#10);
                  'r' : wobs.WriteChar(#13);
                  't' : wobs.WriteChar(#9);
                  'u' : begin
                     hexBuf:='$';
                     while Length(hexBuf)<5 do begin
                        c:=needChar;
                        case c of
                           '0'..'9', 'a'..'f', 'A'..'F' : hexBuf:=hexBuf+c;
                        else
                           RaiseJSONParseError('Invalid unicode hex character "%s"', [c]);
                        end;
                     end;
                     wobs.WriteChar(Char(StrToInt(hexBuf)));
                  end;
               else
                  RaiseJSONParseError('Invalid character "%s" after escape', [c]);
               end;
            end;
         else
            wobs.WriteChar(c);
         end;
      end;
      Result:=wobs.ToString;
   finally
      wobs.Free;
   end;
end;

// ------------------
// ------------------ TdwsJSONValue ------------------
// ------------------

// Create
//
constructor TdwsJSONValue.Create(aOwner : TdwsJSONValue);
begin
   inherited Create;
   FOwner:=aOwner;
end;

// Destroy
//
destructor TdwsJSONValue.Destroy;
begin
   Detach;
   inherited;
end;

// Parse
//
class function TdwsJSONValue.Parse(const needChar : TdwsJSONNeedCharFunc; var trailCharacter : Char) : TdwsJSONValue;
var
   c : Char;
begin
   Result:=nil;
   if not Assigned(needChar) then Exit;
   repeat
      c:=needChar;
      case c of
         #0 : Break;
         #9, #13, #10, ' ' : ;
         '{' : Result:=TdwsJSONObject.Create(nil);
         '[' : Result:=TdwsJSONArray.Create(nil);
         '0'..'9', '"', '-', 't', 'f', 'n' :
            Result:=TdwsJSONImmediate.Create(nil);
      else
         RaiseJSONParseError('Invalid value start character "%s"', [c]);
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

// ParseString
//
class function TdwsJSONValue.ParseString(const json : String) : TdwsJSONValue;
var
   i, line, col : Integer;
   c : Char;
begin
   i:=1;
   line:=1;
   col:=1;
   try
      Result:=TdwsJSONValue.Parse(function : Char
                                  begin
                                     Result:=json[i];
                                     if Result=#10 then begin
                                        col:=1;
                                        Inc(line);
                                     end else Inc(col);
                                     Inc(i);
                                  end, c);
   except
      on e : EdwsJSONParseError do
         raise EdwsJSONParseError.CreateFmt('%s, at line %d, col %d (offset %d)', [e.Message, line, col, i]);
   else
      raise;
   end;
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

// ------------------
// ------------------ TdwsJSONObject ------------------
// ------------------

// Create
//
constructor TdwsJSONObject.Create(aOwner : TdwsJSONValue);
begin
   inherited;
   FValueType:=jvtObject;
   FItems:=TStringList.Create;
end;

// Destroy
//
destructor TdwsJSONObject.Destroy;
begin
   Clear;
   FItems.Free;
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
      writer.WriteName(FItems[i]);
      Elements[i].WriteTo(writer);
   end;
   writer.EndObject;
end;

// DoElementCount
//
function TdwsJSONObject.DoElementCount : Integer;
begin
   Result:=FItems.Count;
end;

// Clear
//
procedure TdwsJSONObject.Clear;
var
   i : Integer;
   v : TdwsJSONValue;
begin
   for i:=0 to ElementCount-1 do begin
      v:=Elements[i];
      v.FOwner:=nil;
      v.Free;
   end;
   FItems.Clear;
end;

// Add
//
procedure TdwsJSONObject.Add(const name : String; value : TdwsJSONValue);
begin
   Assert(value.Owner=nil);
   value.FOwner:=Self;
   FItems.AddObject(name, value);
end;

// AddObject
//
function TdwsJSONObject.AddObject(const name : String) : TdwsJSONObject;
begin
   Result:=TdwsJSONObject.Create(Self);
   FItems.AddObject(name, Result);
end;

// AddArray
//
function TdwsJSONObject.AddArray(const name : String) : TdwsJSONArray;
begin
   Result:=TdwsJSONArray.Create(Self);
   FItems.AddObject(name, Result);
end;

// AddValue
//
function TdwsJSONObject.AddValue(const name : String) : TdwsJSONImmediate;
begin
   Result:=TdwsJSONImmediate.Create(Self);
   FItems.AddObject(name, Result);
end;

// DetachChild
//
procedure TdwsJSONObject.DetachChild(child : TdwsJSONValue);
var
   i : Integer;
begin
   Assert(child.Owner=Self);
   i:=FItems.IndexOfObject(child);
   child.Detach;
   FItems.Delete(i);
end;

// DoGetName
//
function TdwsJSONObject.DoGetName(index : Integer) : String;
begin
   if Cardinal(index)<Cardinal(FItems.Count) then
      Result:=FItems[index]
   else Result:='';
end;

// DoGetElement
//
function TdwsJSONObject.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   if Cardinal(index)<Cardinal(FItems.Count) then
      Result:=TdwsJSONValue(FItems.Objects[index])
   else Result:=nil;
end;

// DoGetItem
//
function TdwsJSONObject.DoGetItem(const name : String) : TdwsJSONValue;
var
   i : Integer;
begin
   i:=FItems.IndexOf(name);
   if i>=0 then
      Result:=Elements[i]
   else Result:=nil;
end;

// DoParse
//
procedure TdwsJSONObject.DoParse(initialChar : Char; const needChar : TdwsJSONNeedCharFunc; var trailCharacter : Char);
var
   c : Char;
   name : String;
   value : TdwsJSONValue;
begin
   Assert(initialChar='{');
   repeat
      c:=SkipBlanks(' ', needChar);
      if c<>'"' then RaiseJSONParseError('Invalid object pair name start character "%s"', [c]);
      name:=ParseJSONString(c, needChar);
      c:=SkipBlanks(' ', needChar);
      if c<>':' then RaiseJSONParseError('Invalid object pair name separator character "%s"', [c]);
      value:=TdwsJSONValue.Parse(needChar, c);
      value.FOwner:=Self;
      FItems.AddObject(name, value);
      c:=SkipBlanks(c, needChar);
   until c<>',';
   if c<>'}' then
      RaiseJSONParseError('Invalid object termination character "%s"', [c]);
   trailCharacter:=' ';
end;

// ------------------
// ------------------ TdwsJSONArray ------------------
// ------------------

// Create
//
constructor TdwsJSONArray.Create(aOwner : TdwsJSONValue);
begin
   inherited;
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

// DetachChild
//
procedure TdwsJSONArray.DetachChild(child : TdwsJSONValue);
var
   i : Integer;
begin
   Assert(child.Owner=Self);
   i:=FElements.IndexOf(child);
   child.Detach;
   FElements.Delete(i);
end;

// DoElementCount
//
function TdwsJSONArray.DoElementCount : Integer;
begin
   Result:=FElements.Count;
end;

// Clear
//
procedure TdwsJSONArray.Clear;
var
   i : Integer;
   v : TdwsJSONValue;
begin
   for i:=0 to FElements.Count-1 do begin
      v:=TdwsJSONValue(FElements.List[i]);
      v.FOwner:=nil;
      v.Free;
   end;
   FElements.Clear;
end;

// Add
//
procedure TdwsJSONArray.Add(value : TdwsJSONValue);
begin
   Assert(value.Owner=nil);
   value.FOwner:=Self;
   FElements.Add(value);
end;

// AddObject
//
function TdwsJSONArray.AddObject : TdwsJSONObject;
begin
   Result:=TdwsJSONObject.Create(Self);
   FElements.Add(Result);
end;

// AddArray
//
function TdwsJSONArray.AddArray : TdwsJSONArray;
begin
   Result:=TdwsJSONArray.Create(Self);
   FElements.Add(Result);
end;

// AddValue
//
function TdwsJSONArray.AddValue : TdwsJSONImmediate;
begin
   Result:=TdwsJSONImmediate.Create(Self);
   FElements.Add(Result);
end;

// DoGetName
//
function TdwsJSONArray.DoGetName(index : Integer) : String;
begin
   if Cardinal(index)<Cardinal(FElements.Count) then
      Result:=IntToStr(index)
   else Result:='';
end;

// DoGetElement
//
function TdwsJSONArray.DoGetElement(index : Integer) : TdwsJSONValue;
begin
   if Cardinal(index)<Cardinal(FElements.Count) then
      Result:=TdwsJSONValue(FElements.List[index])
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

// DoParse
//
procedure TdwsJSONArray.DoParse(initialChar : Char; const needChar : TdwsJSONNeedCharFunc;
                                var trailCharacter : Char);
var
   c : Char;
   value : TdwsJSONValue;
begin
   Assert(initialChar='[');
   repeat
      value:=TdwsJSONValue.Parse(needChar, c);
      value.FOwner:=Self;
      FElements.Add(value);
      c:=SkipBlanks(c, needChar);
   until c<>',';
   if c<>']' then
      RaiseJSONParseError('Invalid array termination character "%s"', [c]);
   trailCharacter:=' ';
end;

// ------------------
// ------------------ TdwsJSONImmediate ------------------
// ------------------

// Create
//
constructor TdwsJSONImmediate.Create(aOwner : TdwsJSONValue);
begin
   inherited;
   FValueType:=jvtNull;
end;

// GetAsString
//
function TdwsJSONImmediate.GetAsString : String;
begin
   if Assigned(Self) then
      Result:=FValue
   else Result:='';
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
      Result:=VarIsEmpty(FValue)
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
procedure TdwsJSONImmediate.DoParse(initialChar : Char; const needChar : TdwsJSONNeedCharFunc;
                                    var trailCharacter : Char);
var
   buf : String;
   c : Char;
begin
   trailCharacter:=' ';
   case initialChar of
      '"' : AsString:=ParseJSONString(initialChar, needChar);
      '0'..'9', '-' : begin
         buf:=initialChar;
         while True do begin
            c:=needChar;
            case c of
               '0'..'9', '-', '+', 'e', 'E', '.' : buf:=buf+c;
            else
               trailCharacter:=c;
               Break;
            end;
         end;
         AsNumber:=StrToFloat(buf);
      end;
      't' :
         if (needChar='r') and (needChar='u') and (needChar='e') then
            AsBoolean:=True
         else RaiseJSONParseError('Invalid immediate value', []);
      'f' :
         if (needChar='a') and (needChar='l') and (needChar='s') and (needChar='e') then
            AsBoolean:=False
         else RaiseJSONParseError('Invalid immediate value', []);
      'n' :
         if (needChar='u') and (needChar='l') and (needChar='l') then
            IsNull:=True
         else RaiseJSONParseError('Invalid immediate value', []);
   else
      RaiseJSONParseError('Invalid immediate value', []);
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
   FOwnsStream:=(FStream=nil);
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
   Assert(FState in [wsNone, wsObjectValue, wsArray]);
   FStateStack.Push(Pointer(FState));
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
end;

// BeginArray
//
procedure TdwsJSONWriter.BeginArray;
begin
   Assert(FState in [wsNone, wsObjectValue, wsArray]);
   FStateStack.Push(Pointer(FState));
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
end;

// WriteName
//
procedure TdwsJSONWriter.WriteName(const aName : String);
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
procedure TdwsJSONWriter.WriteString(const str : String);
var
   c : Char;
begin
   BeforeWriteImmediate;
   FStream.WriteString('"');
   for c in str do begin
      case c of
         #0..#7, #9, #11, #12, #14..#31 :
            FStream.WriteString(Format('\u%.04x', [Ord(c)]));
         #8 : FStream.WriteString('\t');
         #10 : FStream.WriteString('\n');
         #13 : FStream.WriteString('\r');
         '"' : FStream.WriteString('\"');
         '\' : FStream.WriteString('\\');
      else
         FStream.WriteChar(c);
      end;
   end;
   FStream.WriteChar('"');
   AfterWriteImmediate;
end;

// WriteNumber
//
procedure TdwsJSONWriter.WriteNumber(const n : Double);
begin
   BeforeWriteImmediate;
   FStream.WriteString(FloatToStr(n));
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
procedure TdwsJSONBeautifiedWriter.WriteName(const aName : String);
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

end.
