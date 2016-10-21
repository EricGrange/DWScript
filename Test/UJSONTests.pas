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
unit UJSONTests;

interface

uses
   Classes, SysUtils, Math,
   dwsXPlatformTests, dwsJSON, dwsXPlatform, dwsUtils, dwsJSONPath;

type

   TdwsJSONTests = class (TTestCase)
      protected
         procedure JSONExtraComma;
         procedure JSONInvalidChar;
         procedure JSONInvalidUnicodeHexa;
         procedure JSONInvalidEscaped;
         procedure JSONInvalidStart;
         procedure JSONInvalidTrue;
         procedure JSONInvalidFalse;
         procedure JSONInvalidNull;
         procedure JSONInvalidImmediate;
         procedure JSONUnterminatedString;
         procedure JSONInvalidNameValueSeparator;
         procedure JSONInvalidNameValueEnding;
         procedure JSONMissingElementValue;
         procedure JSONWriterNoValue;
         procedure JSONWriterNoName;

         procedure JSONIntegerArrayBadStart;
         procedure JSONIntegerArrayBadChar;
         procedure JSONIntegerArrayBadComma;
         procedure JSONNumberArrayBadStart;
         procedure JSONNumberArrayBadChar;
         procedure JSONNumberArrayBadComma;
         procedure JSONStringArrayBadStart;
         procedure JSONStringArrayBadChar;
         procedure JSONStringArrayBadComma;

         function CompareStringArray(v1, v2 : TdwsJSONValue) : Integer;

         procedure JSONPathFailEmpty;
         procedure JSONPathFailSyntax;
         procedure JSONPathFailIndex;
         procedure JSONPathFailIndexUnfinished;
         procedure JSONPathFailProperty;
         procedure JSONPathFailPropertyUnfinished;
         procedure JSONPathFailDeepPropertyUnfinished;

      published
         procedure JSONTest;
         procedure ParseJSON;
         procedure AccessJSON;
         procedure JSONUnicodeLiteral;
         procedure JSONCRLF;
         procedure JSONQuote;
         procedure UndefinedJSON;
         procedure JSONEmptyObject;
         procedure JSONEmptyArray;
         procedure JSONSpecialChars;
         procedure JSONLongNumber;
         procedure JSONInvalidStuff;
         procedure JSONParseArrayInvalid;
         procedure NestedArrays;
         procedure MultipleElementsWithSameName;
         procedure SetItemTest;
         procedure SubItemFree;
         procedure ISO8601Test;
         procedure DeleteAdd;
         procedure WriterErrors;
         procedure ArrayTest;
         procedure DefaultValues;
         procedure RepositionInArray;
         procedure CloneAndDetach;
         procedure SortArray;
         procedure EnumerateNil;
         procedure EnumerateArray;

         procedure JSONPathBasic;
         procedure JSONPathFails;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cJSONbooks =
        '{ "books" : ['
         + '{ "id"     : 1,  "title"  : "Clean Code", "author" : { "name" : "Robert C. Martin" }, "price"  : 17.96  },'
         + '{ "id"     : 2, "title"  : "Maintainable JavaScript", "author" : { "name" : "Nicholas C. Zakas" }, "price"  : 10 },'
         + '{ "id"     : 3, "title"  : "JavaScript: The Good Parts", "author" : { "name" : "Douglas Crockford" }, "price"  : 15.67 }'
         + '], "nums" :  {"1" : "one", "2" : "two", "3" : 3 },'
         + '"leaf" :  [11, 22]'
      + ' }';

// ------------------
// ------------------ TdwsJSONTests ------------------
// ------------------

// JSONTest
//
procedure TdwsJSONTests.JSONTest;
var
   json : TdwsJSONObject;
begin
   json:=TdwsJSONObject.Create;

   CheckEquals('{}', json.ToString);
   CheckEquals('{ }', json.ToBeautifiedString(0, 3));

   json.AddValue('hello').AsString:='world';

   CheckEquals('{"hello":"world"}', json.ToString);
   CheckEquals('{'#13#10#9'"hello" : "world"'#13#10'}', json.ToBeautifiedString(0, 1));

   with json.AddArray('items') do begin
      AddValue;
      AddValue.AsNumber:=12.3;
      AddValue.AsBoolean:=True;
      AddValue.AsBoolean:=False;
      AddValue.IsNull:=True;
   end;

   CheckEquals('{"hello":"world","items":[null,12.3,true,false,null]}', json.ToString);
   CheckEquals( '{'#13#10
                  +#9'"hello" : "world",'#13#10
                  +#9'"items" : ['#13#10
                     +#9#9'null,'#13#10
                     +#9#9'12.3,'#13#10
                     +#9#9'true,'#13#10
                     +#9#9'false,'#13#10
                     +#9#9'null'#13#10
                  +#9']'#13#10
               +'}', json.ToBeautifiedString(0, 1));

   json.Free;
end;

// ParseJSON
//
procedure TdwsJSONTests.ParseJSON;
var
   json : TdwsJSONValue;
   sl : TStringList;
   buf : String;
begin
   json:=TdwsJSONValue.ParseString('"hello"');
   CheckEquals(TdwsJSONImmediate.ClassName, json.ClassName, '"hello"');
   CheckEquals('"hello"', json.ToString, '"hello"');
   json.Free;

   json:=TdwsJSONValue.ParseString('{"hello":"world","abc":123}');
   CheckEquals(TdwsJSONObject.ClassName, json.ClassName, '"hello"');
   CheckEquals('{"hello":"world","abc":123}', json.ToString, '"hello"');
   json.Free;

   sl:=TStringList.Create;
   try
      sl.LoadFromFile(ExtractFilePath(ParamStr(0))+'\Data\json.txt');
      json:=TdwsJSONValue.ParseString(sl.Text);
      CheckEquals(TdwsJSONObject.ClassName, json.ClassName, 'json.txt');
      CheckEquals(1, json.ElementCount, 'json.txt');
      CheckEquals(3, json.Elements[0].ElementCount, 'json.txt');
      CheckEquals('"templates"', json[0]['servlet'][0]['init-param']['templatePath'].ToString, 'json.txt');
      CheckEquals('', json['doh'][5]['bug'].ToString, 'json.txt');
      json.Free;
   finally
      sl.Free;
   end;

   buf:=LoadTextFromFile(ExtractFilePath(ParamStr(0))+'\Data\json2.txt');
   json:=TdwsJSONValue.ParseString(buf);
   try
      CheckEquals(TdwsJSONArray.ClassName, json.ClassName, 'json2.txt');
      CheckEquals(1, json.ElementCount, 'json2.txt a');
      CheckEquals(1, json.Elements[0].ElementCount, 'json2.txt b');
      CheckEquals(buf, json.ToString, 'json2.txt');
   finally
      json.Free;
   end;
end;

// AccessJSON
//
procedure TdwsJSONTests.AccessJSON;
const
   jsonData = '{"Result":[{"Links":[{"UrlTo":"http://atomos.com/ninja/","Anchor":"Manufacturer info",'
             +'"Type":"Text","Flag":[]}],"Index":1,"Rating":2.035556,"Visited":1330236394,"UrlFrom":'
             +'"http://anthonywrites.posterous.com/","IpFrom":"184.106.20.99","Title":"Anthony Agius - Home"},'
             +'{"Links":[{"UrlTo":"http://atomos.com/samurai/","Type":"Redirect","HttpCode":302,"Flag":[]}],'
             +'"Index":2,"Rating":0.941064,"Visited":1329500858,"UrlFrom":"http://theeditman.com/blogg/ct.ashx'
             +'?id=13592790-3605-42fd-9308-73c79199d1eb&url=http%3A%2F%2Fatomos.com%2Fsamurai%2F","IpFrom":'
             +'"64.202.163.118","Title":""},{"Links":[{"UrlTo":"http://atomos.com/ninja/","Anchor":"","Type":'
             +'"Text","Flag":["img"],"Alt":""}],"Index":3,"Rating":0.925152,"Visited":1329902294,"UrlFrom":'
             +'"http://www.lafcpug.org/events/supermeet_sf_2012.html","IpFrom":"64.93.81.159","Title":'
             +'"The Eleventh Annual San Francisco SuperMeet"},{"Links":[{"UrlTo":"http://atomos.com/",'
             +'"Type":"Redirect","HttpCode":302,"Flag":[]}],"Index":4,"Rating":0.915592,"Visited":1330795307,'
             +'"UrlFrom":"http://tienda.vantec.es/redirect.php?action=manufacturer&manufacturers_id=39",'
             +'"IpFrom":"194.79.85.30","Title":""}]}';
var
   json : TdwsJSONValue;
   result : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString(jsonData);
   try
      result:=json.Items['Result'];
      Check(result<>nil, 'Result is present');
      CheckEquals(4, result.ElementCount, '4 Entries');
      CheckEquals('http://theeditman.com/blogg/ct.ashx?id=13592790-3605-42fd-9308-73c79199d1eb&url=http%3A%2F%2Fatomos.com%2Fsamurai%2F',
                  result[1]['UrlFrom'].Value.AsString, 'long url');
   finally
      json.Free;
   end;
end;

// JSONUnicodeLiteral
//
procedure TdwsJSONTests.JSONUnicodeLiteral;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('"\u044F\u00aA"');
   CheckEquals(TdwsJSONImmediate.ClassName, json.ClassName, 'TdwsJSONImmediate');
   {$ifdef FPC}
   CheckEquals(UTF8Encode(WideChar($44f)+WideChar($aa)), TdwsJSONImmediate(json).AsString, 'unicode');
   {$else}
   CheckEquals(WideChar($44f)+WideChar($aa), TdwsJSONImmediate(json).AsString, 'unicode');
   {$endif}
   json.Free;

   json:=TdwsJSONObject.Create;
   TdwsJSONObject(json).AddValue('test', #$1234#$ABCD);
   CheckEquals('{"test":"\u1234\uABCD"}', json.ToString, 'encode');
   json.Free;
end;

// JSONCRLF
//
procedure TdwsJSONTests.JSONCRLF;
var
   json1, json2 : TdwsJSONValue;
begin
   json1:=TdwsJSONValue.ParseString('{"Value": []}');
   json2:=TdwsJSONValue.ParseString( '{'#13#10
                                    +#9'"Value": ['#13#10
                                    +#9']'#13#10
                                    +'}');
   CheckEquals('{"Value":[]}', json1.ToString, 'Roundtrip');
   CheckEquals(json1.ToString, json2.ToString, 'Json1 vs Json2');

   json1.Free;
   json2.Free;
end;

// JSONQuote
//
procedure TdwsJSONTests.JSONQuote;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"Value":"\""}');
   CheckEquals('"', json['Value'].Value.AsString, 'parse');
   CheckEquals('{"Value":"\""}', json.ToString, 'roundtrip');
   json.Free;
end;

// UndefinedJSON
//
procedure TdwsJSONTests.UndefinedJSON;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"hello":123}');

   Check(json['hello'].ValueType=jvtNumber, 'check hello value type');
   Check(json['missing'].ValueType=jvtUndefined, 'check missing value type');
   Check(json['missing'][2].ValueType=jvtUndefined, 'check missing[2] value type');

   json.Free;
end;

// JSONExtraComma
//
procedure TdwsJSONTests.JSONExtraComma;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"a":1,}');
   json.Free;
end;

// JSONInvalidChar
//
procedure TdwsJSONTests.JSONInvalidChar;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"a":"abc'#25'def"}');
   json.Free;
end;

// JSONInvalidUnicodeHexa
//
procedure TdwsJSONTests.JSONInvalidUnicodeHexa;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"a":"\u0z"}');
   json.Free;
end;

// JSONInvalidEscaped
//
procedure TdwsJSONTests.JSONInvalidEscaped;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"a":"\z"}');
   json.Free;
end;

// JSONInvalidStart
//
procedure TdwsJSONTests.JSONInvalidStart;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('z');
   json.Free;
end;

// JSONInvalidTrue
//
procedure TdwsJSONTests.JSONInvalidTrue;
begin
   TdwsJSONValue.ParseString('{"v":tru}');
end;

// JSONInvalidFalse
//
procedure TdwsJSONTests.JSONInvalidFalse;
begin
   TdwsJSONValue.ParseString('{"v":falze}');
end;

// JSONInvalidNull
//
procedure TdwsJSONTests.JSONInvalidNull;
begin
   TdwsJSONValue.ParseString('{"v":nul');
end;

// JSONInvalidImmediate
//
procedure TdwsJSONTests.JSONInvalidImmediate;
begin
   TdwsJSONValue.ParseString('{"v":bug}');
end;

// JSONUnterminatedString
//
procedure TdwsJSONTests.JSONUnterminatedString;
begin
   TdwsJSONValue.ParseString('"bug');
end;

// JSONInvalidNameValueSeparator
//
procedure TdwsJSONTests.JSONInvalidNameValueSeparator;
begin
   TdwsJSONValue.ParseString('{"n" bug');
end;

// JSONInvalidNameValueEnding
//
procedure TdwsJSONTests.JSONInvalidNameValueEnding;
begin
   TdwsJSONValue.ParseString('{"n":"bug"');
end;

// JSONMissingElementValue
//
procedure TdwsJSONTests.JSONMissingElementValue;
begin
   TdwsJSONValue.ParseString('{"v":}');
end;

// JSONWriterNoValue
//
procedure TdwsJSONTests.JSONWriterNoValue;
var
   wr : TdwsJSONWriter;
begin
   wr:=TdwsJSONWriter.Create(nil);
   try
      wr.BeginObject;
      wr.WriteName('Test');
      wr.EndObject;
   finally
      wr.Free;
   end;
end;

// JSONWriterNoName
//
procedure TdwsJSONTests.JSONWriterNoName;
var
   wr : TdwsJSONWriter;
begin
   wr:=TdwsJSONWriter.Create(nil);
   try
      wr.BeginObject;
      wr.WriteInteger(123);
      wr.EndObject;
   finally
      wr.Free;
   end;
end;

// JSONIntegerArrayBadStart
//
procedure TdwsJSONTests.JSONIntegerArrayBadStart;
var
   state : TdwsJSONParserState;
begin
   state := TdwsJSONParserState.Create(' bug');
   try
      state.ParseIntegerArray(nil);
   finally
      state.Free;
   end;
end;

// JSONIntegerArrayBadChar
//
procedure TdwsJSONTests.JSONIntegerArrayBadChar;
var
   state : TdwsJSONParserState;
begin
   state := TdwsJSONParserState.Create('[bug');
   try
      state.ParseIntegerArray(nil);
   finally
      state.Free;
   end;
end;

// JSONIntegerArrayBadComma
//
procedure TdwsJSONTests.JSONIntegerArrayBadComma;
var
   state : TdwsJSONParserState;
   dest : TSimpleInt64List;
begin
   state := TdwsJSONParserState.Create('[1,2 bug');
   dest := TSimpleInt64List.Create;
   try
      state.ParseIntegerArray(dest);
   finally
      dest.Free;
      state.Free;
   end;
end;

// JSONNumberArrayBadStart
//
procedure TdwsJSONTests.JSONNumberArrayBadStart;
var
   state : TdwsJSONParserState;
begin
   state := TdwsJSONParserState.Create(' bug');
   try
      state.ParseNumberArray(nil);
   finally
      state.Free;
   end;
end;

// JSONNumberArrayBadChar
//
procedure TdwsJSONTests.JSONNumberArrayBadChar;
var
   state : TdwsJSONParserState;
begin
   state := TdwsJSONParserState.Create('[bug');
   try
      state.ParseNumberArray(nil);
   finally
      state.Free;
   end;
end;

// JSONNumberArrayBadComma
//
procedure TdwsJSONTests.JSONNumberArrayBadComma;
var
   state : TdwsJSONParserState;
   dest : TSimpleDoubleList;
begin
   state := TdwsJSONParserState.Create('[1,2 bug');
   dest := TSimpleDoubleList.Create;
   try
      state.ParseNumberArray(dest);
   finally
      dest.Free;
      state.Free;
   end;
end;

// JSONStringArrayBadStart
//
procedure TdwsJSONTests.JSONStringArrayBadStart;
var
   state : TdwsJSONParserState;
begin
   state := TdwsJSONParserState.Create(' bug');
   try
      state.ParseStringArray(nil);
   finally
      state.Free;
   end;
end;

// JSONStringArrayBadChar
//
procedure TdwsJSONTests.JSONStringArrayBadChar;
var
   state : TdwsJSONParserState;
begin
   state := TdwsJSONParserState.Create('[bug');
   try
      state.ParseStringArray(nil);
   finally
      state.Free;
   end;
end;

// JSONStringArrayBadComma
//
procedure TdwsJSONTests.JSONStringArrayBadComma;
var
   state : TdwsJSONParserState;
   dest : TStringList;
begin
   state := TdwsJSONParserState.Create('["1","2" bug');
   dest := TStringList.Create;
   try
      state.ParseStringArray(dest);
   finally
      dest.Free;
      state.Free;
   end;
end;

// CompareStringArray
//
function TdwsJSONTests.CompareStringArray(v1, v2 : TdwsJSONValue) : Integer;
begin
   Result:=UnicodeCompareText(v1.AsString, v2.AsString);
end;

// JSONPathFailEmpty
//
procedure TdwsJSONTests.JSONPathFailEmpty;
begin
   TdwsJSONPathQuery.Create('');
end;

// JSONPathFailSyntax
//
procedure TdwsJSONTests.JSONPathFailSyntax;
begin
   TdwsJSONPathQuery.Create('!');
end;

// JSONPathFailIndex
//
procedure TdwsJSONTests.JSONPathFailIndex;
begin
   TdwsJSONPathQuery.Create('.bug[=]');
end;

// JSONPathFailIndexUnfinished
//
procedure TdwsJSONTests.JSONPathFailIndexUnfinished;
begin
   TdwsJSONPathQuery.Create('.bug[ 1 ');
end;

// JSONPathFailProperty
//
procedure TdwsJSONTests.JSONPathFailProperty;
begin
   TdwsJSONPathQuery.Create('.=');
end;

// JSONPathFailPropertyUnfinished
//
procedure TdwsJSONTests.JSONPathFailPropertyUnfinished;
begin
   TdwsJSONPathQuery.Create('.');
end;

// JSONPathFailDeepPropertyUnfinished
//
procedure TdwsJSONTests.JSONPathFailDeepPropertyUnfinished;
begin
   TdwsJSONPathQuery.Create('..');
end;

// JSONEmptyObject
//
procedure TdwsJSONTests.JSONEmptyObject;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"empty":{}}');

   Check(json['empty'].ValueType=jvtObject, 'check empty value type');
   CheckEquals(0, json['empty'].ElementCount, 'check empty element count');

   json.Free;

   CheckException(JSONExtraComma, EdwsJSONParseError, 'extra comma');
end;

// JSONEmptyArray
//
procedure TdwsJSONTests.JSONEmptyArray;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"empty":[],"nested":[[]]}');

   Check(json['empty'].ValueType=jvtArray, 'check empty value type');
   CheckEquals(0, json['empty'].ElementCount, 'check empty element count');
   Check(json['nested'].ValueType=jvtArray, 'check nested value type');
   CheckEquals(1, json['nested'].ElementCount, 'check empty element count');

   CheckEquals('{"empty":[],"nested":[[]]}', json.ToString, 'roundtrip');

   json.Free;

   CheckException(JSONExtraComma, EdwsJSONParseError, 'extra comma');
end;

// JSONSpecialChars
//
procedure TdwsJSONTests.JSONSpecialChars;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"test":"\t\n\r\b\f\\"}');

   CheckEquals(#9#10#13#8#12'\', json['test'].Value.AsString, 'specials check');
   CheckEquals('"\t\n\r\b\f\\"', json['test'].ToString, 'specials toString');

   json['test'].Value.AsString:=#25#0'bug';
   CheckEquals('"\u0019"', json['test'].ToString, 'very specials');

   json.Free;
end;

// JSONLongNumber
//
procedure TdwsJSONTests.JSONLongNumber;
const
   c1e64 : Double = 1e64;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"test":1'+StringOfChar('0', 64)+'}');

   CheckEquals(c1e64, json['test'].Value.AsNumber, 'specials');

   json.Free;
end;

// JSONInvalidStuff
//
procedure TdwsJSONTests.JSONInvalidStuff;
begin
   CheckException(JSONInvalidChar, EdwsJSONParseError, '#25 char');
   CheckException(JSONInvalidUnicodeHexa, EdwsJSONParseError, 'unicode');
   CheckException(JSONInvalidEscaped, EdwsJSONParseError, 'escaped');
   CheckException(JSONInvalidStart, EdwsJSONParseError, 'start');
   CheckException(JSONInvalidTrue, EdwsJSONParseError, 'true');
   CheckException(JSONInvalidFalse, EdwsJSONParseError, 'false');
   CheckException(JSONInvalidNull, EdwsJSONParseError, 'null');
   CheckException(JSONInvalidImmediate, EdwsJSONParseError, 'immediate');
   CheckException(JSONUnterminatedString, EdwsJSONParseError, 'unterminated');
   CheckException(JSONInvalidNameValueSeparator, EdwsJSONParseError, 'separator');
   CheckException(JSONInvalidNameValueEnding, EdwsJSONParseError, 'ending');

   CheckException(JSONMissingElementValue, EdwsJSONParseError, 'missing element value');
end;

// JSONParseArrayInvalid
//
procedure TdwsJSONTests.JSONParseArrayInvalid;
begin
   CheckException(JSONIntegerArrayBadStart, EdwsJSONParseError, 'bad start i');
   CheckException(JSONIntegerArrayBadChar, EdwsJSONParseError, 'bad char i');
   CheckException(JSONIntegerArrayBadComma, EdwsJSONParseError, 'bad comma i');

   CheckException(JSONNumberArrayBadStart, EdwsJSONParseError, 'bad start n');
   CheckException(JSONNumberArrayBadChar, EdwsJSONParseError, 'bad char n');
   CheckException(JSONNumberArrayBadComma, EdwsJSONParseError, 'bad comma n');

   CheckException(JSONStringArrayBadStart, EdwsJSONParseError, 'bad start s');
   CheckException(JSONStringArrayBadChar, EdwsJSONParseError, 'bad char s');
   CheckException(JSONStringArrayBadComma, EdwsJSONParseError, 'bad comma s');
end;

// NestedArrays
//
procedure TdwsJSONTests.NestedArrays;
var
   a : TdwsJSONArray;
begin
   a:=TdwsJSONArray.Create;
   try
      a.Add(TdwsJSONArray.Create);
      a.AddArray;
      CheckEquals('[[],[]]', a.ToString);
   finally
      a.Free;
   end;
end;

// MultipleElementsWithSameName
//
procedure TdwsJSONTests.MultipleElementsWithSameName;
const
   cAlternatives : array [1..8] of String = (
      '1', 'true', 'null', '"a"',
      '{"b":2}', '[1,2]',
      '{"b":[1,2]}', '[{"b":1}]'
      );
var
   i, j : Integer;
   json : TdwsJSONValue;
begin
   for i:=Low(cAlternatives) to High(cAlternatives) do begin
      for j:=Low(cAlternatives) to High(cAlternatives) do begin
         json:=TdwsJSONValue.ParseString('{"a":'+cAlternatives[i]+',"a":'+cAlternatives[j]+'}', jdoOverwrite);
         try
            CheckEquals('{"a":'+cAlternatives[j]+'}', json.ToString);
         finally
            json.Free;
         end;
      end;
   end;
end;

// SetItemTest
//
procedure TdwsJSONTests.SetItemTest;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"hello":1,"world":{}}');

   json.Items['hello']:=TdwsJSONValue.ParseString('[1, 2]');

   CheckEquals('{"hello":[1,2],"world":{}}', json.ToString, 'replace 1 with [1,2]');

   json.Items['world']:=TdwsJSONImmediate.FromVariant(3);

   CheckEquals('{"hello":[1,2],"world":3}', json.ToString, 'replace {} with 3');

   json.Items['world']:=nil;

   CheckEquals('{"hello":[1,2]}', json.ToString, 'delete world');

   json.Items['hello'].Items['1']:=nil;

   CheckEquals('{"hello":[1]}', json.ToString, 'delete 2');

   json.Free;
end;

// SubItemFree
//
procedure TdwsJSONTests.SubItemFree;
var
   obj : TdwsJSONObject;
   key : TdwsJSONValue;
begin
   obj:=TdwsJSONObject.ParseString('{"Key": "value"}') as TdwsJSONObject;
   key:=obj['Key'];
   key.Free;
   obj.Free;

   key:=TdwsJSONValue.ParseString('{"Key": "value"}');
   key.Items['Key'].Free;
   key.Free;

   key:=TdwsJSONValue.ParseString('["Key", "value"]');
   key.Elements[0].Free;
   key.Free;
end;

// ISO8601Test
//
procedure TdwsJSONTests.ISO8601Test;
var
   wr : TdwsJSONWriter;
begin
   wr:=TdwsJSONWriter.Create(nil);
   try
      wr.BeginObject;
      wr.WriteName('Date');
      wr.WriteDate(EncodeDate(2080, 12, 24));
      wr.WriteName('DateTime');
      wr.WriteDate(EncodeDate(2080, 12, 24)+EncodeTime(15, 30, 45, 450));
      wr.EndObject;
      CheckEquals('{"Date":"2080-12-24","DateTime":"2080-12-24T15:30:45"}',
                  wr.ToString);
   finally
      wr.Free;
   end;
end;

// DeleteAdd
//
procedure TdwsJSONTests.DeleteAdd;
var
   obj : TdwsJSONObject;
begin
   obj:=TdwsJSONObject.Create;
   try
      obj.AddValue('test');
      obj.AddValue('test');
      CheckEquals('{"test":null,"test":null}', obj.ToString);
      obj.MergeDuplicates;
      CheckEquals('{"test":null}', obj.ToString);
      obj.AddValue('test');
      CheckEquals('{"test":null,"test":null}', obj.ToString);
      obj.MergeDuplicates;
      CheckEquals('{"test":null}', obj.ToString);
   finally
      obj.Free;
   end;
end;

// WriterErrors
//
procedure TdwsJSONTests.WriterErrors;
begin
   CheckException(JSONWriterNoValue, EdwsJSONWriterError, 'no value');
   CheckException(JSONWriterNoName, EdwsJSONWriterError, 'no name');
end;

// ArrayTest
//
procedure TdwsJSONTests.ArrayTest;
var
   jsonArray : TdwsJSONArray;
begin
   jsonArray :=TdwsJSONArray.Create;
   try
      jsonArray.Add(TdwsJSONValue.ParseString('true'));
      jsonArray.Add(TdwsJSONValue.ParseString('false'));
      jsonArray.Add(TdwsJSONValue.ParseString('null'));
      jsonArray.Add(TdwsJSONValue.ParseString('"test"'));
      jsonArray.Add(TdwsJSONValue.ParseString('{"foo":"bar"}'));
      jsonArray.Add(TdwsJSONValue.ParseString('true'));
      jsonArray.Add(TdwsJSONValue.ParseString('false'));
      jsonArray.Add(TdwsJSONValue.ParseString('false'));
      jsonArray.Add(TdwsJSONValue.ParseString('false'));
      CheckEquals('[true,false,null,"test",{"foo":"bar"},true,false,false,false]', jsonArray.ToString, '1st');
      jsonArray.Clear;
      jsonArray.Add(TdwsJSONValue.ParseString('true'));
      jsonArray.Add(TdwsJSONValue.ParseString('false'));
      jsonArray.Add(TdwsJSONValue.ParseString('null'));
      jsonArray.Add(TdwsJSONValue.ParseString('"test"'));
      jsonArray.Add(TdwsJSONValue.ParseString('{"foo":"bar"}'));
      jsonArray.Add(TdwsJSONValue.ParseString('true'));
      jsonArray.Add(TdwsJSONValue.ParseString('false'));
      jsonArray.Add(TdwsJSONValue.ParseString('true'));
      jsonArray.Add(TdwsJSONValue.ParseString('false'));
      CheckEquals('[true,false,null,"test",{"foo":"bar"},true,false,true,false]', jsonArray.ToString, '2nd');
   finally
      jsonArray.Free;
   end;
end;

// DefaultValues
//
procedure TdwsJSONTests.DefaultValues;
var
   json : TdwsJSONValue;
begin
   json := nil;

   CheckEquals('undefined', json.AsString, 'str');
   CheckEquals(False, json.IsNull, 'null');
   CheckEquals(False, json.IsDefined, 'defined');
   CheckEquals(False, json.AsBoolean, 'bool');
   Check(IsNan(json.AsNumber), 'num');
   CheckEquals(True, json.IsNaN, 'nan');
   CheckEquals(0, json.AsInteger, 'int');
end;

// RepositionInArray
//
procedure TdwsJSONTests.RepositionInArray;
var
   a, b : TdwsJSONValue;
begin
   a:=TdwsJSONArray.Create;
   try
      b:=TdwsJSONObject.Create;
      a.Elements[0]:=b;
      CheckEquals('[{}]', a.ToString);

      a.Elements[1]:=b;
      CheckEquals('[null,{}]', a.ToString);
   finally
      a.Free;
   end;
end;

// CloneAndDetach
//
procedure TdwsJSONTests.CloneAndDetach;
var
   a, b, c : TdwsJSONValue;
begin
   a:=TdwsJSONObject.ParseString('{"hello":"world"}');
   try
      b:=a.Clone;
      try
         a.Elements[0].AsString:='WORLD';
         CheckEquals('{"hello":"WORLD"}', a.ToString);
         CheckEquals('{"hello":"world"}', b.ToString);

         c:=b.Elements[0];
         c.Detach;
         try
            CheckEquals('{}', b.ToString);
            CheckEquals('"world"', c.ToString);
         finally
            c.Free;
         end;
      finally
         b.Free;
      end;
   finally
      a.Free;
   end;
end;

// SortArray
//
procedure TdwsJSONTests.SortArray;
var
   a : TdwsJSONValue;
begin
   a:=TdwsJSONObject.ParseString('["hello", "world", "some", "alpha", "stuff"]');
   try
      (a as TdwsJSONArray).Sort(CompareStringArray);
      CheckEquals('["alpha","hello","some","stuff","world"]', a.ToString);
   finally
      a.Free;
   end;
end;

// EnumerateNil
//
procedure TdwsJSONTests.EnumerateNil;
var
   v, i : TdwsJSONValue;
   n : Integer;
begin
   n:=0;
   v:=nil;
   for i in v do begin
      Inc(n);
      Assert(i=nil); // dummy test to shut up the compiler warning
   end;
   CheckEquals(0, n, 'nil');

   v:=TdwsJSONImmediate.Create;
   try
      v.AsString:='hello';
      for i in v do begin
         Inc(n);
         Assert(i=nil); // dummy test to shut up the compiler warning
      end;
      CheckEquals(0, n, 'immediate');
   finally
      v.Free;
   end;
end;

// EnumerateArray
//
procedure TdwsJSONTests.EnumerateArray;
var
   v, i : TdwsJSONValue;
   s : String;
begin
   v:=TdwsJSONValue.ParseString('[1,"a",true]');
   try
      for i in v do begin
         s:=s+';'+i.AsString;
      end;
      CheckEquals(';1;a;true', s);
   finally
      v.Free;
   end;
end;

// JSONPathBasic
//
procedure TdwsJSONTests.JSONPathBasic;
var
   js : TdwsJSONValue;

   procedure CheckPath(const query, expected : String);
   var
      list : TdwsJSONValueList;
   begin
      list := JSONPath.Query(query, js);
      try
         CheckEquals(expected, list.ToString, query);
      finally
         list.Free;
      end;
   end;

begin
   js := TdwsJSONValue.ParseString(cJSONbooks);
   try

      CheckPath('.books.author',
                '[{"name":"Robert C. Martin"},{"name":"Nicholas C. Zakas"},{"name":"Douglas Crockford"}]');

      CheckPath('.books.author.name',
                '["Robert C. Martin","Nicholas C. Zakas","Douglas Crockford"]');

      CheckPath('.books..name',
                '["Robert C. Martin","Nicholas C. Zakas","Douglas Crockford"]');

      CheckPath('.id',
                '[]');

      CheckPath('..id',
                '[1,2,3]');

      CheckPath('.*.id',
                '[1,2,3]');

      CheckPath('.books.1.price',
                '[10]');

      CheckPath('.books[1].price',
                '[10]');

      CheckPath('.books[-1]..name',
                '["Douglas Crockford"]');

      CheckPath('.books[*].price',
                '[17.96,10,15.67]');

      CheckPath('.books[].id',
                '[1,2,3]');

      CheckPath('.books..title',
                '["Clean Code","Maintainable JavaScript","JavaScript: The Good Parts"]');

      CheckPath('..leaf',
                '[[11,22]]');
      CheckPath('..leaf[1]',
                '[22]');
      CheckPath('..leaf.."0"',
                '[11]');
      CheckPath('.leaf..nope',
                '[]');

      CheckPath('.nums[1]',
                '["one"]');
      CheckPath('.nums.2',
                '["two"]');
      CheckPath('.nums."3"',
                '[3]');

      CheckPath(' .books [ 2 ] .title ',
                '["JavaScript: The Good Parts"]');
   finally
      js.Free;
   end;
end;

// JSONPathFails
//
procedure TdwsJSONTests.JSONPathFails;
begin
   CheckException(JSONPathFailEmpty, EJSONPathException);
   CheckException(JSONPathFailSyntax, EJSONPathException);
   CheckException(JSONPathFailIndex, EJSONPathException);
   CheckException(JSONPathFailIndexUnfinished, EJSONPathException);
   CheckException(JSONPathFailProperty, EJSONPathException);
   CheckException(JSONPathFailPropertyUnfinished, EJSONPathException);
   CheckException(JSONPathFailDeepPropertyUnfinished, EJSONPathException);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('JSON', TdwsJSONTests);

end.
