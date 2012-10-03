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

uses Classes, SysUtils, dwsXPlatformTests, dwsJSON;

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

      published
         procedure JSONTest;
         procedure ParseJSON;
         procedure AccessJSON;
         procedure JSONUnicodeLiteral;
         procedure JSONCRLF;
         procedure JSONQuote;
         procedure UndefinedJSON;
         procedure JSONEmptyObject;
         procedure JSONSpecialChars;
         procedure JSONLongNumber;
         procedure JSONInvalidStuff;
         procedure NestedArrays;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

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

// JSONSpecialChars
//
procedure TdwsJSONTests.JSONSpecialChars;
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseString('{"test":"\t\n\r\b\f"}');

   CheckEquals(#9#10#13#8#12, json['test'].Value.AsString, 'specials check');
   CheckEquals('"\t\n\r\b\f"', json['test'].ToString, 'specials toString');

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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('JSONTests', TdwsJSONTests);

end.
