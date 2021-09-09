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
unit UdwsWebUtilsTests;

interface

uses Classes, SysUtils, Math, dwsXPlatformTests, dwsWebUtils;

type

   TdwsWebUtilsTests = class (TTestCase)
      private

      protected

      published
         procedure URLEncodedEncoder;

         procedure ParseURLEncodedTest;
         procedure ParseMIMEHeaderValueTest;
         procedure ParseMultiPartFormDataTest;
         procedure HTMLAttributeTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsWebUtilsTests ------------------
// ------------------

// ParseURLEncodedTest
//
procedure TdwsWebUtilsTests.ParseURLEncodedTest;
var
   decoded : TStringList;
begin
   decoded:=TStringList.Create;
   try

      WebUtils.ParseURLEncoded('hello=world', decoded);
      CheckEquals('hello=world', decoded.CommaText);

      WebUtils.ParseURLEncoded('hell%64=+world', decoded);
      CheckEquals('hello=world,"helld= world"', decoded.CommaText);

      decoded.Clear;
      WebUtils.ParseURLEncoded('a=b&c=d', decoded);
      CheckEquals('a=b,c=d', decoded.CommaText);

      decoded.Clear;
      WebUtils.ParseURLEncoded('a=b;c=d', decoded);
      CheckEquals('a=b,c=d', decoded.CommaText);

      decoded.Clear;
      WebUtils.ParseURLEncoded('&a&c=d&', decoded);
      CheckEquals('a,c=d', decoded.CommaText);

      decoded.Clear;
      WebUtils.ParseURLEncoded('a+"%20c', decoded);
      CheckEquals('"a "" c"', decoded.CommaText);

   finally
      decoded.Free;
   end;
end;

// ParseMIMEHeaderValueTest
//
procedure TdwsWebUtilsTests.ParseMIMEHeaderValueTest;
var
   decoded : TStringList;
begin
   decoded:=TStringList.Create;
   try

      WebUtils.ParseMIMEHeaderValue('test', decoded);
      CheckEquals('test', decoded.CommaText);

      decoded.Clear;
      WebUtils.ParseMIMEHeaderValue('form-data; name=xml', decoded);
      CheckEquals('form-data,name=xml', decoded.CommaText);

      decoded.Clear;
      WebUtils.ParseMIMEHeaderValue('form-data; name="a,b"', decoded);
      CheckEquals('form-data,"name=a,b"', decoded.CommaText);

   finally
      decoded.Free;
   end;
end;

// ParseMultiPartFormDataTest
//
procedure TdwsWebUtilsTests.ParseMultiPartFormDataTest;
const
   cData = #13#10
      + '-----------------------------9051914041544843365972754266'#13#10
      + 'Content-Disposition: form-data; name="text"'#13#10
      + #13#10
      + 'text default'
      + #13#10'-----------------------------9051914041544843365972754266'#13#10
      + 'Content-Disposition: form-data; name="file1"; filename="a.txt"'#13#10
      + 'Content-Type: text/plain'#13#10
      + #13#10
      + 'Content of a.txt.'#13#10
      + #13#10'-----------------------------9051914041544843365972754266'#13#10
      + 'Content-Disposition: form-data; name="file2"; filename="a.html"'#13#10
      + 'Content-Type: text/html'#13#10
      + #13#10
      + '<!DOCTYPE html><title>Content of a.html.</title>'
      + #13#10'-----------------------------9051914041544843365972754266--'
      + #13#10;
var
   data : TIMIMEBodyParts;
begin
   WebUtils.ParseMultiPartFormData(cData, '-----------------------------9051914041544843365972754266', data);
   CheckEquals(3, Length(data));

   CheckEquals('text default', data[0].RawData);
   CheckEquals('form-data; name="text"', data[0].ContentDisposition);
   CheckEquals('', data[0].ContentType, '0.ContenType');
   CheckEquals('text', data[0].Name);

   CheckEquals('Content of a.txt.'#13#10, data[1].RawData);
   CheckEquals('form-data; name="file1"; filename="a.txt"', data[1].ContentDisposition);
   CheckEquals('text/plain', data[1].ContentType);
   CheckEquals('file1', data[1].Name);
   CheckEquals('a.txt', data[1].FileName);

   CheckEquals('<!DOCTYPE html><title>Content of a.html.</title>', data[2].RawData);
   CheckEquals('form-data; name="file2"; filename="a.html"', data[2].ContentDisposition);
   CheckEquals('text/html', data[2].ContentType);
   CheckEquals('file2', data[2].Name);
   CheckEquals('a.html', data[2].FileName);
end;

// HTMLAttributeTest
//
procedure TdwsWebUtilsTests.HTMLAttributeTest;
begin
   CheckEquals('', WebUtils.HTMLAttributeEncode(''), 'empty');
   CheckEquals('1', WebUtils.HTMLAttributeEncode('1'), '1');
   CheckEquals('ab', WebUtils.HTMLAttributeEncode('ab'), 'ab');
   CheckEquals('&#x09;&#x7E;', WebUtils.HTMLAttributeEncode(#9#126), '#9#126');
   CheckEquals('a&#32;c', WebUtils.HTMLAttributeEncode('a c'), 'a c');
   CheckEquals('a&#34;&#60;&#62;', WebUtils.HTMLAttributeEncode('a"<>'), 'a"<>');
   CheckEquals('a&#47;hj', WebUtils.HTMLAttributeEncode('a/hj'), 'a/hj');
end;

// URLEncodedEncoder
//
procedure TdwsWebUtilsTests.URLEncodedEncoder;
begin
   CheckEquals('', WebUtils.EncodeURLEncoded(''), 'empty');
   CheckEquals('a', WebUtils.EncodeURLEncoded('a'), 'a');
   CheckEquals('a%3D', WebUtils.EncodeURLEncoded('a='), 'a=');
   CheckEquals('%3D%3D%3D%3D%3D%3D', WebUtils.EncodeURLEncoded('======'), '======');
   CheckEquals('a%20b%22c', WebUtils.EncodeURLEncoded('a b"c'), 'a b"c');
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('UtilsTests', TdwsWebUtilsTests);

end.
