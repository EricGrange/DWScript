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
unit UdwsXPlatformTests;

interface

uses Classes, SysUtils, Math, Types, dwsXPlatformTests, dwsUtils, dwsXPlatform;

type

   TdwsXPlatformTests = class (TTestCase)
      protected
         procedure SetUp; override;
         procedure TearDown; override;
      published
         procedure DecimalPointTest;
         procedure CollectFilesTest;
         procedure DateTimeConversionTest;
         procedure MillisecondsConversionTest;
         procedure UnicodeLowerAndUpperCaseTest;
         procedure UnicodeCompareTest;
         procedure RawBytesStringTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsXPlatformTests ------------------
// ------------------


// SetUp
//
procedure TdwsXPlatformTests.SetUp;
begin

end;

// TearDown
//
procedure TdwsXPlatformTests.TearDown;
begin

end;

// DateTimeConversionTest
//
procedure TdwsXPlatformTests.CollectFilesTest;
var
   Files : TStringList;
begin
   Files:=TStringList.Create;
   try
      CollectFiles(ExtractFilePath(ParamStr(0))+'Data'+PathDelim, '*.txt', Files);
      CheckEquals(3, Files.Count);
   finally
      Files.Free;
   end;
end;

procedure TdwsXPlatformTests.DateTimeConversionTest;
var
   CurrentDateTime : TDateTime;
begin
   CurrentDateTime := Now;
   CheckEquals(CurrentDateTime, UTCDateTimeToLocalDateTime(LocalDateTimeToUTCDateTime(CurrentDateTime)));
end;


// MillisecondsConversionTest
//
procedure TdwsXPlatformTests.DecimalPointTest;
var
   oldDecimalSeparator : Char;
begin
   oldDecimalSeparator := GetDecimalSeparator;
   try
      SetDecimalSeparator(',');
      CheckEquals(',', GetDecimalSeparator);
      SetDecimalSeparator('.');
      CheckEquals('.', GetDecimalSeparator);
   finally
      SetDecimalSeparator(oldDecimalSeparator);
   end;
end;

procedure TdwsXPlatformTests.MillisecondsConversionTest;
var
   currentMilliseconds : Int64;
begin
   currentMilliseconds := GetSystemMilliseconds;
   CheckTrue(Abs(currentMilliseconds - UnixTimeToSystemMilliseconds(SystemMillisecondsToUnixTime(currentMilliseconds))) < 1000);
end;

procedure TdwsXPlatformTests.RawBytesStringTest;
var
  Bytes: TBytes;
const
  cTestString = 'Hello World!';
begin
  Bytes := RawByteStringToBytes(RawByteString(cTestString));
  CheckEquals(cTestString, BytesToRawByteString(@Bytes[0], Length(Bytes)));
end;

// UnicodeLowerAndUpperCaseTest
//
procedure TdwsXPlatformTests.UnicodeCompareTest;
const
  CTestStrings : array [0 .. 5] of string = ('HeLlO WoRlD!', 'hElLo wOrLd!',
    'Héllo World!', 'AA', 'AAa', 'AB');
begin
   // test equal string with different cases
   CheckEquals(0, UnicodeCompareP(PWideChar(CTestStrings[0]),
      Length(CTestStrings[0]), PWideChar(CTestStrings[1]),
      Length(CTestStrings[1])));

   // test equal string with different cases (same length)
   CheckEquals(0, UnicodeCompareP(PWideChar(CTestStrings[0]),
      PWideChar(CTestStrings[1]), Max(Length(CTestStrings[0]),
      Length(CTestStrings[1]))));

   // test equal string with different cases but different length
   CheckEquals(1, UnicodeCompareP(PWideChar(CTestStrings[0]), Length(CTestStrings[0]),
      PWideChar(CTestStrings[1]), Length(CTestStrings[1]) - 2));

   // test unequal string with different cases (same length)
   CheckEquals(-1, UnicodeCompareP(PWideChar(CTestStrings[0]),
      PWideChar(CTestStrings[2]), Max(Length(CTestStrings[0]),
      Length(CTestStrings[2]))));
   CheckEquals(-1, UnicodeCompareP(PWideChar(CTestStrings[4]),
      Length(CTestStrings[4]), PWideChar(CTestStrings[5]),
      Length(CTestStrings[5])));
end;

procedure TdwsXPlatformTests.UnicodeLowerAndUpperCaseTest;
const
  TestStringUpperCaseBasic = '0123456789<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  TestStringUpperCaseSupplement = 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ';
  TestStringUpperCaseExtendedA =
    'ĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŹŻŽ';
  TestStringUpperCaseExtendedB = 'ƇƉƑƓƔƘǄǇǊǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮ';
  TestStringUpperCaseGreek = 'ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫϢ';
  TestStringUpperCaseCyrillic = 'ЁЂЃЄЅІЇЈЉЊЋЌЎЏАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';
  TestStringUpperCaseArmenian = 'ԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿՀՁՂՃՄՅՆՇՈՉՊՋՌՍՎՏՐՑՒՓՔՕ';
  TestStringUpperCaseExtendedAdditional =
    'ḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮḰḲḴḶḸḺḼḾṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾ';
  TestStringUpperCaseGreekExtended =
    'ἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹἺἻἼἽἾἿὈὉὊὋὌὍὙὛὝὟὨὩὪὫὬὭὮὯᾈᾉᾊᾋᾌᾍ';
  TestStringUpperCaseFullWidth =
    'ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ';
  TestStringLowerCaseBasic = '0123456789<=>abcdefghijklmnopqrstuvwxyz';
  TestStringLowerCaseSupplement = 'àáâãäåæçèéêëìíîïðñòóôõö';
  TestStringLowerCaseExtendedA =
    'āăąćĉċčďđēĕėęěĝğġģĥħĩīĭįĳĵķĺļľŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷźżž';
  TestStringLowerCaseExtendedB = 'ƈɖƒɠɣƙǆǉǌǎǐǒǔǖǘǚǜǟǡǣǥǧǩǫǭǯ';
  TestStringLowerCaseGreek = 'αβγδεζηθικλμνξοπρστυφχψωϊϋϣ';
  TestStringLowerCaseCyrillic = 'ёђѓєѕіїјљњћќўџабвгдежзийклмнопрстуфхцчшщъыьэюя';
  TestStringLowerCaseArmenian = 'աբգդեզէըթժիլխծկհձղճմյնշոչպջռսվտրցւփքօ';
  TestStringLowerCaseExtendedAdditional =
    'ḁḃḅḇḉḋḍḏḑḓḕḗḙḛḝḟḡḣḥḧḩḫḭḯḱḳḵḷḹḻḽḿṁṃṅṇṉṋṍṏṑṓṕṗṙṛṝṟṡṣṥṧṩṫṭṯṱṳṵṷṹṻṽṿ';
  TestStringLowerCaseGreekExtended =
    'ἀἁἂἃἄἅἆἇἐἑἒἓἔἕἠἡἢἣἤἥἦἧἰἱἲἳἴἵἶἷὀὁὂὃὄὅὑὓὕὗὠὡὢὣὤὥὦὧᾀᾁᾂᾃᾄᾅ';
  TestStringLowerCaseFullWidth =
    'ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ';

   procedure CheckLower(const expected, test : String);
   var
      buf : String;
   begin
      UnicodeLowerCase(test, buf);
      CheckEquals(expected, buf);
   end;

   procedure CheckUpper(const expected, test : String);
   var
      buf : String;
   begin
      UnicodeUpperCase(test, buf);
      CheckEquals(expected, buf);
   end;

begin
   CheckLower(TestStringLowerCaseBasic, TestStringUpperCaseBasic);
   CheckLower(TestStringLowerCaseSupplement, TestStringUpperCaseSupplement);
   CheckLower(TestStringLowerCaseExtendedA, TestStringUpperCaseExtendedA);
   CheckLower(TestStringLowerCaseExtendedB, TestStringUpperCaseExtendedB);
   CheckLower(TestStringLowerCaseGreek, TestStringUpperCaseGreek);
   CheckLower(TestStringLowerCaseCyrillic, TestStringUpperCaseCyrillic);
   CheckLower(TestStringLowerCaseArmenian, TestStringUpperCaseArmenian);
   CheckLower(TestStringLowerCaseExtendedAdditional, TestStringUpperCaseExtendedAdditional);
   CheckLower(TestStringLowerCaseGreekExtended, TestStringUpperCaseGreekExtended);
   CheckLower(TestStringLowerCaseFullWidth, TestStringUpperCaseFullWidth);

   CheckUpper(TestStringUpperCaseBasic, TestStringLowerCaseBasic);
   CheckUpper(TestStringUpperCaseSupplement, TestStringLowerCaseSupplement);
   CheckUpper(TestStringUpperCaseExtendedA, TestStringLowerCaseExtendedA);
   CheckUpper(TestStringUpperCaseExtendedB, TestStringLowerCaseExtendedB);
   CheckUpper(TestStringUpperCaseGreek, TestStringLowerCaseGreek);
   CheckUpper(TestStringUpperCaseCyrillic, TestStringLowerCaseCyrillic);
   CheckUpper(TestStringUpperCaseArmenian, TestStringLowerCaseArmenian);
   CheckUpper(TestStringUpperCaseExtendedAdditional, TestStringLowerCaseExtendedAdditional);
   CheckUpper(TestStringUpperCaseGreekExtended, TestStringLowerCaseGreekExtended);
   CheckUpper(TestStringUpperCaseFullWidth, TestStringLowerCaseFullWidth);
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('XPlatformTests', TdwsXPlatformTests);

end.
