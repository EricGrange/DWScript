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
unit UdwsDateTimeTests;

interface

uses
   Winapi.Windows,
   SysUtils, DateUtils,
   dwsXPlatformTests, dwsXPlatform, dwsWebUtils, dwsDateTime;

type

   TdwsDateTimeTests = class (TTestCase)
      private

      protected

      published
         procedure BasicInitAndZero;
         procedure UnixTime;
         procedure LocalVsUTC;
         procedure RFC822;
         procedure FileTime;
         procedure ParseDateTime;
         procedure ParseDateTimeMonkey;
         procedure ParseDateNames;
         procedure ParseDateLitterals;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsDateTimeTests ------------------
// ------------------

// BasicInitAndZero
//
procedure TdwsDateTimeTests.BasicInitAndZero;
var
   dt : TDateTime;
   t : TdwsDateTime;
begin
   t.Clear;
   CheckTrue(t.IsZero, 'clear');

   dt := Now;
   t := TdwsDateTime.Now;
   CheckTrue((t.AsLocalDateTime - dt) <= 1/864e5, 'init from now');
   t := TdwsDateTime.FromLocalDateTime(dt);
   CheckTrue((t.AsLocalDateTime - dt) <= 1/864e5, 'init from localtime');

   CheckFalse(t.IsZero, 'not clear');

   t.Clear;
   CheckTrue(t.IsZero, 'clear again');
end;

// UnixTime
//
procedure TdwsDateTimeTests.UnixTime;
var
   t : TdwsDateTime;
begin
   t.AsUnixTime := 1234567890;
   CheckEquals(1234567890, t.AsUnixTime, 'in out');
   CheckEquals('2009-02-13 23:31:30.000', FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', t.AsUTCDateTime));
   t.IncMilliseconds(123);
   CheckEquals('2009-02-13 23:31:30.123', FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', t.AsUTCDateTime));
   CheckEquals(1234567890, t.AsUnixTime, 'rounding');
end;

// LocalVsUTC
//
procedure TdwsDateTimeTests.LocalVsUTC;
var
   t : TdwsDateTime;
   dt : TDateTime;
   dtu : Int64;
begin
   dt := Now;
   t.AsLocalDateTime := dt;

   CheckEquals(DateTimeToStr(dt), DateTimeToStr(t.AsLocalDateTime), 'local');

   dtu := DateTimeToUnix(dt, False);
   CheckEquals(dtu, t.AsUnixTime, 'local to unix');

   t.AsUTCDateTime := dt;
   dtu := DateTimeToUnix(dt, True);
   CheckEquals(dtu, t.AsUnixTime, 'ut to unix');
end;

// RFC822
//
procedure TdwsDateTimeTests.RFC822;
var
   t : TdwsDateTime;
begin
   t.AsUnixTime := 1234567890;
   CheckEquals('Fri, 13 Feb 2009 23:31:30 GMT', WebUtils.DateTimeToRFC822(t));
end;

// FileTime
//
procedure TdwsDateTimeTests.FileTime;
var
   t : TdwsDateTime;
   ft : TFileTime;
   st : TSystemTime;
   dosDate, dosTime : Word;
begin
   t.AsUnixTime := 1234567890;
   t.IncMilliseconds(123);

   ft := t.AsFileTime;
   FileTimeToSystemTime(ft, st);
   CheckEquals(2009, st.wYear);
   CheckEquals(2, st.wMonth);
   CheckEquals(13, st.wDay);
   CheckEquals(23, st.wHour);
   CheckEquals(31, st.wMinute);
   CheckEquals(30, st.wSecond);
   CheckEquals(123, st.wMilliseconds);

   FileTimeToDosDateTime(ft, dosDate, dosTime);
   CheckEquals((Cardinal(dosDate) shl 16) or dosTime, t.AsDosDateTime);

   t.Clear;
   t.AsFileTime := ft;
   CheckEquals(1234567890123, t.Value);
end;

// ParseDateTime
//
procedure TdwsDateTimeTests.ParseDateTime;
var
   fmt : TdwsFormatSettings;
   dt : Double;
begin
   fmt := TdwsFormatSettings.Create;
   try
      CheckFalse(fmt.TryStrToDateTime('dd mmmm yyyy', '18 Dec 2019', dt, tzUTC), 'parse 1 fail');
      CheckTrue(fmt.TryStrToDateTime('dd mmm yyyy', '18 Dec 2019', dt, tzUTC), 'parse 1 pass');
      CheckEquals('18.12.2019 00:00:00', fmt.FormatDateTime('dd.mm.yyyy hh:nn:ss', dt, tzUTC), 'format 1');

      CheckFalse(fmt.TryStrToDateTime('m/d/yyyy h:nn:ss ampm', '1/2/2021 1:30:40 mm', dt, tzUTC), 'parse 2 fail');
      CheckTrue(fmt.TryStrToDateTime('m/d/yyyy h:nn:ss ampm', '1/2/2021 1:30:40 am', dt, tzUTC), 'parse 2 pass');
      CheckEquals('02.01.2021 01:30:40', fmt.FormatDateTime('dd.mm.yyyy hh:nn:ss', dt, tzUTC), 'format 2');

      CheckTrue(fmt.TryStrToDateTime('m/d/yyyy h:nn:ss ampm', '1/2/2021 1:30:40 PM', dt, tzUTC), 'parse 4 pass');
      CheckEquals('02.01.2021 01:30:40 PM', fmt.FormatDateTime('dd.mm.yyyy hh:nn:ss ampm', dt, tzUTC), 'format 3');

      CheckTrue(fmt.TryStrToDateTime('h:m:s ampm d yyyy m', '12:22:33 pm 4 2020 5', dt, tzUTC), 'parse 5 pass');
      CheckEquals('05.05.2020 00:22:33', fmt.FormatDateTime('dd.mm.yyyy hh:nn:ss', dt, tzUTC), 'format 4');

      CheckTrue(fmt.TryStrToDateTime('_yyyy_mm_dd_', '_2020_01_02_', dt, tzLocal), 'litterals pass');
      CheckFalse(fmt.TryStrToDateTime('_yyyy_mm_dd_', '-2020_01_02_', dt, tzLocal), 'litterals fail head 1');
      CheckFalse(fmt.TryStrToDateTime('_yyyy_mm_dd_', '2020_01_02_', dt, tzLocal), 'litterals fail head 2');
      CheckFalse(fmt.TryStrToDateTime('_yyyy_mm_dd_', '_2020-01_02_', dt, tzLocal), 'litterals fail mid 1');
      CheckFalse(fmt.TryStrToDateTime('_yyyy_mm_dd_', '_202001_02_', dt, tzLocal), 'litterals fail mid 2');
      CheckFalse(fmt.TryStrToDateTime('_yyyy_mm_dd_', '_2020_01_02-', dt, tzLocal), 'litterals fail tail 1');
      CheckFalse(fmt.TryStrToDateTime('_yyyy_mm_dd_', '_2020_01_02', dt, tzLocal), 'litterals fail tail 2');

      CheckTrue(fmt.TryStrToDateTime('dd mmm yy', '18 dec 19', dt, tzUTC), 'two digits year');
      CheckEquals('18.12.19', fmt.FormatDateTime('dd.mm.yy', dt, tzUTC), 'format two digits');
      CheckTrue(fmt.TryStrToDateTime('d m yy', '8 12 2019', dt, tzUTC), 'two digits year with 4 actual');
      CheckEquals('08.12.19', fmt.FormatDateTime('dd.mm.yy', dt, tzUTC), 'format two digits bis');
      CheckTrue(fmt.TryStrToDateTime('dd mmm yy', '12 jan 99', dt, tzUTC), 'two digits year wrap');
      CheckEquals('12.1.1999', fmt.FormatDateTime('dd.m.yyyy', dt, tzUTC), 'format four digits wrap');
      CheckEquals('12.1.99', fmt.FormatDateTime('dd.m.yy', dt, tzUTC), 'format two digits wrap');
   finally
      fmt.Free;
   end;
end;

// ParseDateTimeMonkey
//
procedure TdwsDateTimeTests.ParseDateTimeMonkey;
var
   fmt : TdwsFormatSettings;
   ref, mutated : String;
   i : Integer;
   dt : Double;
begin
   fmt := TdwsFormatSettings.Create;
   try
      ref := '12:34:56 21/05/1987';
      for i := 1 to Length(ref) do begin
         mutated := ref;
         mutated[i] := '_';
         CheckFalse(fmt.TryStrToDateTime('hh:mm:ss dd/mm/yyyy', mutated, dt, tzLocal), mutated);
         mutated := ref;
         Delete(mutated, i, 1);
         CheckFalse(fmt.TryStrToDateTime('hh:mm:ss dd/mm/yyyy', mutated, dt, tzLocal), mutated);
         mutated := ref;
         Insert('_', mutated, i);
         CheckFalse(fmt.TryStrToDateTime('hh:mm:ss dd/mm/yyyy', mutated, dt, tzLocal), mutated);
      end;
   finally
      fmt.Free;
   end;
end;

// ParseDateNames
//
procedure TdwsDateTimeTests.ParseDateNames;
var
   fmt : TdwsFormatSettings;
   dt, ref : Double;
   s : String;

   procedure CheckFmt(const f : String);
   begin
      s := fmt.FormatDateTime(f, ref, tzLocal);
      CheckTrue(fmt.TryStrToDateTime(f, s, dt, tzLocal), f);
      CheckEquals(ref, dt, f);
   end;

begin
   fmt := TdwsFormatSettings.Create;
   try
      ref := EncodeDateTime(2021, 11, 1, 12, 23, 45, 678);
      CheckFmt('ddd dd mmm yy hh.nn.ss.zzz ');
      CheckFmt('d dddd mmmm yyyy h.m.s z');
      CheckFmt('DDMMYYYYHHMMSSZZZ');
   finally
      fmt.Free;
   end;
end;

// ParseDateLitterals
//
procedure TdwsDateTimeTests.ParseDateLitterals;
var
   fmt : TdwsFormatSettings;
   dt, ref : Double;
   s : String;

   procedure CheckFmt(const f : String);
   begin
      s := fmt.FormatDateTime(f, ref, tzLocal);
      CheckTrue(fmt.TryStrToDateTime(f, s, dt, tzLocal), f);
      CheckEquals(ref, dt, f);
   end;

begin
   ref := EncodeDateTime(2021, 11, 1, 12, 23, 45, 678);
   CheckEquals('12h 23mn', fmt.FormatDateTime('hh"h" mm"mn"', ref, tzLocal));
   CheckEquals('12hr 23''', fmt.FormatDateTime('h''hr'' m"''"', ref, tzLocal));

   CheckTrue(fmt.TryStrToDateTime('"""z"dd mm yy', 'z01 02 03', dt, tzLocal));
   CheckEquals(EncodeDate(2003, 2, 1), dt);

   CheckFalse(fmt.TryStrToDateTime('"""z"dd mm yy', 'y01 02 03', dt, tzLocal));

   CheckTrue(fmt.TryStrToDateTime('dd mm yy', '01 02 03', dt, tzLocal));
   CheckTrue(fmt.TryStrToDateTime('dd mm yy""', '01 02 03', dt, tzLocal));
   CheckFalse(fmt.TryStrToDateTime('dd mm yy"', '01 02 03', dt, tzLocal));

   fmt := TdwsFormatSettings.Create;
   try
      CheckFmt('d"d" m"m" yyyy"y" hh"h" mm"mn" ss"sec" zzz');
      CheckFmt('""dd''''mm""yyyyHHnn''''SS""zzz');
   finally
      fmt.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('DateTimeTests', TdwsDateTimeTests);

end.
