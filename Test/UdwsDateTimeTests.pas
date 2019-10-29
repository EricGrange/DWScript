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
   dwsXPlatformTests, dwsXPlatform, dwsWebUtils;

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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('DateTimeTests', TdwsDateTimeTests);

end.
