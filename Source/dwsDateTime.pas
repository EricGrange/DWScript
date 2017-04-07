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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsDateTime;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsXPlatform;

type

   // Delphi TDateTime is internally in LocalTime
   // tz UTC option is applied on I/O
   //    for datetime -> str, UTC means it will return the UTC time
   //    for str -> datetime, UTC means the str will be interpreted as UTC and converted to local

   TdwsTimeZone = (tzDefault, tzLocal, tzUTC);

   TdwsFormatSettings = class
      public
         Settings : TFormatSettings;
         TimeZone : TdwsTimeZone;

         constructor Create;

         function FormatDateTime(const fmt : String; dt : Double; tz : TdwsTimeZone) : String;
         function DateTimeToStr(const dt : TDateTime; tz : TdwsTimeZone) : String;
         function DateToStr(const dt : TDateTime; tz : TdwsTimeZone) : String;
         function TimeToStr(const dt : TDateTime; tz : TdwsTimeZone) : String;

         function TryStrToDateTime(const fmt : String; const str : String; var dt : Double; tz : TdwsTimeZone) : Boolean; overload;
         function TryStrToDateTime(const str : String; var dt : Double; tz : TdwsTimeZone) : Boolean; overload;
         function TryStrToDate(const str : String; var dt : Double; tz : TdwsTimeZone) : Boolean;
         function TryStrToTime(const str : String; var dt : Double; tz : TdwsTimeZone) : Boolean;

         function TryEncodeDate(y, m, d : Integer; tz : TdwsTimeZone; var dt : Double) : Boolean;
         function EncodeDate(y, m, d : Integer; tz : TdwsTimeZone) : Double;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsFormatSettings ------------------
// ------------------

// Create
//
constructor TdwsFormatSettings.Create;
begin
   inherited;
   InitializeWithDefaultFormatSettings(Self.Settings);
   TimeZone:=tzLocal;
end;

// FormatDateTime
//
function TdwsFormatSettings.FormatDateTime(const fmt : String; dt : Double; tz : TdwsTimeZone) : String;
begin
   if (dt<-693592) or (dt>2146790052) then
      raise EConvertError.Create('Invalid date/time');
   if tz = tzDefault then
      tz := TimeZone;
   if tz = tzUTC then
      dt := UTCDateTimeToLocalDateTime(dt);
   Result:=SysUtils.FormatDateTime(fmt, dt, Settings);
end;

// DateTimeToStr
//
function TdwsFormatSettings.DateTimeToStr(const dt : TDateTime; tz : TdwsTimeZone) : String;
begin
   Result:=FormatDateTime(Settings.ShortDateFormat+' '+Settings.LongTimeFormat, dt, tz);
end;

// DateToStr
//
function TdwsFormatSettings.DateToStr(const dt : TDateTime; tz : TdwsTimeZone) : String;
begin
   Result:=FormatDateTime(Settings.ShortDateFormat, dt, tz);
end;

// TimeToStr
//
function TdwsFormatSettings.TimeToStr(const dt : TDateTime; tz : TdwsTimeZone) : String;
begin
   Result:=FormatDateTime(Settings.LongTimeFormat, dt, tz);
end;

// TryStrToDateTime
//
// Clean Room implementation based strictly on the format String
function TdwsFormatSettings.TryStrToDateTime(
      const fmt : String; const str : String; var dt : Double; tz : TdwsTimeZone) : Boolean;
var
   year, month, day, hours, minutes, seconds, msec : Integer;
   i, j, p, value, fmtLength, digit : Integer;
   c : Char;
   tok, litteral : String;
   dth : Double;
   match, previousWasHour, hourToken : Boolean;

   procedure GrabDigits(nbDigits : Integer);
   begin
      while (nbDigits>0) and (p<=Length(str)) do begin
         Dec(nbDigits);
         case str[p] of
            '0'..'9' : begin
               value:=value*10+Ord(str[p])-Ord('0');
               Inc(p);
            end;
         else
            break;
         end;
      end;
   end;


begin
   Result:=False;

   year:=0;
   month:=0;
   day:=0;
   hours:=0;
   minutes:=0;
   seconds:=0;
   msec:=0;

   fmtLength:=Length(fmt);
   i:=1;
   p:=1;
   previousWasHour:=False;
   while i<=fmtLength do begin
      c:=fmt[i];
      tok:='';
      value:=0;
      while (i<=fmtLength) and (fmt[i]=c) do begin
         case c of
            'A'..'Z' : tok:=tok+Chr(Ord(c)+(Ord('a')-Ord('A')));
         else
            tok:=tok+c;
         end;
         if p>Length(str) then Exit;
         digit:=Ord(str[p])-Ord('0');
         if (Cardinal(digit)<10) and (value>=0) then
            value:=value*10+digit
         else value:=-1;
         Inc(i);
         Inc(p);
      end;

      // variable length fields
      case Length(tok) of
         1 : case tok[1] of
               'm', 'd', 'h', 'n', 's' : GrabDigits(1);
               'z' : GrabDigits(2);
            end;
         2 : if tok='yy' then GrabDigits(2);
      end;

      hourToken:=False;
      case tok[1] of
         'd' :
            if (tok='d') or (tok='dd') then
               day:=value
            else if tok='ddd' then begin
               match:=False;
               Dec(p, 3);
               for j:=0 to High(settings.ShortDayNames) do begin
                  if UnicodeSameText(Copy(fmt, p, Length(settings.ShortDayNames[j])),
                                     settings.ShortDayNames[j]) then begin
                     Inc(p, Length(settings.ShortDayNames[j]));
                     match:=True;
                     break;
                  end;
               end;
               if not match then Exit;
            end else if tok='dddd' then begin
               match:=False;
               Dec(p, 4);
               for j:=0 to High(settings.LongDayNames) do begin
                  if UnicodeSameText(Copy(fmt, p, Length(settings.LongDayNames[j])),
                                     settings.LongDayNames[j]) then begin
                     Inc(p, Length(settings.LongDayNames[j]));
                     match:=True;
                     break;
                  end;
               end;
               if not match then Exit;
            end;
         'm' :
            if (tok='m') or (tok='mm') then begin
               if previousWasHour then
                  minutes:=value
               else month:=value
            end else if tok='mmm' then begin
               match:=False;
               Dec(p, 3);
               for j:=0 to High(settings.ShortMonthNames) do begin
                  if UnicodeSameText(Copy(fmt, p, Length(settings.ShortDayNames[j])),
                                     settings.ShortDayNames[j]) then begin
                     Inc(p, Length(settings.ShortDayNames[j]));
                     match:=True;
                     break;
                  end;
               end;
               if not match then Exit;
            end else if tok='mmmm' then begin
               match:=False;
               Dec(p, 4);
               for j:=0 to High(settings.LongMonthNames) do begin
                  if UnicodeSameText(Copy(fmt, p, Length(settings.LongDayNames[j])),
                                     settings.LongDayNames[j]) then begin
                     Inc(p, Length(settings.LongDayNames[j]));
                     match:=True;
                     break;
                  end;
               end;
               if not match then Exit;
            end;
         'y' :
            if (tok='yy') or (tok='yyyy') then
               year:=value;
         'h' :
            if (tok='h') or (tok='hh') then begin
               hours:=value;
               hourToken:=True;
            end;
         'n' :
            if (tok='n') or (tok='nn') then
               minutes:=value;
         's' :
            if (tok='s') or (tok='ss') then
               seconds:=value;
         'z' :
            if (tok='z') or (tok='zzz') then
               msec:=value;
      else
         litteral := Copy(str, p-Length(tok), Length(tok));
         if not UnicodeSameText(tok, litteral) then Exit;
         hourToken:=previousWasHour;
      end;
      previousWasHour:=hourToken;
   end;
   if p<Length(str) then Exit;

   dt:=0;
   if     (Cardinal(hours)<24) and (Cardinal(minutes)<60)
      and (Cardinal(seconds)<60) and (Cardinal(msec)<1000) then begin
      dth:=(hours+(minutes+(seconds+msec*0.001)/60)/60)/24;
   	if (day or month or year)<>0 then begin
         if TryEncodeDate(year, month, day, tz, dt) then begin
            dt:=dt+dth;
            Result:=True;
         end;
      end else begin
         dt:=dth;
         Result:=True;
      end;
   end;
end;

// TryStrToDate
//
function TdwsFormatSettings.TryStrToDate(const str : String; var dt : Double; tz : TdwsTimeZone) : Boolean;
begin
   Result:=   TryStrToDateTime(settings.ShortDateFormat, str, dt, tz)
           or TryStrToDateTime(settings.LongDateFormat, str, dt, tz);
end;

// TryStrToTime
//
function TdwsFormatSettings.TryStrToTime(const str : String; var dt : Double; tz : TdwsTimeZone) : Boolean;
begin
   Result:=   TryStrToDateTime(settings.ShortTimeFormat, str, dt, tz)
           or TryStrToDateTime(settings.LongTimeFormat, str, dt, tz);
end;

// TryStrToDateTime
//
function TdwsFormatSettings.TryStrToDateTime(const str : String; var dt : Double; tz : TdwsTimeZone) : Boolean;
begin
   Result:=   TryStrToDateTime(settings.ShortDateFormat+' '+settings.ShortTimeFormat, str, dt, tz)
           or TryStrToDateTime(settings.ShortDateFormat+' '+settings.LongTimeFormat, str, dt, tz)
           or TryStrToDateTime(settings.LongDateFormat+' '+settings.LongTimeFormat, str, dt, tz)
           or TryStrToDateTime(settings.LongDateFormat+' '+settings.ShortTimeFormat, str, dt, tz);
end;

// TryEncodeDate
//
function TdwsFormatSettings.TryEncodeDate(y, m, d : Integer; tz : TdwsTimeZone; var dt : Double) : Boolean;
begin
   Result:=SysUtils.TryEncodeDate(y, m, d, TDateTime(dt));

   if tz = tzDefault then
      tz := TimeZone;
   if tz = tzUTC then
      dt := LocalDateTimeToUTCDateTime(dt);
end;

// EncodeDate
//
function TdwsFormatSettings.EncodeDate(y, m, d : Integer; tz : TdwsTimeZone) : Double;
begin
   if not TryEncodeDate(y, m, d, tz, Result) then
      raise EConvertError.Create('Invalid date/time');
end;

end.
