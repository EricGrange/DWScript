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
   dwsUtils, dwsXPlatform, dwsXXHash;

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

         function YearOf(const dt : TDateTime) : Integer;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TDateTimeToken = (
      _string,
      _d, _dd, _ddd, _dddd,
      _m, _mm, _mmm, _mmmm,
      _yy, _yyyy,
      _h24, _hh24, _h12, _hh12,
      _n, _nn, _s, _ss, _z, _zzz,
      _ampm, _am_pm, _a_p
      );

   TDateTimeItem = record
      Token : TDateTimeToken;
      Literal : String;
   end;

   IdwsDateTimeFormatter = interface
      function Apply(dt : TDateTime; const settings : TFormatSettings) : String;
   end;

   TdwsDateTimeFormatter = class (TInterfacedObject, IdwsDateTimeFormatter)
      private
         Format : String;
         Items : array of TDateTimeItem;
         TimeNeeded, DateNeeded : Boolean;

         procedure AddToken(tok : TDateTimeToken);
         procedure AddLitteral(const s : String);

      public
         constructor Create(const fmt : String);

         class function Acquire(const fmt : String) : IdwsDateTimeFormatter; static;
         class procedure FlushCache; static;

         function Apply(dt : TDateTime; const settings : TFormatSettings) : String;
   end;

var
   vFormattersCacheLock : TMultiReadSingleWrite;
   vFormattersCache : TNameObjectHash;

// Acquire
//
class function TdwsDateTimeFormatter.Acquire(const fmt : String) : IdwsDateTimeFormatter;
var
   hash : Cardinal;
   formatter, other : TdwsDateTimeFormatter;
begin
   hash := vFormattersCache.HashName(fmt);
   vFormattersCacheLock.BeginRead;
   try
      formatter := TdwsDateTimeFormatter(vFormattersCache.HashedObjects[fmt, hash]);
      if formatter <> nil then
         Exit(formatter);
   finally
      vFormattersCacheLock.EndRead;
   end;
   formatter := TdwsDateTimeFormatter.Create(fmt);
   vFormattersCacheLock.BeginWrite;
   try
      other := TdwsDateTimeFormatter(vFormattersCache.HashedObjects[fmt, hash]);
      if other = nil then begin
         vFormattersCache.HashedObjects[fmt, hash] := formatter;
         formatter._AddRef;
      end else begin
         formatter.Free;
         formatter := other;
      end;
      Result := formatter;
   finally
      vFormattersCacheLock.EndWrite;
   end;
end;

// FlushCache
//
class procedure TdwsDateTimeFormatter.FlushCache;
var
   i : Integer;
begin
   vFormattersCacheLock.BeginWrite;
   try
      for i := 0 to vFormattersCache.HighIndex do
         if vFormattersCache.BucketObject[i] <> nil then
            TdwsDateTimeFormatter(vFormattersCache.BucketObject[i])._Release;
      vFormattersCache.Clear;
   finally
      vFormattersCacheLock.EndWrite;
   end;
end;

// Create
//
constructor TdwsDateTimeFormatter.Create(const fmt : String);
var
   p : PChar;
   quoteStart : PChar;
   buf : String;
begin
   inherited Create;
   Format := fmt;

   if fmt = '' then Exit;
   p := PChar(fmt);

   while p^ <> #0 do begin
      case p^ of
         'd', 'D' : begin
            if (p[1] = 'd') or (p[1] = 'D') then begin
               if (p[2] = 'd') or (p[2] = 'D') then begin
                  if (p[3] = 'd') or (p[3] = 'D') then begin
                     AddToken(_dddd);
                     Inc(p, 4);
                  end else begin
                     AddToken(_ddd);
                     Inc(p, 3);
                  end;
               end else begin
                  AddToken(_dd);
                  Inc(p, 2);
               end;
            end else begin
               AddToken(_d);
               Inc(p);
            end;
         end;
         'm', 'M' : begin
            if (p[1] = 'm') or (p[1] = 'M')  then begin
               if (p[2] = 'm') or (p[2] = 'M') then begin
                  if (p[3] = 'm') or (p[3] = 'm') then begin
                     AddToken(_mmmm);
                     Inc(p, 4);
                  end else begin
                     AddToken(_mmm);
                     Inc(p, 3);
                  end;
               end else begin
                  AddToken(_mm);
                  Inc(p, 2);
               end;
            end else begin
               AddToken(_m);
               Inc(p);
            end;
         end;
         'y', 'Y' : begin
            if (p[1] = 'y') or (p[1] = 'Y') then begin
               if ((p[2] = 'y') or (p[2] = 'Y')) and ((p[3] = 'y') or (p[3] = 'Y')) then begin
                  AddToken(_yyyy);
                  Inc(p, 4);
               end else begin
                  AddToken(_yy);
                  Inc(p, 2);
               end;
            end else begin
               AddLitteral('y');
               Inc(p);
            end;
         end;
         'h', 'H' : begin
            if (p[1] = 'h') or (p[1] = 'H') then begin
               AddToken(_hh24);
               Inc(p, 2);
            end else begin
               AddToken(_h24);
               Inc(p);
            end;
         end;
         'n', 'N' : begin
            if (p[1] = 'n') or (p[1] = 'n') then begin
               AddToken(_nn);
               Inc(p, 2);
            end else begin
               AddToken(_n);
               Inc(p);
            end;
         end;
         's', 'S' : begin
            if (p[1] = 's') or (p[1] = 's') then begin
               AddToken(_ss);
               Inc(p, 2);
            end else begin
               AddToken(_s);
               Inc(p);
            end;
         end;
         'z', 'Z' : begin
            if ((p[1] = 'z') or (p[1] = 'Z')) and ((p[2] = 'z') or (p[2] = 'Z')) then begin
               AddToken(_zzz);
               Inc(p, 3);
            end else begin
               AddToken(_z);
               Inc(p);
            end;
         end;
         'a' : begin
            if StrComp(p, 'ampm')=0 then begin
               AddToken(_ampm);
               Inc(p, 4);
            end else if StrComp(p, 'am/pm')=0 then begin
               AddToken(_am_pm);
               Inc(p, 5);
            end else if StrComp(p, 'a/p')=0 then begin
               AddToken(_a_p);
               Inc(p, 3);
            end else begin
               AddLitteral('a');
               Inc(p);
            end;
         end;
         '"', '''' : begin
            quoteStart := p;
            Inc(p);
            while (p^ <> #0) and (p^ <> quoteStart^) do
               Inc(p);
            Inc(quoteStart);
            if quoteStart <> p then begin
               SetString(buf, quoteStart, (NativeUInt(p)-NativeUInt(quoteStart)) div SizeOf(Char));
               AddLitteral(buf);
            end;
            if p^ <> #0 then
               Inc(p);
         end;
      else
         AddLitteral(p^);
         Inc(p);
      end;
   end;
end;

// Apply
//
function TdwsDateTimeFormatter.Apply(dt : TDateTime; const settings : TFormatSettings) : String;
var
   i : Integer;
   hours, minutes, seconds, msec : Word;
   year, month, day, dow : Word;
   wobs : TWriteOnlyBlockStream;
begin
   if DateNeeded then begin
      DecodeDateFully(dt, year, month, day, dow);
      if (month = 0) or (day = 0) then
         raise Exception.CreateFmt('Invalid date (%f)', [dt]);
   end;
   if TimeNeeded then
      DecodeTime(dt, hours, minutes, seconds, msec);

   wobs := TWriteOnlyBlockStream.AllocFromPool;
   try
      for i := 0 to High(Items) do begin
         case Items[i].Token of
            _d : wobs.WriteString(day);
            _dd : wobs.WriteP(@cTwoDigits[day], 2);
            _ddd : wobs.WriteString(settings.ShortDayNames[dow]);
            _dddd : wobs.WriteString(settings.LongDayNames[dow]);
            _m : wobs.WriteString(month);
            _mm : wobs.WriteP(@cTwoDigits[month], 2);
            _mmm : wobs.WriteString(settings.ShortMonthNames[month]);
            _mmmm : wobs.WriteString(settings.LongMonthNames[month]);
            _yy : wobs.WriteP(@cTwoDigits[year mod 100], 2);
            _yyyy : wobs.WriteDigits(year, 4);
            _h24 : wobs.WriteString(hours);
            _hh24 : wobs.WriteP(@cTwoDigits[hours], 2);
            _h12 : wobs.WriteString((hours + 11) mod 12 + 1);
            _hh12 : wobs.WriteP(@cTwoDigits[(hours + 11) mod 12 + 1], 2);
            _n : wobs.WriteString(minutes);
            _nn : wobs.WriteP(@cTwoDigits[minutes], 2);
            _s : wobs.WriteString(seconds);
            _ss : wobs.WriteP(@cTwoDigits[seconds], 2);
            _z : wobs.WriteString(msec);
            _zzz : wobs.WriteDigits(msec, 3);
            _ampm : begin
               if hours in [1..12] then
                  wobs.WriteString(settings.TimeAMString)
               else wobs.WriteString(settings.TimePMString);
            end;
            _am_pm : begin
               if hours in [1..12] then
                  wobs.WriteString('am')
               else wobs.WriteString('pm');
            end;
            _a_p : begin
               if hours in [1..12] then
                  wobs.WriteString('a')
               else wobs.WriteString('p');
            end;
            _string : wobs.WriteString(Items[i].Literal);
         else
            Assert(False);
         end;
      end;
      Result := wobs.ToString;
   finally
      wobs.ReturnToPool;
   end;
end;

// AddToken
//
procedure TdwsDateTimeFormatter.AddToken(tok : TDateTimeToken);
var
   i, n : Integer;
begin
   n := Length(Items);
   SetLength(Items, n+1);
   case tok of
      _d.._dddd, _mmm.._yyyy : DateNeeded := True;
      _m, _mm : begin
         // if immediately after an hour, interpret as  minutes, otherwise interpret as month
         for i := n-1 downto 0 do begin
            case Items[i].Token of
               _string : ;
               _h24.._hh12 : begin
                  if tok = _m then
                     tok := _n
                  else tok := _nn;
                  Break; // TimeNeeded already set by hour token
               end;
            else
               DateNeeded := True;
               Break;
            end;
         end;
      end;
      _h24.._zzz : TimeNeeded := True;
      _ampm.._a_p : begin
         TimeNeeded := True;
         // affects the previous hour token to make it 12 rather than 24
         for i := n-1 downto 0 do begin
            case Items[i].Token of
               _h24 : begin
                  Items[i].Token := _h12;
                  Break;
               end;
               _hh24 : begin
                  Items[i].Token := _hh12;
                  Break;
               end;
               _h12, _hh12 : Break;
            end;
         end;
      end;
   end;
   Items[n].Token := tok;
end;

// AddLitteral
//
procedure TdwsDateTimeFormatter.AddLitteral(const s : String);
var
   n : Integer;
begin
   n := Length(Items);
   if (n > 0) and (Items[n-1].Token = _string) then begin
      Items[n-1].Literal := Items[n-1].Literal + s;
   end else begin
      SetLength(Items, n+1);
      Items[n].Token := _string;
      Items[n].Literal := s;
   end;
end;

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
// Clean Room implementation based strictly on the format String
function TdwsFormatSettings.FormatDateTime(const fmt : String; dt : Double; tz : TdwsTimeZone) : String;
begin
   if fmt = '' then Exit;

   if (dt<-693592) or (dt>2146790052) then
      raise EConvertError.Create('Invalid date/time');
   if tz = tzDefault then
      tz := TimeZone;
   if tz = tzUTC then
      dt := UTCDateTimeToLocalDateTime(dt);

   Result := TdwsDateTimeFormatter.Acquire(fmt).Apply(dt, Settings);
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
   i, j, p, value, fmtLength, digit, currentYear : Integer;
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
               for j:=Low(Settings.ShortDayNames) to High(Settings.ShortDayNames) do begin
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
               for j:=Low(Settings.LongDayNames) to High(Settings.LongDayNames) do begin
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
               for j:=Low(settings.ShortMonthNames) to High(settings.ShortMonthNames) do begin
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
               for j:=Low(settings.LongMonthNames) to High(settings.LongMonthNames) do begin
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
            if tok = 'yyyy' then
               year := value
            else if tok = 'yy' then begin
               currentYear := YearOf(Now);
               year := (currentYear div 100)*100 + value;
               if year > currentYear + 50 then
                  year := year - 100;
            end;
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

// YearOf
//
function TdwsFormatSettings.YearOf(const dt : TDateTime) : Integer;
var
   y, m, d : Word;
begin
   DecodeDate(dt, y, m, d);
   Result := y;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vFormattersCache := TNameObjectHash.Create;
   vFormattersCacheLock := TMultiReadSingleWrite.Create;

finalization

   TdwsDateTimeFormatter.FlushCache;
   FreeAndNil(vFormattersCacheLock);
   FreeAndNil(vFormattersCache);

end.
