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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsTimeFunctions;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsStrings, dwsXPlatform, dwsDateTime,
   dwsFunctions, dwsExprs, dwsSymbols, dwsUnitSymbols, dwsExprList,
   dwsMagicExprs, dwsExternalSymbols, dwsWebUtils;

type

  TNowFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TDateFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TUTCDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TLocalDateTimeToUTCDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TUTCDateTimeToLocalDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TUnixTimeFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TUnixTimeToDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TLocalDateTimeToUnixTimeFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TUnixTimeToLocalDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TSleepFunc = class(TInternalMagicProcedure)
    procedure DoEvalProc(const args : TExprBaseListExec); override;
  end;

  TDateTimeToStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
  end;

  TStrToDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TStrToDateTimeDefFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TParseDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TDateToStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
  end;

  TStrToDateFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TStrToDateDefFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TTimeToStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
  end;

  TStrToTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TStrToTimeDefFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TDateToISO8601Func = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
  end;

  TDateTimeToISO8601Func = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
  end;

  TISO8601ToDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TDateTimeToRFC822Func = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
  end;

  TRFC822ToDateTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TDayOfWeekFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TDayOfTheWeekFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TFormatDateTimeFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
  end;

  TIsLeapYearFunc = class(TInternalMagicBoolFunction)
    function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
  end;

  TIncMonthFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TDecodeDateFunc = class(TInternalMagicProcedure)
    procedure DoEvalProc(const args : TExprBaseListExec); override;
  end;

  TEncodeDateFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TDecodeTimeFunc = class(TInternalMagicProcedure)
    procedure DoEvalProc(const args : TExprBaseListExec); override;
  end;

  TEncodeTimeFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TFirstDayOfYearFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TFirstDayOfNextYearFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TFirstDayOfMonthFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TFirstDayOfNextMonthFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TFirstDayOfWeekFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
  end;

  TDayOfYearFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TMonthOfYearFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TDayOfMonthFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TWeekNumberFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TYearOfWeekFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

  TYearOfFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  cDateTime = SYS_FLOAT;
  cSleepGranulosity = 200;

  SYS_FORMAT_SETTINGS = 'FormatSettings';
  SYS_DATE_TIME_ZONE = 'DateTimeZone';

type
   TFormatSettingsHandler = class (TInterfacedObject, IExternalSymbolHandler)
      procedure Assign(exec : TdwsExecution; symbol : TDataSymbol; expr : TTypedExpr; var handled : Boolean);
      procedure Eval(exec : TdwsExecution; symbol : TDataSymbol; var handled : Boolean; var result : Variant);
   end;

// RegisterFormatSettings
//
procedure RegisterFormatSettings(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                 unitTable : TSymbolTable);

   function AddClassVar(owner : TClassSymbol; const name : String; const h : IExternalSymbolHandler; typ : TTypeSymbol) : TClassVarSymbol;
   begin
      Result:=TClassVarSymbol.Create(name, typ, cvPublic);
      Result.ExternalName:='$fmt.'+name;
      owner.AddClassVar(Result);
      TExternalSymbolHandler.Register(Result, h);
   end;

var
   typFormatSettings : TClassSymbol;
   typDTZ : TEnumerationSymbol;
   handler : IExternalSymbolHandler;
begin
   if systemTable.FindLocal(SYS_FORMAT_SETTINGS)<>nil then exit;

   typDTZ:=TEnumerationSymbol.Create(SYS_DATE_TIME_ZONE, systemTable.TypInteger, enumScoped);
   typDTZ.AddElement(TElementSymbol.Create('Default', typDTZ, 0, False));
   typDTZ.AddElement(TElementSymbol.Create('Local', typDTZ, 1, False));
   typDTZ.AddElement(TElementSymbol.Create('UTC', typDTZ, 2, False));
   systemTable.AddSymbol(typDTZ);

   handler:=TFormatSettingsHandler.Create;

   typFormatSettings:=TClassSymbol.Create(SYS_FORMAT_SETTINGS, nil);
   typFormatSettings.IsExternal:=True;
   typFormatSettings.IsStatic:=True;
   AddClassVar(typFormatSettings, 'ShortDateFormat', handler, systemTable.TypString);
   AddClassVar(typFormatSettings, 'LongDateFormat', handler, systemTable.TypString);
   AddClassVar(typFormatSettings, 'ShortTimeFormat', handler, systemTable.TypString);
   AddClassVar(typFormatSettings, 'LongTimeFormat', handler, systemTable.TypString);
   AddClassVar(typFormatSettings, 'TimeAMString', handler, systemTable.TypString);
   AddClassVar(typFormatSettings, 'TimePMString', handler, systemTable.TypString);
   AddClassVar(typFormatSettings, 'Zone', handler, typDTZ).ExternalName:='$TZ';

   systemTable.AddSymbol(typFormatSettings);
end;

// DateTimeConversionError
//
procedure DateTimeConversionError(const str : String);
begin
   raise EConvertError.CreateFmt('Date/time parsing error for "%s"', [str]);
end;

// ------------------
// ------------------ TFormatSettingsHandler ------------------
// ------------------

// Assign
//
procedure TFormatSettingsHandler.Assign(exec : TdwsExecution; symbol : TDataSymbol; expr : TTypedExpr; var handled : Boolean);

   {$ifdef FPC}
   procedure EvalAsString(var s : String);
   var
      u : String;
   begin
      expr.EvalAsString(exec, u);
      s := u;
      handled := True;
   end;
   {$else}
   procedure EvalAsString(var s : String);
   begin
      expr.EvalAsString(exec, s);
      handled:=True;
   end;
   {$endif}

begin
   handled:=False;
   if symbol.Name='' then Exit;

   case symbol.Name[1] of
      'S' :
         if symbol.Name='ShortDateFormat' then
            EvalAsString(exec.FormatSettings.Settings.ShortDateFormat)
         else if symbol.Name='ShortTimeFormat' then
            EvalAsString(exec.FormatSettings.Settings.ShortTimeFormat);
      'L' :
         if symbol.Name='LongDateFormat' then
            EvalAsString(exec.FormatSettings.Settings.LongDateFormat)
         else if symbol.Name='LongTimeFormat' then
            EvalAsString(exec.FormatSettings.Settings.LongTimeFormat);
      'T' :
         if symbol.Name='TimeAMString' then
            EvalAsString(exec.FormatSettings.Settings.TimeAMString)
         else if symbol.Name='TimePMString' then
            EvalAsString(exec.FormatSettings.Settings.TimePMString);
      'Z' :
         if symbol.Name='Zone' then
            exec.FormatSettings.TimeZone:=TdwsTimeZone(expr.EvalAsInteger(exec));
   end;
end;

// Eval
//
procedure TFormatSettingsHandler.Eval(exec : TdwsExecution; symbol : TDataSymbol; var handled : Boolean; var result : Variant);
begin
   handled:=False;
   if symbol.Name='' then Exit;

   VarClearSafe(result);
   case symbol.Name[1] of
      'S' :
         if symbol.Name='ShortDateFormat' then
            result:=exec.FormatSettings.Settings.ShortDateFormat
         else if symbol.Name='ShortTimeFormat' then
            result:=exec.FormatSettings.Settings.ShortTimeFormat;
      'L' :
         if symbol.Name='LongDateFormat' then
            result:=exec.FormatSettings.Settings.LongDateFormat
         else if symbol.Name='LongTimeFormat' then
            result:=exec.FormatSettings.Settings.LongTimeFormat;
      'T' :
         if symbol.Name='TimeAMString' then
            result:=exec.FormatSettings.Settings.TimeAMString
         else if symbol.Name='TimePMString' then
            result:=exec.FormatSettings.Settings.TimePMString;
      'Z' :
         if symbol.Name='Zone' then
            result:=Integer(exec.FormatSettings.TimeZone);
   end;

   handled := TVarData(result).VType<>varEmpty;
end;

{ TNowFunc }

procedure TNowFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Now;
end;

{ TDateFunc }

procedure TDateFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Date;
end;

{ TTimeFunc }

procedure TTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Time;
end;

{ TUTCDateTimeFunc }

procedure TUTCDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=UTCDateTime;
end;

{ TLocalDateTimeToUTCDateTimeFunc }

procedure TLocalDateTimeToUTCDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := LocalDateTimeToUTCDateTime(args.AsFloat[0]);
end;

{ TUTCDateTimeToLocalDateTimeFunc }

procedure TUTCDateTimeToLocalDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := UTCDateTimeToLocalDateTime(args.AsFloat[0]);
end;

{ TUnixTimeFunc }

function TUnixTimeFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   t : TDateTime;
begin
   if args.Count>0 then
      t:=args.AsFloat[0]
   else t:=UTCDateTime;
   Result:=Round(t*86400)-Int64(25569)*86400;
end;

{ TUnixTimeToDateTimeFunc }

procedure TUnixTimeToDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=args.AsInteger[0]/86400+25569;
end;

{ TUnixTimeToLocalDateTimeFunc }

function TLocalDateTimeToUnixTimeFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := Trunc(LocalDateTimeToUTCDateTime(args.AsFloat[0])*86400)-Int64(25569)*86400;
end;

{ TUnixTimeToLocalDateTimeFunc }

procedure TUnixTimeToLocalDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := UTCDateTimeToLocalDateTime(args.AsInteger[0]/86400+25569);
end;

{ TDateTimeToStrFunc }

procedure TDateTimeToStrFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result:=args.FormatSettings.DateTimeToStr(args.AsFloat[0], TdwsTimeZone(args.AsInteger[1]));
end;

{ TStrToDateTimeFunc }

procedure TStrToDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   s : String;
   utc : TdwsTimeZone;
begin
   s:=args.AsString[0];
   utc:=TdwsTimeZone(args.AsInteger[1]);
   if not args.FormatSettings.TryStrToDateTime(s, Result, utc) then
      if not args.FormatSettings.TryStrToDate(s, Result, utc) then
         DateTimeConversionError(s);
end;

{ TStrToDateTimeDefFunc }

procedure TStrToDateTimeDefFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   s : String;
   def : Double;
   utc : TdwsTimeZone;
begin
   s := args.AsString[0];
   def := args.AsFloat[1];
   utc := TdwsTimeZone(args.AsInteger[2]);
   if not args.FormatSettings.TryStrToDateTime(args.AsString[0], Result, utc) then
      if not args.FormatSettings.TryStrToDate(s, Result, utc) then
         Result:=def;
end;

{ TParseDateTimeFunc }

procedure TParseDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   if not args.FormatSettings.TryStrToDateTime(args.AsString[0], args.AsString[1], Result, TdwsTimeZone(args.AsInteger[2])) then
      Result:=0;
end;

{ TDateToStrFunc }

procedure TDateToStrFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result:=args.FormatSettings.DateToStr(args.AsFloat[0], TdwsTimeZone(args.AsInteger[1]));
end;

{ TStrToDateFunc }

procedure TStrToDateFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   s : String;
begin
   s:=args.AsString[0];
   if not args.FormatSettings.TryStrToDate(args.AsString[0], Result, TdwsTimeZone(args.AsInteger[1])) then
      DateTimeConversionError(s);
end;

{ TStrToDateDefFunc }

procedure TStrToDateDefFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   def : Double;
begin
   def:=args.AsFloat[1];
   if not args.FormatSettings.TryStrToDate(args.AsString[0], Result, TdwsTimeZone(args.AsInteger[2])) then
      Result:=def;
end;

{ TTimeToStrFunc }

procedure TTimeToStrFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result:=args.FormatSettings.TimeToStr(args.AsFloat[0], TdwsTimeZone(args.AsInteger[1]));
end;

{ TStrToTimeFunc }

procedure TStrToTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   s : String;
begin
   s:=args.AsString[0];
   if not args.FormatSettings.TryStrToTime(s, Result, TdwsTimeZone(args.AsInteger[1])) then
      DateTimeConversionError(s);
end;

{ TStrToTimeDefFunc }

procedure TStrToTimeDefFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   def : Double;
begin
   def:=args.AsFloat[1];
   if not args.FormatSettings.TryStrToTime(args.AsString[0], Result, TdwsTimeZone(args.AsInteger[2])) then
      Result:=def;
end;

{ TDateToISO8601Func }

procedure TDateToISO8601Func.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result:=FormatDateTime('yyyy-mm-dd', args.AsFloat[0]);
end;

{ TDateTimeToISO8601Func }

procedure TDateTimeToISO8601Func.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result:=DateTimeToISO8601(args.AsFloat[0], True);
end;

{ TISO8601ToDateTimeFunc }

procedure TISO8601ToDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=ISO8601ToDateTime(args.AsString[0]);
end;

{ TDateTimeToRFC822Func }

procedure TDateTimeToRFC822Func.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result := WebUtils.DateTimeToRFC822(args.AsFloat[0]);
end;

{ TRFC822ToDateTimeFunc }

procedure TRFC822ToDateTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := WebUtils.RFC822ToDateTime(args.AsString[0]);
end;

{ TDayOfWeekFunc }

function TDayOfWeekFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=DayOfWeek(args.AsFloat[0]);
end;

{ TDayOfTheWeekFunc }

function TDayOfTheWeekFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=(DayOfWeek(args.AsFloat[0])+5) mod 7 +1;
end;

{ TFormatDateTimeFunc }

// DoEvalAsString
//
procedure TFormatDateTimeFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result:=args.FormatSettings.FormatDateTime(args.AsString[0], args.AsFloat[1], TdwsTimeZone(args.AsInteger[2]));
end;

{ TIsLeapYearFunc }

function TIsLeapYearFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=IsLeapYear(args.AsInteger[0]);
end;

{ TIncMonthFunc }

procedure TIncMonthFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=IncMonth(args.AsFloat[0], args.AsInteger[1]);
end;

{ TDecodeDateFunc }

// DoEvalProc
//
procedure TDecodeDateFunc.DoEvalProc(const args : TExprBaseListExec);
var
  y, m, d: word;
begin
  DecodeDate(args.AsFloat[0], y, m, d);
  args.AsInteger[1] := y;
  args.AsInteger[2] := m;
  args.AsInteger[3] := d;
end;

{ TEncodeDateFunc }

procedure TEncodeDateFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=args.FormatSettings.EncodeDate(args.AsInteger[0], args.AsInteger[1], args.AsInteger[2],
                                          TdwsTimeZone(args.AsInteger[3]));
end;

{ TDecodeTimeFunc }

// DoEvalProc
//
procedure TDecodeTimeFunc.DoEvalProc(const args : TExprBaseListExec);
var
   h, m, s, ms: word;
begin
   DecodeTime(args.AsFloat[0], h, m, s, ms);
   args.AsInteger[1]:=h;
   args.AsInteger[2]:=m;;
   args.AsInteger[3]:=s;
   args.AsInteger[4]:=ms;
end;

{ TEncodeTimeFunc }

procedure TEncodeTimeFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=EncodeTime(args.AsInteger[0], args.AsInteger[1], args.AsInteger[2], args.AsInteger[3]);
end;

{ TFirstDayOfYearFunc }

procedure TFirstDayOfYearFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   Result:=EncodeDate(y, 1, 1);
end;

{ TFirstDayOfNextYearFunc }

procedure TFirstDayOfNextYearFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   Result:=EncodeDate(y+1, 1, 1);
end;

{ TFirstDayOfMonthFunc }

procedure TFirstDayOfMonthFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   Result:=EncodeDate(y, m, 1);
end;

{ TFirstDayOfNextMonthFunc }

procedure TFirstDayOfNextMonthFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   if m=12 then begin
      Inc(y);
      m:=1;
   end else Inc(m);
   Result:=EncodeDate(y, m, 1);
end;

{ TFirstDayOfWeekFunc }

procedure TFirstDayOfWeekFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
const
   cDayOfWeekConverter : array [1..7] of Byte = (6, 0, 1, 2, 3, 4, 5);
var
   dt : TDateTime;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   Result:=Trunc(dt)-cDayOfWeekConverter[DayOfWeek(dt)];
end;

{ TDayOfYearFunc }

function TDayOfYearFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   Result:=Trunc(dt-EncodeDate(y, 1, 1))+1;
end;

{ TMonthOfYearFunc }

function TMonthOfYearFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   Result:=m;
end;

{ TDayOfMonthFunc }

function TDayOfMonthFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   Result:=d;
end;

{ TWeekNumberFunc }

// DateToWeekNumber
//
function DateToWeekNumber(aDate : TDateTime) : Integer;
var
   weekDay : Integer;
   month, day : Word;
   yearOfWeek : Word;
const
   // Weekday to start the week
   //   1 : Sunday
   //   2 : Monday (according to ISO 8601)
   cISOFirstWeekDay = 2;

   // minmimum number of days of the year in the first week of the year week
   //   1 : week one starts at 1/1
   //   4 : first week has at least four days (according to ISO 8601)
   //   7 : first full week
   cISOFirstWeekMinDays=4;
begin
   weekDay:=((DayOfWeek(aDate)-cISOFirstWeekDay+7) mod 7)+1;
   aDate:=aDate-weekDay+8-cISOFirstWeekMinDays;
   DecodeDate(aDate, YearOfWeek, month, day);
   Result:=(Trunc(aDate-EncodeDate(yearOfWeek, 1, 1)) div 7)+1;
end;

function TWeekNumberFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   dt : TDateTime;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   Result:=DateToWeekNumber(dt);
end;

{ TYearOfWeekFunc }

function TYearOfWeekFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   if ((m=1) and (d<4)) then begin
      // days whose week can be on previous year
      if (DateToWeekNumber(dt)=1) then begin
         // first week of the same year as the day
         Result:=y;
      end else begin
         // week 52 or 53 of previous year
         Result:=y-1;
      end;
   end else if ((m=12) and (d>=29)) then begin
      // days whose week can be on the next year
      if (DateToWeekNumber(dt)=1) then begin
         // week one of next year
         Result:=y+1;
      end else begin
         // week 52 or 53 of current year
         Result:=y;
      end;
   end else begin
      // middle of the year, nothing to compute
      Result:=y;
   end;
end;

{ TYearOfFunc }

function TYearOfFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   dt : TDateTime;
   y, m, d : Word;
begin
   dt:=args.AsFloat[0];
   if dt=0 then
      dt:=Now;
   DecodeDate(dt, y, m, d);
   Result:=y;
end;

// ------------------
// ------------------ TSleepFunc ------------------
// ------------------

// DoEvalProc
//
procedure TSleepFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   args.Exec.Sleep(args.AsInteger[0], cSleepGranulosity);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterFormatSettings);

   RegisterInternalFloatFunction(TNowFunc, 'Now', []);
   RegisterInternalFloatFunction(TDateFunc, 'Date', []);
   RegisterInternalFloatFunction(TTimeFunc, 'Time', []);

   RegisterInternalProcedure(TSleepFunc, 'Sleep', ['msec', SYS_INTEGER]);

   RegisterInternalFloatFunction(TUTCDateTimeFunc, 'UTCDateTime', []);

   RegisterInternalFloatFunction(TLocalDateTimeToUTCDateTimeFunc, 'LocalDateTimeToUTCDateTime', ['t', cDateTime]);
   RegisterInternalFloatFunction(TUTCDateTimeToLocalDateTimeFunc, 'UTCDateTimeToLocalDateTime', ['t', cDateTime]);

   RegisterInternalIntFunction(TUnixTimeFunc, 'UnixTime', []);
   RegisterInternalIntFunction(TUnixTimeFunc, 'DateTimeToUnixTime', ['utc', cDateTime]);
   RegisterInternalFloatFunction(TUnixTimeToDateTimeFunc, 'UnixTimeToDateTime', ['ut', SYS_INTEGER]);
   RegisterInternalIntFunction(TLocalDateTimeToUnixTimeFunc, 'LocalDateTimeToUnixTime', ['dt', cDateTime]);
   RegisterInternalFloatFunction(TUnixTimeToLocalDateTimeFunc, 'UnixTimeToLocalDateTime', ['ut', SYS_INTEGER]);

   RegisterInternalFloatFunction(TParseDateTimeFunc, 'ParseDateTime', ['fmt', SYS_STRING, 'str', SYS_STRING, 'utc=0', SYS_DATE_TIME_ZONE]);

   RegisterInternalStringFunction(TDateTimeToStrFunc, 'DateTimeToStr', ['dt', cDateTime, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalFloatFunction(TStrToDateTimeFunc, 'StrToDateTime', ['str', SYS_STRING, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalFloatFunction(TStrToDateTimeDefFunc, 'StrToDateTimeDef', ['str', SYS_STRING, 'def', cDateTime, 'utc=0', SYS_DATE_TIME_ZONE]);

   RegisterInternalStringFunction(TDateToStrFunc, 'DateToStr', ['dt', cDateTime, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalFloatFunction(TStrToDateFunc, 'StrToDate', ['str', SYS_STRING, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalFloatFunction(TStrToDateDefFunc, 'StrToDateDef', ['str', SYS_STRING, 'def', cDateTime, 'utc=0', SYS_DATE_TIME_ZONE]);

   RegisterInternalStringFunction(TDateToISO8601Func, 'DateToISO8601', ['dt', cDateTime]);
   RegisterInternalStringFunction(TDateTimeToISO8601Func, 'DateTimeToISO8601', ['dt', cDateTime]);
   RegisterInternalFloatFunction(TISO8601ToDateTimeFunc, 'ISO8601ToDateTime', ['s', SYS_STRING]);

   RegisterInternalStringFunction(TDateTimeToRFC822Func, 'DateTimeToRFC822', ['dt', cDateTime]);
   RegisterInternalFloatFunction(TRFC822ToDateTimeFunc, 'RFC822ToDateTime', ['s', SYS_STRING]);

   RegisterInternalStringFunction(TTimeToStrFunc, 'TimeToStr', ['dt', cDateTime, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalFloatFunction(TStrToTimeFunc, 'StrToTime', ['str', SYS_STRING, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalFloatFunction(TStrToTimeDefFunc, 'StrToTimeDef', ['str', SYS_STRING, 'def', cDateTime, 'utc=0', SYS_DATE_TIME_ZONE]);

   RegisterInternalIntFunction(TDayOfWeekFunc, 'DayOfWeek', ['dt', cDateTime]);
   RegisterInternalIntFunction(TDayOfTheWeekFunc, 'DayOfTheWeek', ['dt', cDateTime]);
   RegisterInternalStringFunction(TFormatDateTimeFunc, 'FormatDateTime', ['frm', SYS_STRING, 'dt', cDateTime, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalBoolFunction(TIsLeapYearFunc, 'IsLeapYear', ['year', SYS_INTEGER]);
   RegisterInternalFloatFunction(TIncMonthFunc, 'IncMonth', ['dt', cDateTime, 'nb', SYS_INTEGER, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalProcedure(TDecodeDateFunc, 'DecodeDate', ['dt', cDateTime, '@y', SYS_INTEGER, '@m', SYS_INTEGER, '@d', SYS_INTEGER, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalFloatFunction(TEncodeDateFunc, 'EncodeDate', ['y', SYS_INTEGER, 'm', SYS_INTEGER, 'd', SYS_INTEGER, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalProcedure(TDecodeTimeFunc, 'DecodeTime', ['dt', cDateTime, '@h', SYS_INTEGER, '@m', SYS_INTEGER, '@s', SYS_INTEGER, '@ms', SYS_INTEGER, 'utc=0', SYS_DATE_TIME_ZONE]);
   RegisterInternalFloatFunction(TEncodeTimeFunc, 'EncodeTime', ['h', SYS_INTEGER, 'm', SYS_INTEGER, 's', SYS_INTEGER, 'ms', SYS_INTEGER], [iffStateLess]);

   RegisterInternalFloatFunction(TFirstDayOfYearFunc, 'FirstDayOfYear', ['dt', cDateTime]);
   RegisterInternalFloatFunction(TFirstDayOfNextYearFunc, 'FirstDayOfNextYear', ['dt', cDateTime]);
   RegisterInternalFloatFunction(TFirstDayOfMonthFunc, 'FirstDayOfMonth', ['dt', cDateTime]);
   RegisterInternalFloatFunction(TFirstDayOfNextMonthFunc, 'FirstDayOfNextMonth', ['dt', cDateTime]);
   RegisterInternalFloatFunction(TFirstDayOfWeekFunc, 'FirstDayOfWeek', ['dt', cDateTime]);
   RegisterInternalIntFunction(TDayOfYearFunc, 'DayOfYear', ['dt', cDateTime]);
   RegisterInternalIntFunction(TMonthOfYearFunc, 'MonthOfYear', ['dt', cDateTime]);
   RegisterInternalIntFunction(TDayOfMonthFunc, 'DayOfMonth', ['dt', cDateTime]);
   RegisterInternalIntFunction(TYearOfFunc, 'YearOf', ['dt', cDateTime]);

   RegisterInternalIntFunction(TWeekNumberFunc, 'DateToWeekNumber', ['dt', cDateTime]);
   RegisterInternalIntFunction(TWeekNumberFunc, 'WeekNumber', ['dt', cDateTime]);
   RegisterInternalIntFunction(TYearOfWeekFunc, 'DateToYearOfWeek', ['dt', cDateTime]);
   RegisterInternalIntFunction(TYearOfWeekFunc, 'YearOfWeek', ['dt', cDateTime]);

end.
