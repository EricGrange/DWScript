{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at                                          }
{                                                                      }
{    http://www.mozilla.org/MPL/                                       }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Original Code is DelphiWebScriptII source code, released      }
{    January 1, 2001                                                   }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. Portions created by Matthias Ackermann are             }
{    Copyright (C) 2000 Matthias Ackermann, Switzerland. All           }
{    Rights Reserved.                                                  }
{                                                                      }
{    Contributor(s): Daniele Teti <d.teti@bittime.it>                  }
{                                                                      }
{**********************************************************************}

{$I dws.inc}

unit dwsHtmlFilter;

interface

uses
  Variants, Classes, SysUtils, dwsComp, dwsExprs, dwsFunctions, dwsSymbols,
  dwsErrors, dwsCompiler, dwsStrings, dwsStringResult;

type
  TdwsHtmlFilter = class(TdwsFilter)
  private
    FPatternOpen: string;
    FPatternClose: string;
    FPatternEval: string;
    procedure SetPatternClose(const Value: string);
    procedure SetPatternEval(const Value: string);
    procedure SetPatternOpen(const Value: string);
    procedure CheckPatterns;
  public
    constructor Create(AOwner: TComponent); override;
    function Process(const Text: string; Msgs: TdwsMessageList): string; override;
  published
    property PatternClose: string read FPatternClose write SetPatternClose;
    property PatternEval: string read FPatternEval write SetPatternEval;
    property PatternOpen: string read FPatternOpen write SetPatternOpen;
  end;

  TdwsHtmlUnit = class(TdwsUnitComponent)
  protected
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSendFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TSendLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

implementation

{ TdwsHtmlFilter }

constructor TdwsHtmlFilter.Create(AOwner: TComponent);
begin
  inherited;
  PrivateDependencies.Add('HTML');
  
  FPatternOpen := '<%';
  FPatternClose := '%>';
  FPatternEval := '=';
end;

function TdwsHtmlFilter.Process(const Text: String; Msgs: TdwsMessageList): String;

   procedure StuffString(const str: String; start, stop : Integer;
                         dest : TStringBuilder);
   var
      isQuoted: Boolean;
      i, lineCount: Integer;
   begin
      dest.Append('''');
      isQuoted := True;
      lineCount := 0;
      for i := start to stop do begin
         if isQuoted then begin
            case str[i] of
               '''': dest.Append('''''');
               #10: begin
                  dest.Append('''#10');
                  isQuoted := False;
                  Inc(lineCount);
               end;
               #13: begin
                  dest.Append('''#13');
                  isQuoted := False;
               end;
               #9: begin
                  dest.Append('''#9');
                  isQuoted := False;
               end;
            else
               dest.Append(str[i]);
            end
         end else begin
            case str[i] of
               '''': begin
                  dest.Append('''''''');
                  isQuoted := True;
               end;
               #10: begin
                  dest.Append('#10');
                  Inc(lineCount);
               end;
               #13: dest.Append('#13');
               #9: dest.Append('#9');
            else
               dest.Append('''');
               dest.Append(Str[i]);
               isQuoted := True;
            end;
         end;
      end;

      if isQuoted then
         dest.Append('''');

      for i := 1 to lineCount do
         dest.Append(#13#10);
   end;

var
   state: (sNone, sSend);
   index, patOpen, patClose, patEval: Integer;
   htmlText, chunk, pattern: string;
   builder : TStringBuilder;
begin
   // Initializations
   htmlText := inherited Process(Text, Msgs);
   patOpen := Length(FPatternOpen) - 1;
   patClose := Length(FPatternClose) - 1;
   patEval := Length(FPatternEval) + 1;

   builder:=TStringBuilder.Create;
   try

      state := sNone;
      pattern := FPatternOpen;

      // Start conversion
      repeat
         index := AnsiPos(pattern, htmlText);
         if index = 0 then
            index := Length(htmlText) + 1;

         case state of
            sNone: begin
               // Normal HTML code.
               // Looking for <%
               if index > 1 then begin
                  builder.Append('Send(');
                  StuffString(htmlText, 1, index-1, builder);
                  builder.Append(');');
               end;
               Delete(htmlText, 1, index + patOpen);
               pattern := FPatternClose;
               state := sSend;
            end;
            sSend: begin
               // Inside a <% %> tag
               // Looking for %>
               chunk := Copy(htmlText, 1, index - 1);
               if Pos(FPatternEval, chunk) = 1 then begin
                  builder.Append('Send(');
                  builder.Append(Copy(chunk, patEval, Length(chunk)));
                  builder.Append(');');
               end else begin
                  builder.Append( chunk );
               end;
               Delete(htmlText, 1, index + patClose);
               pattern := FPatternOpen;
               state := sNone;
            end;
         end;
      until Length(htmlText) = 0;

      Result:=builder.ToString;

   finally
      builder.Free;
   end;
end;

procedure TdwsHtmlFilter.SetPatternClose(const Value: string);
begin
  FPatternClose := Value;
  CheckPatterns;
end;

procedure TdwsHtmlFilter.SetPatternEval(const Value: string);
begin
  FPatternEval := Value;
  CheckPatterns;
end;

procedure TdwsHtmlFilter.SetPatternOpen(const Value: string);
begin
  FPatternOpen := Value;
  CheckPatterns;
end;

procedure TdwsHtmlFilter.CheckPatterns;
begin
  if Length(FPatternOpen) = 0 then
    raise Exception.Create('Property "PatternOpen" must be set!');
  if Length(FPatternClose) = 0 then
    raise Exception.Create('Property "PatternClose" must be set!');
  if Length(FPatternEval) = 0 then
    raise Exception.Create('Property "PatternEval" must be set!');
end;

{ TSendFunction }

procedure TSendFunction.Execute;
begin
  Info.Caller.Result.AddString(Info.ValueAsString['s']);
end;

{ TSendLnFunction }

procedure TSendLnFunction.Execute;
var
   result : TdwsResult;
begin
   result:=Info.Caller.Result;
   result.AddString(Info.ValueAsString['s']);
   result.AddString(#13#10);
end;

{ TdwsHtmlUnit }

procedure TdwsHtmlUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  TSendFunction.Create(SymbolTable, 'Send', ['s', SYS_VARIANT], '', False);
  TSendLnFunction.Create(SymbolTable, 'SendLn', ['s', SYS_VARIANT], '', False);
end;

constructor TdwsHtmlUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'HTML';
end;

end.

