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
  Variants,
  Classes, SysUtils, dwsComp, dwsExprs, dwsFunctions, dwsSymbols,
  dwsErrors, dwsCompiler;

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
    function Process(const Text: string; Msgs: TMsgs): string; override;
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

uses
  dwsStrings, dwsStringResult;

{ TdwsHtmlFilter }

constructor TdwsHtmlFilter.Create(AOwner: TComponent);
begin
  inherited;
  PrivateDependencies.Add('HTML');
  
  FPatternOpen := '<%';
  FPatternClose := '%>';
  FPatternEval := '=';
end;

function TdwsHtmlFilter.Process(const Text: string; Msgs: TMsgs): string;

  function StuffString(const Str: string): string;
  var
    IsQuoted: Boolean;
    x, LineCount: Integer;
  begin
    Result := '''';
    IsQuoted := True;
    LineCount := 0;
    for x := 1 to Length(Str) do
      if IsQuoted then
        case Str[x] of
          '''': Result := Result + '''''';
          #10:
            begin
              Result := Result + '''#10';
              IsQuoted := False;
              Inc(LineCount);
            end;
          #13:
            begin
              Result := Result + '''#13';
              IsQuoted := False;
            end;
          #9:
            begin
              Result := Result + '''#9';
              IsQuoted := False;
            end;
        else
          Result := Result + Str[x];
        end
      else
        case Str[x] of
          '''':
            begin
              Result := Result + '''''''';
              IsQuoted := True;
            end;
          #10:
            begin
              Result := Result + '#10';
              Inc(LineCount);
            end;
          #13: Result := Result + '#13';
          #9: Result := Result + '#9';
        else
          Result := Result + '''' + Str[x];
          IsQuoted := True;
        end;

    if IsQuoted then
      Result := Result + '''';

    for x := 1 to LineCount do
      Result := Result + #13#10;
  end;

var
  state: (sNone, sSend);
  index, patOpen, patClose, patEval: Integer;
  htmlText, chunk, pattern: string;
begin
  // Initializations
  htmlText := inherited Process(Text, Msgs);
  patOpen := Length(FPatternOpen) - 1;
  patClose := Length(FPatternClose) - 1;
  patEval := Length(FPatternEval) + 1;

  state := sNone;
  pattern := FPatternOpen;
  Result := '';

  // Start conversion
  repeat
    index := AnsiPos(pattern, htmlText);
    if index = 0 then
      index := Length(htmlText) + 1;

    case state of
      sNone:
        // Normal HTML code.
        // Looking for <%
        begin
          if index > 1 then
            Result := Result +
              Format('Send(%s);', [StuffString(Copy(htmlText, 1, index - 1))]);
          Delete(htmlText, 1, index + patOpen);
          pattern := FPatternClose;
          state := sSend;
        end;
      sSend:
        // Inside a <% %> tag
        // Looking for %>
        begin
          chunk := Copy(htmlText, 1, index - 1);
          if Pos(FPatternEval, chunk) = 1 then
            Result := Result + Format('Send(%s);', [Copy(chunk, patEval, Length(chunk))])
          else
            Result := Result + chunk;
          Delete(htmlText, 1, index + patClose);
          pattern := FPatternOpen;
          state := sNone;
        end;
    end;
  until Length(htmlText) = 0;
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
  TdwsStringResult(Info.Caller.Result).AddStr(VarToStr(Info.ValueAsVariant['s']));
end;

{ TSendLnFunction }

procedure TSendLnFunction.Execute;
begin
  TdwsStringResult(Info.Caller.Result).AddStr(VarToStr(Info.ValueAsVariant['s']) + #13#10);
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

