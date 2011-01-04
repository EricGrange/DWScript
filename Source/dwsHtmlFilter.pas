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
  dwsErrors, dwsCompiler, dwsStrings, dwsStringResult, dwsUtils;

type

   // TdwsHtmlFilter
   //
   TdwsHtmlFilter = class(TdwsFilter)
      private
         FPatternOpen: string;
         FPatternClose: string;
         FPatternEval: string;

      public
         constructor Create(AOwner: TComponent); override;

         procedure CheckPatterns;

         function Process(const Text: string; Msgs: TdwsMessageList): string; override;

      published
         property PatternClose: string read FPatternClose write FPatternClose;
         property PatternEval: string read FPatternEval write FPatternEval;
         property PatternOpen: string read FPatternOpen write FPatternOpen;
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

  EHTMLFilterException = class (Exception) end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{ TdwsHtmlFilter }

constructor TdwsHtmlFilter.Create(AOwner: TComponent);
begin
  inherited;
  PrivateDependencies.Add('HTML');
  
  FPatternOpen := '<%';
  FPatternClose := '%>';
  FPatternEval := '=';
end;

// Process
//
function TdwsHtmlFilter.Process(const Text: String; Msgs: TdwsMessageList): String;

   procedure StuffString(const str: String; start, stop : Integer;
                         dest : TWriteOnlyBlockStream);
   var
      isQuoted: Boolean;
      i, lineCount: Integer;
   begin
      dest.WriteString('''');
      isQuoted := True;
      lineCount := 0;
      for i := start to stop do begin
         if isQuoted then begin
            case str[i] of
               '''': dest.WriteString('''''');
               #10: begin
                  dest.WriteString('''#10');
                  isQuoted := False;
                  Inc(lineCount);
               end;
               #13: begin
                  dest.WriteString('''#13');
                  isQuoted := False;
               end;
               #9: begin
                  dest.WriteString('''#9');
                  isQuoted := False;
               end;
            else
               dest.WriteString(str[i]);
            end
         end else begin
            case str[i] of
               '''': begin
                  dest.WriteString('''''''');
                  isQuoted := True;
               end;
               #10: begin
                  dest.WriteString('#10');
                  Inc(lineCount);
               end;
               #13: dest.WriteString('#13');
               #9: dest.WriteString('#9');
            else
               dest.WriteString('''');
               dest.WriteString(Str[i]);
               isQuoted := True;
            end;
         end;
      end;

      if isQuoted then
         dest.WriteString('''');

      for i := 1 to lineCount do
         dest.WriteString(#13#10);
   end;

var
   state: (sNone, sSend);
   index, patOpen, patClose, patEval: Integer;
   htmlText, chunk, pattern: String;
   builder : TWriteOnlyBlockStream;
begin
   CheckPatterns;

   // Initializations
   htmlText := inherited Process(Text, Msgs);
   patOpen := Length(FPatternOpen) - 1;
   patClose := Length(FPatternClose) - 1;
   patEval := Length(FPatternEval) + 1;

   builder:=TWriteOnlyBlockStream.Create;
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
                  builder.WriteString('Send(');
                  StuffString(htmlText, 1, index-1, builder);
                  builder.WriteString(');');
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
                  builder.WriteString('Send(');
                  builder.WriteString(Copy(chunk, patEval, Length(chunk)));
                  builder.WriteString(');');
               end else begin
                  builder.WriteString( chunk );
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

procedure TdwsHtmlFilter.CheckPatterns;
begin
   if FPatternOpen='' then
      raise EHTMLFilterException.Create('Property "PatternOpen" must be set!');
   if FPatternClose='' then
      raise EHTMLFilterException.Create('Property "PatternClose" must be set!');
   if FPatternEval='' then
      raise EHTMLFilterException.Create('Property "PatternEval" must be set!');
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

