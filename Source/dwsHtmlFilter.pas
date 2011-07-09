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
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{    Contributor(s): Daniele Teti <d.teti@bittime.it>                  }
{                                                                      }
{**********************************************************************}

{$I dws.inc}

unit dwsHtmlFilter;

interface

uses
  Variants, Classes, SysUtils, dwsComp, dwsExprs, dwsFunctions, dwsSymbols,
  dwsErrors, dwsCompiler, dwsStrings, dwsStringResult, dwsUtils, StrUtils;

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
    procedure Execute(info : TProgramInfo); override;
  end;

  TSendLnFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
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
//  PrivateDependencies.Add('HTML');
  
  FPatternOpen := '<?pas';
  FPatternClose := '?>';
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
      if start>=stop then Exit;

      dest.WriteString('Print(');
      isQuoted:=False;
      lineCount:=0;
      for i:=start to stop do begin
         if isQuoted then begin
            case str[i] of
               '''': dest.WriteString('''''');
               #10: begin
                  dest.WriteString('''#10');
                  isQuoted:=False;
                  Inc(lineCount);
               end;
               #13: begin
                  dest.WriteString('''#13');
                  isQuoted:=False;
               end;
               #9: begin
                  dest.WriteString('''#9');
                  isQuoted:=False;
               end;
            else
               dest.WriteChar(str[i]);
            end
         end else begin
            case str[i] of
               '''': begin
                  dest.WriteString('''''''');
                  isQuoted:=True;
               end;
               #10: begin
                  dest.WriteString('#10');
                  Inc(lineCount);
               end;
               #13: dest.WriteString('#13');
               #9: dest.WriteString('#9');
            else
               dest.WriteChar('''');
               dest.WriteChar(str[i]);
               isQuoted:=True;
            end;
         end;
      end;

      if isQuoted then
         dest.WriteString('''');
      dest.WriteString(');');

      for i := 1 to lineCount do
         dest.WriteString(#13#10);
   end;

var
   p, start, stop : Integer;
   isEval : Boolean;
   input : String;
   output : TWriteOnlyBlockStream;
begin
   CheckPatterns;

   input:=inherited Process(Text, Msgs);

   output:=TWriteOnlyBlockStream.Create;
   try

      stop:=1;
      p:=1;
      repeat
         start:=PosEx(PatternOpen, input, p);
         if start<=0 then begin
            StuffString(input, p, Length(input), output);
            Break;
         end else StuffString(input, p, start-1, output);
         start:=start+Length(FPatternOpen);
         isEval:=CompareMem(@input[start], @FPatternEval[1], Length(FPatternEval));
         if isEval then begin
            output.WriteString('Print(');
            start:=start+Length(FPatternEval);
         end;
         stop:=PosEx(PatternClose, input, start);
         if stop<=0 then
            output.WriteSubString(input, start)
         else begin
            output.WriteSubString(input, start, stop-start);
            p:=stop+Length(FPatternClose);
         end;
         if isEval then
            output.WriteString(');');
      until (stop<=0);

      Result:=output.ToString;

   finally
      output.Free;
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

procedure TSendFunction.Execute(info : TProgramInfo);
begin
  Info.Execution.Result.AddString(Info.ValueAsString['s']);
end;

{ TSendLnFunction }

procedure TSendLnFunction.Execute(info : TProgramInfo);
var
   result : TdwsResult;
begin
   result:=Info.Execution.Result;
   result.AddString(Info.ValueAsString['s']);
   result.AddString(#13#10);
end;

{ TdwsHtmlUnit }

procedure TdwsHtmlUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  TSendFunction.Create(SymbolTable, 'Send', ['s', SYS_VARIANT], '', False);
  TSendFunction.Create(SymbolTable, 'Print', ['s', SYS_VARIANT], '', False);
  TSendLnFunction.Create(SymbolTable, 'SendLn', ['s', SYS_VARIANT], '', False);
  TSendLnFunction.Create(SymbolTable, 'PrintLn', ['s', SYS_VARIANT], '', False);
end;

constructor TdwsHtmlUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'HTML';
end;

end.

