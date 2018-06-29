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
unit dwsHtmlFilter;

{$I dws.inc}

interface

uses
   Classes, SysUtils, StrUtils,
   dwsComp, dwsExprs, dwsFunctions, dwsSymbols, dwsExprList, dwsFilter,
   dwsErrors, dwsCompiler, dwsStrings, dwsUtils, dwsMagicExprs,
   dwsResultFunctions;

type

   TCheckPatternEvalFunc = function (p : PChar) : Integer of object;

   // TdwsHtmlFilter
   //
   TdwsHtmlFilter = class(TdwsFilter)
      private
         FPatternOpen : UnicodeString;
         FPatternOpenLength : Integer;
         FPatternClose : UnicodeString;
         FPatternCloseLength : Integer;
         FPatternEval : UnicodeString;
         FPatternEvalLength : Integer;
         FCheckPatternEval : TCheckPatternEvalFunc;

      protected
         procedure SetPatternOpen(const val : UnicodeString);
         procedure SetPatternClose(const val : UnicodeString);
         procedure SetPatternEval(const val : UnicodeString);

      public
         constructor Create(AOwner: TComponent); override;

         procedure CheckPatterns;
         function CheckEvalLong(p : PChar) : Integer;
         function CheckEvalChar(p : PChar) : Integer;

         function Process(const aText : String; msgs: TdwsMessageList) : String; override;

      published
         property PatternOpen : UnicodeString read FPatternOpen write SetPatternOpen;
         property PatternClose : UnicodeString read FPatternClose write SetPatternClose;
         property PatternEval : UnicodeString read FPatternEval write SetPatternEval;
   end;

   TdwsHtmlUnit = class(TdwsUnitComponent)
      protected
         procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;

      public
         constructor Create(AOwner: TComponent); override;
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

   PatternOpen:='<?pas';
   PatternClose:='?>';
   PatternEval:='=';
end;

// Process
//
function TdwsHtmlFilter.Process(const aText : String; msgs : TdwsMessageList) : String;
var
   endQuote : array [0..1] of Char;
   output : TWriteOnlyBlockStream;

   procedure StuffSpaces(n : Integer);
   const
      cManySpaces = '                ';
   begin
      case n of
         0 : ;
         1..Length(cManySpaces) :
            output.WriteSubString(cManySpaces, 1, n);
      else
         while n>Length(cManySpaces) do begin
            output.WriteSubString(cManySpaces, 1, Length(cManySpaces));
            Dec(n, Length(cManySpaces));
         end;
         StuffSpaces(n);
      end;
   end;

   procedure StuffString(const input : UnicodeString; start, stop : Integer);
   const
      // Truth table for #13, #10, #9 and ''''
      cSpecial : array [#0..''''] of Byte =
         (0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1);
   var
      isQuoted : Boolean;
      i, k, lineCount, nbSpaces : Integer;
      c : Char;
   begin
      if start>stop then Exit;

      if EditorMode then begin
         nbSpaces := 0;
         for i := start to stop do begin
            c := input[i];
            case c of
               #13, #10, #9 : begin
                  if nbSpaces > 0 then begin
                     StuffSpaces(nbSpaces);
                     nbSpaces := 0;
                  end;
                  output.WriteChar(c);
               end;
            else
               Inc(nbSpaces);
            end;
         end;
         if nbSpaces > 0 then
            StuffSpaces(nbSpaces);
         Exit;
      end;

      output.WriteString('Print(');
      isQuoted:=False;
      lineCount:=0;
      i:=start;
      while i<=stop do begin
         if isQuoted then begin
            k:=i;
            repeat
               c:=input[i];
               if (c<=High(cSpecial)) and (cSpecial[c]<>0) then
                  break
               else Inc(i);
            until i>stop;
            if i>k then begin
               output.WriteP(@input[k], i-k);
               if i>stop then break;
            end;
            case input[i] of
               '''':
                  output.WriteString('''''');
               #10: begin
                  output.WriteString('''#10');
                  isQuoted:=False;
                  Inc(lineCount);
               end;
               #13: begin
                  output.WriteString('''#13');
                  isQuoted:=False;
               end;
               #9: begin
                  output.WriteString('''#9');
                  isQuoted:=False;
               end;
            end
         end else begin
            case input[i] of
               '''': begin
                  output.WriteString('''''''');
                  isQuoted:=True;
               end;
               #10: begin
                  output.WriteString('#10');
                  Inc(lineCount);
               end;
               #13: output.WriteString('#13');
               #9: output.WriteString('#9');
            else
               endQuote[1]:=input[i];
               output.WriteP(@endQuote, 2);
               isQuoted:=True;
            end;
         end;
         Inc(i);
      end;

      if isQuoted then
         output.WriteString(''');')
      else output.WriteString(');');

      for i:=1 to lineCount do
         output.WriteCRLF;
   end;

var
   p, start, stop : Integer;
   isEval : Integer;
   input : UnicodeString;
   inputPtr : PChar;
begin
   CheckPatterns;

   endQuote[0]:='''';

   input := UnicodeString(inherited Process(aText, Msgs));
   inputPtr:=PChar(input);

   output:=TWriteOnlyBlockStream.AllocFromPool;
   try

      {$ifndef DELPHI_TOKYO_PLUS} stop:=1; {$endif}
      p:=1;
      repeat
         start:=PosEx(PatternOpen, input, p);
         if start<=0 then begin
            StuffString(input, p, Length(input));
            Break;
         end else StuffString(input, p, start-1);
         start:=start+FPatternOpenLength;
         if EditorMode then StuffSpaces(FPatternOpenLength-1);

         isEval:=FCheckPatternEval(@inputPtr[start-1]);
         if isEval=0 then begin
            start:=start+FPatternEvalLength;
            if EditorMode then begin
               StuffSpaces(FPatternEvalLength);
               output.WriteString('(');
            end else output.WriteString('Print(');
         end else if EditorMode then StuffSpaces(1);

         stop:=PosEx(PatternClose, input, start);
         if stop<=0 then
            output.WriteSubString(input, start)
         else begin
            output.WriteP(@inputPtr[start-1], stop-start);
            p:=stop+FPatternCloseLength;
            if EditorMode then StuffSpaces(FPatternCloseLength-2);
         end;

         if isEval=0 then
            output.WriteString(');')
         else if EditorMode then StuffSpaces(2);

      until (stop<=0);

      Result:=output.ToString;

   finally
      output.ReturnToPool;
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

// CheckEvalLong
//
function TdwsHtmlFilter.CheckEvalLong(p : PChar) : Integer;
begin
   Result:=Ord(not CompareMem(p, Pointer(FPatternEval), FPatternEvalLength));
end;

// CheckEvalChar
//
function TdwsHtmlFilter.CheckEvalChar(p : PChar) : Integer;
begin
   Result:=(Ord(FPatternEval[1])-Ord(p^));
end;

// SetPatternOpen
//
procedure TdwsHtmlFilter.SetPatternOpen(const val : UnicodeString);
begin
   FPatternOpen:=val;
   FPatternOpenLength:=Length(val);
end;

// SetPatternClose
//
procedure TdwsHtmlFilter.SetPatternClose(const val : UnicodeString);
begin
   FPatternClose:=val;
   FPatternCloseLength:=Length(val);
end;

// SetPatternEval
//
procedure TdwsHtmlFilter.SetPatternEval(const val : UnicodeString);
begin
   FPatternEval:=val;
   FPatternEvalLength:=Length(val);
   if FPatternEvalLength=1 then
      FCheckPatternEval:=CheckEvalChar
   else FCheckPatternEval:=CheckEvalLong;
end;

{ TdwsHtmlUnit }

procedure TdwsHtmlUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
   TPrintFunction.Create(SymbolTable, 'Send', ['s', SYS_VARIANT], '', [iffDeprecated]);
   TPrintLnFunction.Create(SymbolTable, 'SendLn', ['s', SYS_VARIANT], '', [iffDeprecated]);

   RegisterStandardResultFunctions(SymbolTable);
end;

constructor TdwsHtmlUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'HTML';
end;

end.

