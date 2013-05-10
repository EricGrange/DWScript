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
{    The Initial Developer of the Original DWS Code is Matthias        }
{    Ackermann.                                                        }
{    For other initial contributors, see DWS contributors.txt          }
{    Ackermann.                                                        }
{    DWS code is currently maintained by Eric Grange.                  }
{                                                                      }
{    Current maintainer of the IDE utility: Brian Frost                }
{                                                                      }
{**********************************************************************}
unit UDwsIdeDefs;

interface

uses
  Windows,
  Themes,
  Classes,
  Graphics,
  dwsDebugger,
  SynEditHighlighter,
  dwsSuggestions,
  SynHighlighterDWS;

type

  IDwsIde = interface
    // IDwsIde
    // -------------------------------------------------------------------------
    function  DwsIde_GetDebugger : TdwsDebugger;
    // -------------------------------------------------------------------------
  end;

  TEditorHighlighterClass = class of TSynCustomHighlighter;

  TDwsIdeOptions = record
    EditorHighlighterClass : TEditorHighlighterClass;
    EditorFontName         : string;
    EditorFontSize         : integer;
    ScriptFolder           : string;
    ProjectName            : string;
  end;


  TSynDWSSyn_DelphiLookalike = class( TSynDWSSyn )
    constructor Create(AOwner: TComponent); override;
  end;


const
  IdeOptions_Legacy   : TDwsIdeOptions = (
    EditorHighlighterClass : TSynDWSSyn_DelphiLookalike;
    EditorFontName         : 'Courier New';
    EditorFontSize         : 10
    );

  IdeOptions_VistaOrLater : TDwsIdeOptions = (
    EditorHighlighterClass : TSynDWSSyn_DelphiLookalike;
    EditorFontName         : 'Consolas';
    EditorFontSize         : 11
    );

  SuggestionCategoryNames : array[TdwsSuggestionCategory] of string = (
    'Unknown',
    'Unit',
    'Type',
    'Class',
    'Record',
    'Interface',
    'Delegate',
    'Function',
    'Procedure',
    'Method',
    'Constructor',
    'Destructor',
    'Property',
    'Enum',
    'Element',
    'Parameter',
    'Variable',
    'Const',
    'ReservedWord',
    'SpecialFunction' );

  sDwsIdeProjectSourceFileExt   = '.dws';     // ext of the main file (like Delphi's dpr)
  sDwsIdeProjectSourceFileExt2  = '.pas';     // ext of units
  sDwsIdeProjectFileExt         = '.dwsproj'; // ext of the project file (like Delphi dproj)



// Utility routines
function BeginsWith( const ABeginsStr, AStr : string; AMatchCase : boolean = False ) : boolean;
// Returns TRUE if AStr begins with ABeginsStr

function DebuggerEvaluate( ADebugger : TDwsDebugger; const AExpression : string) : String;




implementation

uses
  SysUtils,
  variants,
  dwsCompiler;

function DebuggerEvaluate( ADebugger : TDwsDebugger; const AExpression : string) : String;
var
  expr : IdwsEvaluateExpr;
  V    : variant;
begin
   try
      expr:= ADebugger.Evaluate(AExpression);
      try
         Result:='(no result)';
         expr.Expression.EvalAsVariant( ADebugger.Execution.ExecutionObject, V );
         Result := VarToStr( V );
         if VarIsStr( V ) then
           Result := '''' + Result + '''';
      finally
         expr:=nil;
      end;
   except
      on E : Exception do
         Result:=E.Message;
   end;
end;


function BeginsWith( const ABeginsStr, AStr : string; AMatchCase : boolean = False ) : boolean;
// Returns TRUE if AStr begins with ABeginsStr

  function Min( A, B : integer ) : integer;
  begin
    If A <= B then
      Result := A
     else
       Result := B;
  end;

var
  I : integer;
begin
  Result := False;
  If ABeginsStr = '' then
    Exit;

  if AMatchCase then
    begin
    for I := 1 to Min( Length( ABeginsStr ), Length( AStr )) do
      If ABeginsStr[I] <> AStr[I] then
        Exit;
    end
   else
    for I := 1 to Min( Length( ABeginsStr ), Length( AStr )) do
      If UpCase(ABeginsStr[I]) <> UpCase(AStr[I]) then
        Exit;

  Result := True;

end;

{ TSynDWSSyn_DelphiLookalike }

constructor TSynDWSSyn_DelphiLookalike.Create(AOwner: TComponent);
begin
  inherited;
  LoadDelphiStyle;
end;

end.
