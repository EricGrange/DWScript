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
unit UDwsIdeForm;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Themes, UxTheme,
  UDwsIdeDefs,
  dwsExprs,
  dwsComp,
  dwsCompiler,
  dwsDebugger,
  dwsErrors,
  dwsFunctions,
  dwsUtils, dwsSymbols, dwsUnitSymbols,
  Dialogs, StdCtrls,
  ExtCtrls,
  SynEditHighlighter,
  SynHighlighterDWS,
  SynEditTypes,
  SynEditKeyCmds,
  UDwsIdeConfig,
  Diagnostics,
  SynEdit,
  ActnList,
  ComCtrls,
  XMLIntf,
  XMLDoc,
  UDwsIdeCodeProposalForm,
  Menus, ToolWin, ActnCtrls,
  ImgList, UDwsIdeLocalVariablesFrame, UDwsIdeWatchesFrame, UDwsIdeCallStackFrame,
  StdActns;


const
  WM_CodeSuggest = 1024;

type
  EDwsIde      = class( Exception );

  TDwsIdeForm  = class;
  TEditorPage  = class;

  TBreakpointStatus = (bpsNone, bpsBreakpoint, bpsBreakpointDisabled );

  TExecutableLines = array of boolean;

  TLineNumbers = array of integer;

  TEditorPage = class( TWinControl )
  private
    FEditor : TSynEdit;
    FForm   : TDwsIdeForm;
    FExecutableLines : TExecutableLines;
    FTabLeft : Integer;
    FTabWidth : Integer;

    function TabRight : Integer;
    function CloseButtonRect : TRect;

    function  GetFilename: string;
    procedure SetFileName(const Value: string);

    procedure PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
                      FirstLine, LastLine: integer);
    procedure SetCurrentLine(ALine: integer);
    procedure SynEditorClick(Sender: TObject);
    procedure SynEditorKeyDown( Sender: TObject; var Key: Word;  Shift: TShiftState );
    procedure SynEditorSpecialLineColors(Sender: TObject;
                 Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure SynEditorGutterClick(Sender: TObject; Button: TMouseButton; X, Y,
      Line: Integer; Mark: TSynEditMark);
    procedure SynEditorCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    function GetIsReadOnly: boolean;
    procedure SetIsReadOnly(const Value: boolean);
    function GetIsProjectSourcefile: boolean;
    function GetBreakpointStatus(ALine: integer): TBreakpointStatus;
    function IsExecutableLine(ALine: integer): boolean;
    procedure DoOnEditorChange(ASender: TObject);
    procedure AddBreakpoint( ALineNum : integer; AEnabled : boolean );
    procedure ClearBreakpoint( ALineNum : integer );
    procedure ClearExecutableLines;
    procedure InitExecutableLines;
  PUBLIC

    FCurrentLine : integer;

    constructor Create(
                    AOwner    : TDwsIdeForm;
              const AFileName : string;
                    ALoadFile : boolean); reintroduce;
    destructor  Destroy; override;

    property  Editor : TSynEdit
                read FEditor;

    property  FileName : string
                read GetFilename
                write SetFileName;

    property  IsReadOnly : boolean
                read GetIsReadOnly
                write SetIsReadOnly;

    property  IsProjectSourcefile : boolean
                read GetIsProjectSourcefile;

    procedure SaveToFile( APromptOverwrite : boolean );
    procedure SaveIfModified( APromptOverwrite : boolean );
    procedure SaveAs;

    function  UnitName : string;

    procedure ShowExecutableLines;

  end;


  TDwsIdeForm = class(TForm, IDwsIde)
    ActionList1: TActionList;
    actOpenFile: TAction;
    pnlEditor: TPanel;
    actClosePage: TAction;
    EditorPageTabContextMenu: TPopupMenu;
    CloseFile1: TMenuItem;
    OpenFileDialog: TFileOpenDialog;
    actCloseAllOtherPages: TAction;
    CloseAllOtherPages1: TMenuItem;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    OpenFile1: TMenuItem;
    SmallImages: TImageList;
    miPages: TMenuItem;
    N1: TMenuItem;
    actExit: TAction;
    N2: TMenuItem;
    Exit1: TMenuItem;
    New1: TMenuItem;
    actSaveProjectAs: TAction;
    SaveProjectDialog: TFileSaveDialog;
    N3: TMenuItem;
    SaveProjectAs1: TMenuItem;
    actFileNewProject: TAction;
    NewProject2: TMenuItem;
    actFileNewUnit: TAction;
    FileNewUnit1: TMenuItem;
    actFileNewIncludeFile: TAction;
    NewIncludeFile1: TMenuItem;
    actFileSave: TAction;
    Save1: TMenuItem;
    View1: TMenuItem;
    actViewProjectSource: TAction;
    ViewProjectSource1: TMenuItem;
    actFileCloseAll: TAction;
    ClosePage1: TMenuItem;
    CloseAll1: TMenuItem;
    actOpenProject: TAction;
    OpenProject1: TMenuItem;
    OpenProjectDialog: TFileOpenDialog;
    StatusBar: TStatusBar;
    UpdateTimer: TTimer;
    actToggleReadOnly: TAction;
    N4: TMenuItem;
    ReadOnly1: TMenuItem;
    actRun: TAction;
    Run1: TMenuItem;
    Run2: TMenuItem;
    N5: TMenuItem;
    Edit1: TMenuItem;
    ReadOnly2: TMenuItem;
    dwsDebugger1: TdwsDebugger;
    Project1: TMenuItem;
    actBuild: TAction;
    Build1: TMenuItem;
    actClearAllBreakpoints: TAction;
    ClearAllBreakpoints1: TMenuItem;
    actProgramReset: TAction;
    Reset1: TMenuItem;
    actStepOver: TAction;
    StepOver1: TMenuItem;
    actTraceInto: TAction;
    raceInto1: TMenuItem;
    actRunWithoutDebugging: TAction;
    actRunWithoutDebugging1: TMenuItem;
    Panel2: TPanel;
    Splitter1: TSplitter;
    actFileSaveAs: TAction;
    SaveAs1: TMenuItem;
    SaveSourceDialog: TFileSaveDialog;
    Save2: TMenuItem;
    SaveAs2: TMenuItem;
    N7: TMenuItem;
    actShowExecutionPoint: TAction;
    ShowExecutionPoint1: TMenuItem;
    DwsIdeLocalVariablesFrame: TDwsIdeLocalVariablesFrame;
    DwsIdeWatchesFrame: TDwsIdeWatchesFrame;
    DwsIdeCallStackFrame: TDwsIdeCallStackFrame;
    actViewSymbols: TAction;
    ViewSymbols1: TMenuItem;
    EditorPagePopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N6: TMenuItem;
    Copy1: TMenuItem;
    actEditorSelectAll: TEditSelectAll;
    actEditorCopyToClipboard: TEditCopy;
    SelectAll1: TMenuItem;
    actEditorCut: TEditCut;
    Cut1: TMenuItem;
    actEditorPaste: TEditPaste;
    Paste1: TMenuItem;
    N8: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    SelectAll2: TMenuItem;
    actEditorDelete: TEditDelete;
    Delete1: TMenuItem;
    Delete2: TMenuItem;
    actRunProcedureAtCursor: TAction;
    RunProcedureAtCursor1: TMenuItem;
    RunProcedureAtCursor2: TMenuItem;
    N9: TMenuItem;
    Suggest1: TMenuItem;
    actCodeProposalInvoke: TAction;
    pnlPageControl: TPanel;
    imgTabs: TImage;
    procedure EditorChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actClosePageExecute(Sender: TObject);
    procedure pcEditorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actCloseAllOtherPagesExecute(Sender: TObject);
    procedure actClosePageUpdate(Sender: TObject);
    procedure actCloseAllOtherPagesUpdate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actSaveProjectAsExecute(Sender: TObject);
    procedure actFileNewProjectExecute(Sender: TObject);
    procedure actFileNewUnitExecute(Sender: TObject);
    procedure actFileNewIncludeFileExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actViewProjectSourceExecute(Sender: TObject);
    procedure actViewProjectSourceUpdate(Sender: TObject);
    procedure actFileCloseAllExecute(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure actToggleReadOnlyExecute(Sender: TObject);
    procedure actToggleReadOnlyUpdate(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actRunUpdate(Sender: TObject);
    procedure actRunUnitTestsUpdate(Sender: TObject);
    procedure actBuildExecute(Sender: TObject);
    procedure dwsDebugger1Debug(exec: TdwsExecution; expr: TExprBase);
    procedure dwsDebugger1DebugStart(exec: TdwsExecution);
    procedure dwsDebugger1DebugStop(exec: TdwsExecution);
    procedure dwsDebugger1EnterFunc(exec: TdwsExecution; expr: TExprBase);
    procedure dwsDebugger1LeaveFunc(exec: TdwsExecution; expr: TExprBase);
    procedure dwsDebugger1StateChanged(Sender: TObject);
    procedure actClearAllBreakpointsExecute(Sender: TObject);
    procedure actProgramResetExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStepOverUpdate(Sender: TObject);
    procedure actTraceIntoExecute(Sender: TObject);
    procedure actTraceIntoUpdate(Sender: TObject);
    procedure actRunWithoutDebuggingExecute(Sender: TObject);
    procedure actRunWithoutDebuggingUpdate(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveAsUpdate(Sender: TObject);
    procedure actShowExecutionPointExecute(Sender: TObject);
    procedure actShowExecutionPointUpdate(Sender: TObject);
    procedure actProgramResetUpdate(Sender: TObject);
    procedure actViewSymbolsExecute(Sender: TObject);
    procedure actViewSymbolsUpdate(Sender: TObject);
    constructor Create( AOwner : TComponent; const AOptions : TDwsIdeOptions ); reintroduce;
    procedure FormShow(Sender: TObject);
    procedure actEditorCopyToClipboardExecute(Sender: TObject);
    procedure actEditorCopyToClipboardUpdate(Sender: TObject);
    procedure actEditorSelectAllExecute(Sender: TObject);
    procedure actEditorSelectAllUpdate(Sender: TObject);
    procedure actEditorCutExecute(Sender: TObject);
    procedure actEditorCutUpdate(Sender: TObject);
    procedure actEditorPasteExecute(Sender: TObject);
    procedure actEditorPasteUpdate(Sender: TObject);
    procedure actEditorDeleteExecute(Sender: TObject);
    procedure actEditorDeleteUpdate(Sender: TObject);
    procedure actRunProcedureAtCursorExecute(Sender: TObject);
    procedure actRunProcedureAtCursorUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WMCodeSuggest( var AMessage : TMessage ); message WM_CodeSuggest;
    procedure actCodeProposalInvokeExecute(Sender: TObject);
    procedure pnlPageControlResize(Sender: TObject);
    procedure imgTabsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgTabsMouseLeave(Sender: TObject);
  private
    { Private declarations }
    FScript : TDelphiWebScript;

    FpcEditorLastMouseButton : TMouseButton;
    FpcEditorLastMouseXY     : TPoint;

    FProjectFileName : string;

    FProgram : IdwsProgram;

    FScriptFolder: string;

    FOptions : TDwsIdeOptions;

    FIDEFormRect : TRect;

    FCodeProposalForm : TDwsIdeCodeProposalForm;

    FActivePageIndex : Integer;
    FHoveredPageIndex : Integer;
    FBasePageIndex : Integer;
    FHoveredCloseButton : Boolean;
    FHoveredLeftArrow, FHoveredRightArrow : Boolean;
    FLeftArrowActive, FRightArrowActive : Boolean;
    FTabArrowLeft, FTabArrowRight : TRect;
    FPages : TSimpleList<TEditorPage>;

    procedure CodeSuggest( ACodeSuggestionMode : TCodeSuggestionMode);
    procedure DoOnCodeSuggestionFormSelectItem( const AItemText : string );
    procedure EditorPageAddNew( const AFileName : string; ALoadfile : boolean  );
    function  ProjectSourceScript : string;
    function  EditorPageCount : integer;
    function  EditorPage(AIndex: integer) : TEditorPage;
    function  CurrentEditorPage : TEditorPage;
    procedure EditorPageClose( AIndex : integer );
    procedure EditorCloseAllPages( AExceptIndex : integer = -1 );
    procedure EditorSaveAllIfModified( APromptOverwrite : boolean );
    function  HasProject : boolean;
    function  NameToEditorPageIndex( const AName : string ) : integer;

    procedure SetEditorCurrentPageIndex(const Value: integer);
    procedure SetProjectFileName(const Value: string);
    procedure GotoScriptPos(AScriptPos: TScriptPos);
    procedure ResetProgram;
    function  GetExecutableLines(const AUnitName: string): TLineNumbers;
    procedure SetScript(const Value: TDelphiWebScript);
    procedure SetScriptFolder(const Value: string);
    property  EditorCurrentPageIndex : integer
                read FActivePageIndex
                write SetEditorCurrentPageIndex;
    function  CurrentEditor : TSynEdit;
    function  HasEditorPage : boolean;
    function  GetProjectSourceFileName: string;
    procedure SetProjectSourceFileName(const Value: string);
    property  ProjectSourceFileName : string
                read GetProjectSourceFileName
                write SetProjectSourceFileName;

    function  FileIsOpenInEditor( const AFileName : string ) : boolean;
    function  FileIsProjectSource( const AFileName : string ) : boolean;
    function  ProjectSourceFileIndex : integer;
    function  ModifyFileNameToUniqueInProject( const AFileName : string ) : string;
    function  SaveProjectAs : boolean;
    function  ProjectfileNameToProjectSourceFileName( const AProjectfileName : string ) : string;

    procedure DoOnClickEditorPageTabContextMenuPageItem( ASender : TObject );

    procedure LoadProjectFile( const AProjectFileName : string );
    procedure NewProjectFile( const AProjectFileName : string );
    procedure SaveProjectFileAs(const AProjectFileName: string);

    procedure ShowExecutableLines;
    procedure ClearCurrentLine;
    procedure ClearAllBreakpoints;
    procedure ClearExecutableLines;
    procedure AddStatusMessage( const AStr : string );
    procedure Compile( ABuild : boolean );
    function  IsCompiled: boolean;
    property  ScriptFolder : string
                read FScriptFolder
                write SetScriptFolder;
    procedure ListSymbols;

    procedure LoadSettings(
           var AProjectFileName : string;
           var AIDEFormRect     : TRect );
    procedure SaveSettings(
         const AProjectFileName : string;
         const AIDEFormRect     : TRect );

    procedure RunProcedureByName( const AName : string );

    procedure RefreshTabs;
    procedure RefreshTabArrows;
    function IndexOfTab(x : Integer) : Integer;

  PUBLIC
    // IDwsIde
    // -------------------------------------------------------------------------
    function  DwsIde_GetDebugger : TdwsDebugger;
    // -------------------------------------------------------------------------

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;


    property ProjectFileName : string
               read FProjectFileName
               write SetProjectFileName;

    property Script : TDelphiWebScript
               read FScript
               write SetScript;

  end;


procedure DwsIDE_ShowModal( AScript : TDelphiWebScript ); overload;
procedure DwsIDE_ShowModal( AScript : TDelphiWebScript; const AOptions : TDwsIdeOptions ); overload;

implementation

{$R *.dfm}

uses
  Registry,
  dwsSuggestions,
  SynHighlighterPas,
  ShlObj;


const
  sProjectSourceFileExt = '.dws';
  sProjectFileExt       = '.dwsproj';
  sMainModule           = '*MainModule*';

  iiExecutableLine        = 13;
  iiForwardArrow          = 16;
  iiCurrentLineBreakpoint = 15;
  iiBreakpoint            = 12;
  iiBreakpointDisabled    = 14;


// Utility routines
// -----------------------------------------------------------------------------

function GetDesktopPath: string;
const
  CSIDL_APPDATA = $001A;
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_DESKTOP, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;











procedure DwsIDE_ShowModal( AScript : TDelphiWebScript );
begin
  DwsIDE_ShowModal( AScript, IdeOptions_Style1 );
end;


procedure DwsIDE_ShowModal( AScript : TDelphiWebScript; const AOptions : TDwsIdeOptions );
var
  Frm : TDwsIdeForm;
begin
  Frm := TDwsIdeForm.Create( Application, AOptions );
  try
    Frm.Script := AScript;
    Frm.ShowModal;
  finally
    Frm.Free;
  end;
end;



function TextFileToString( const AfileName : string ) : string;
// Reads this string from a text file
var
  SL : TStrings;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile( AFileName );
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure StringToTextFile( const AString, AfileName : string );
// Writes this string to a text file
var
  SL : TStrings;
begin
  SL := TStringList.Create;
  try
    SL.Text := AString;
    SL.SaveToFile( AFileName );
  finally
    SL.Free;
  end;
end;

function JustFileName( const AFileName : string ) : string;
// Returns only the file name without dir or ext
begin
  Result := ChangeFileExt( ExtractFileName( AFileName ), '' );
end;


procedure ErrorDlg(const AStr: string);
begin
  TaskMessageDlg( 'Error', AStr, mtError, [mbok], 0 );
end;

function ConfirmDlg(const AStr: string): boolean;
begin
  Result := TaskMessageDlg( 'Confirm', AStr, mtError, [mbYes, mbNo], 0 ) = idYes;
end;

function ConfirmDlgYesNoAbort(const AStr: string): boolean;
begin
  Result := False;
  Case TaskMessageDlg( 'Confirm', AStr, mtError, [mbYes, mbNo, mbCancel], 0 ) of
    idYes : Result := True;
    idNo  : Exit;
   else
     Abort;
  end;
end;



procedure SymbolsToStrings( ATable : TSymbolTable; AStrings: TStrings);
// Dumps this table symbol names to AStrings recursively.

  procedure AddSymbolTable( ATable : TSymbolTable );
  var
    I : integer;
    Sym : TSymbol;
  begin
    for I := 0 to ATable.Count-1 do
      begin
      Sym := ATable.Symbols[I];
      if Sym is TUnitSymbol then
        AddSymbolTable( TUnitSymbol(Sym).Table )
       else
        AStrings.Add( Sym.Name + '   ' + Sym.ToString +  '  (' + Sym.ClassName + ')' );
      end;
  end;

begin
  AddSymbolTable( ATable );
end;






{ TEditorPageSynEditPlugin }

type
  TEditorPageSynEditPlugin = class(TSynEditPlugin)
  protected
    FPage : TEditorPage;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(APage : TEditorPage);
  end;

constructor TEditorPageSynEditPlugin.Create(APage : TEditorPage);
begin
  inherited Create( APage.Editor);
  FPage := APage;
end;

procedure TEditorPageSynEditPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: integer);
begin
  FPage.PaintGutterGlyphs(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TEditorPageSynEditPlugin.LinesInserted(FirstLine, Count: integer);
var
  I, iLineCount : integer;
begin
  // Track the executable lines
  iLineCount := FPage.Editor.Lines.Count;
  SetLength( FPage.FExecutableLines, iLineCount );
  for I := iLineCount-1 downto FirstLine + Count do
    FPage.FExecutableLines[i] := FPage.FExecutableLines[I-Count];
  for I := FirstLine + Count-1 downto FirstLine do
    FPage.FExecutableLines[i] := False;

  // Track the breakpoint lines in the debugger
  for I := 0 to FPage.FForm.dwsDebugger1.Breakpoints.Count-1 do
    If FPage.FForm.dwsDebugger1.Breakpoints[I].SourceName = FPage.UnitName then
      If FPage.FForm.dwsDebugger1.Breakpoints[I].Line >= FirstLine then
         FPage.FForm.dwsDebugger1.Breakpoints[I].Line :=
           FPage.FForm.dwsDebugger1.Breakpoints[I].Line + Count;

  // Redraw the gutter for updated icons.
  FPage.Editor.InvalidateGutter;
end;

procedure TEditorPageSynEditPlugin.LinesDeleted(FirstLine, Count: integer);
var
  I : integer;
begin
  // Track the executable lines
  for I := FirstLine-1 to Length( FPage.FExecutableLines )-1 do
    FPage.FExecutableLines[i] := FPage.FExecutableLines[I+Count];
  SetLength( FPage.FExecutableLines, Length( FPage.FExecutableLines )-Count );

  // Track the breakpoint lines in the debugger
  for I := 0 to FPage.FForm.dwsDebugger1.Breakpoints.Count-1 do
    If FPage.FForm.dwsDebugger1.Breakpoints[I].SourceName = FPage.UnitName then
      If FPage.FForm.dwsDebugger1.Breakpoints[I].Line >= FirstLine then
         FPage.FForm.dwsDebugger1.Breakpoints[I].Line :=
           FPage.FForm.dwsDebugger1.Breakpoints[I].Line - Count;

  // Redraw the gutter for updated icons.
  FPage.Editor.InvalidateGutter;

end;



{ TDwsIdeForm }

procedure TDwsIdeForm.actClearAllBreakpointsExecute(Sender: TObject);
begin
  ClearAllBreakpoints;
end;



procedure TDwsIdeForm.Compile( ABuild : boolean );
begin
  if ABuild or not IsCompiled then
    begin
    FScript.Config.CompilerOptions :=
      FScript.Config.CompilerOptions + [coSymbolDictionary];

    FProgram := FScript.Compile( ProjectSourceScript );

    if FProgram.Msgs.Count = 0 then // no errors
      AddStatusMessage( 'Compiled' )
     else
       begin
       GotoScriptPos( FProgram.Msgs.LastMessagePos );
       ErrorDlg( FProgram.Msgs.AsInfo );
       AddStatusMessage( 'Error(s)' );
       FProgram := nil;
       end
    end;
end;


constructor TDwsIdeForm.Create(AOwner: TComponent; const AOptions : TDwsIdeOptions );
begin
  inherited Create( AOwner );

  FOptions := AOptions;
end;

procedure TDwsIdeForm.actBuildExecute(Sender: TObject);
begin
  Compile( True );
  if IsCompiled then
    ShowExecutableLines;
end;

procedure TDwsIdeForm.actCloseAllOtherPagesExecute(Sender: TObject);
begin
  EditorCloseAllPages( EditorCurrentPageIndex );
end;

procedure TDwsIdeForm.EditorCloseAllPages( AExceptIndex : integer = -1 );
var
  I : integer;
begin
  for I := EditorPageCount-1 downto 0 do
    If I <> AExceptIndex then
      EditorPageClose( I );
end;

function TDwsIdeForm.NameToEditorPageIndex( const AName : string ) : integer;
begin
  for Result := 0 to EditorPageCount-1 do
    If SameText( EditorPage( Result ).UnitName, AName ) then
      Exit;
  Result := -1;
end;

procedure TDwsIdeForm.actCloseAllOtherPagesUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := EditorPageCount > 1;
end;

procedure TDwsIdeForm.actClosePageExecute(Sender: TObject);
begin
  EditorPageClose( EditorCurrentPageIndex );
end;

procedure TDwsIdeForm.actClosePageUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := EditorCurrentPageIndex <> -1;
end;



procedure TDwsIdeForm.actCodeProposalInvokeExecute(Sender: TObject);
begin
  CodeSuggest( csCodeProposal );
end;

procedure TDwsIdeForm.actEditorCopyToClipboardExecute(Sender: TObject);
begin
  CurrentEditor.CopyToClipboard;
end;

procedure TDwsIdeForm.actEditorCopyToClipboardUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.actEditorCutExecute(Sender: TObject);
begin
  CurrentEditor.CutToClipboard;
end;

procedure TDwsIdeForm.actEditorCutUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.actEditorDeleteExecute(Sender: TObject);
begin
  CurrentEditor.ClearSelection;
end;

procedure TDwsIdeForm.actEditorDeleteUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.actEditorPasteExecute(Sender: TObject);
begin
  CurrentEditor.PasteFromClipboard;
end;

procedure TDwsIdeForm.actEditorPasteUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.actEditorSelectAllExecute(Sender: TObject);
begin
  CurrentEditor.SelectAll;
  {$Message 'need self-contained editor page with own actions'}
end;

procedure TDwsIdeForm.actEditorSelectAllUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDwsIdeForm.actFileNewProjectExecute(Sender: TObject);
var
  sFileName : string;
begin
  sFileName := Format(
    '%sProject1%s',
    [IncludeTrailingBackslash(FScriptFolder), sProjectFileExt] );
  sFileName := ModifyFileNameToUniqueInProject( sFileName );

  NewProjectFile( sFileName );

  sFileName := ProjectfileNameToProjectSourceFileName( sFileName );
  EditorPageAddNew( sFileName, False );

end;

procedure TDwsIdeForm.actFileCloseAllExecute(Sender: TObject);
begin
  EditorCloseAllPages;
  ProjectFileName := '';
end;

procedure TDwsIdeForm.actFileNewIncludeFileExecute(Sender: TObject);
var
  sFileName : string;
begin
  sFileName := Format(
    '%sIncludeFile1.inc',
    [IncludeTrailingBackslash(FScriptFolder)] );
  sFileName := ModifyFileNameToUniqueInProject( sFileName );

  EditorPageAddNew( sFileName, False );
end;

procedure TDwsIdeForm.actFileNewUnitExecute(Sender: TObject);
var
  sFileName : string;
begin
  sFileName := Format(
    '%sUnit1.pas',
    [IncludeTrailingBackslash(FScriptFolder)] );
  sFileName := ModifyFileNameToUniqueInProject( sFileName );

  EditorPageAddNew( sFileName, False );
end;

procedure TDwsIdeForm.actFileSaveAsExecute(Sender: TObject);
begin
  if HasEditorPage then
    CurrentEditorPage.SaveAs;
end;

procedure TDwsIdeForm.actFileSaveAsUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.actFileSaveExecute(Sender: TObject);
begin
  if HasEditorPage then
    CurrentEditorPage.SaveToFile( False {dont prompt} );
end;

procedure TDwsIdeForm.EditorSaveAllIfModified( APromptOverwrite : boolean );
var
  I : integer;
begin
  for I := 0 to EditorPageCount-1 do
    EditorPage( I ).SaveIfModified( APromptOverwrite );
end;

procedure TDwsIdeForm.actFileSaveUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasEditorPage and CurrentEditor.Modified;
end;

procedure TDwsIdeForm.actSaveProjectAsExecute(Sender: TObject);
begin
  SaveProjectAs;
end;

procedure TDwsIdeForm.actShowExecutionPointExecute(Sender: TObject);
begin
  GotoScriptPos( dwsDebugger1.CurrentScriptPos );
end;

procedure TDwsIdeForm.actShowExecutionPointUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := dwsDebugger1.State = dsDebugSuspended;
end;

procedure TDwsIdeForm.actStepOverExecute(Sender: TObject);
begin
  dwsDebugger1.StepOver;
end;

procedure TDwsIdeForm.actStepOverUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := daCanStep in dwsDebugger1.AllowedActions;
end;

procedure TDwsIdeForm.actToggleReadOnlyExecute(Sender: TObject);
var
  Page : TEditorPage;
begin
  Page := CurrentEditorPage;
  Page.IsReadOnly := not Page.IsReadOnly;
end;

procedure TDwsIdeForm.actToggleReadOnlyUpdate(Sender: TObject);
begin
  With Sender as TAction do
    begin
    Enabled := HasEditorPage;
    Checked := Enabled and CurrentEditorPage.IsReadOnly;
    end;
end;

procedure TDwsIdeForm.actTraceIntoExecute(Sender: TObject);
begin
  dwsDebugger1.StepDetailed;
end;

procedure TDwsIdeForm.actTraceIntoUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := daCanStep in dwsDebugger1.AllowedActions;
end;

procedure TDwsIdeForm.actViewProjectSourceExecute(Sender: TObject);
var
  I : integer;
begin
  I := ProjectSourceFileIndex;
  If I >= 0 then
    EditorCurrentPageIndex := I
   else
    EditorPageAddNew( ProjectfileNameToProjectSourceFileName( ProjectFileName ), True );
end;

procedure TDwsIdeForm.actViewProjectSourceUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := ProjectFileName <> '';
end;



procedure TDwsIdeForm.actViewSymbolsExecute(Sender: TObject);
begin
  ListSymbols;
end;

procedure TDwsIdeForm.actViewSymbolsUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := IsCompiled;
end;

function TDwsIdeForm.SaveProjectAs : boolean;
var
  sFilename : string;
begin
  SaveProjectDialog.FileName := ExtractFileName(FProjectFileName);
  Result := SaveProjectDialog.Execute;
  If Result then
    begin
    sFilename := SaveProjectDialog.FileName;
    ProjectSourceFileName := ProjectfileNameToProjectSourceFileName( SaveProjectDialog.FileName );
    SaveProjectFileAs( sFileName );
    end;

end;

procedure TDwsIdeForm.AddStatusMessage(const AStr: string);
begin
  StatusBar.Panels[3].Text := AStr;
end;

procedure TDwsIdeForm.AfterConstruction;
var
  sProjectFileName : string;
begin
  inherited;

  // Set up callback links
  DwsIdeLocalVariablesFrame.DwsIde := Self;
  DwsIdeWatchesFrame.DwsIde        := Self;
  DwsIdeCallStackFrame.DwsIde      := Self;

  // Set the script folder, creating it if required
  ScriptFolder := IncludeTrailingBackslash(GetDesktopPath) + 'DWS Script Files';

  if not DirectoryExists( ScriptFolder ) then
    Raise Exception.Create( 'For this IDE demonstration, please place a copy of the ''DWS Script Files'' folder from project source on to your desktop.' );

  FIDEFormRect := BoundsRect;
  LoadSettings( sProjectFileName, FIDEFormRect );

  if FileExists(sProjectFileName) then
    LoadProjectFile( sProjectFileName )
   else
     begin
     sProjectFileName := ScriptFolder + '\ExampleScript.dwsproj';
     if FileExists(sProjectFileName) then
       LoadProjectFile( sProjectFileName )
      else
      actFileNewProjectExecute( nil );
     end;
end;

procedure TDwsIdeForm.BeforeDestruction;
begin
  SaveSettings(
    ProjectFileName,
    BoundsRect );

  inherited;

end;

procedure TDwsIdeForm.actOpenFileExecute(Sender: TObject);
var
  S : string;
  I : integer;
begin
  If not OpenFileDialog.Execute then
    Exit;

  for I := 0 to OpenFileDialog.Files.Count-1 do
    begin
    S := OpenFileDialog.Files[I];
    if (S <> '') and not FileIsOpenInEditor( S ) then
      EditorPageAddNew( S, True );
    end;
end;

procedure TDwsIdeForm.actOpenProjectExecute(Sender: TObject);
begin
  If OpenProjectDialog.Execute then
    LoadProjectfile( OpenProjectDialog.FileName );
end;

procedure TDwsIdeForm.ResetProgram;
begin
  dwsDebugger1.EndDebug;
end;

procedure TDwsIdeForm.RunProcedureByName(const AName: string);
var
  Exec : IdwsProgramExecution;
  FunctionInfo : IInfo;
  Stopwatch : TStopwatch;
begin
  If not IsCompiled then
    Compile( False );
  if not IsCompiled then
    Exit;

  Exec := FProgram.BeginNewExecution;
  try
    FunctionInfo := Exec.Info.Func[ AName ];
    if FunctionInfo = nil then
      raise Exception.CreateFmt('Cannot locate procedure "%s"', [AName] );

    AddStatusMessage( 'Running' );
    Application.ProcessMessages;

    try
      Stopwatch := TStopwatch.Create;
      Stopwatch.Start;
      try
        FunctionInfo.Call;
      finally
        Stopwatch.Stop;
        Exec.EndProgram;
      end;
    except
      On E:Exception do
        ShowMessage( E.Message );
    end;

    if Exec.Msgs.Count > 0 then
      begin
      AddStatusMessage( 'Errors' );
      ShowMessage(Exec.Msgs.AsInfo)
      end
     else
      If Stopwatch.Elapsed.TotalSeconds < 1.0 then
        AddStatusMessage( Format( 'Completed in %0.3f ms', [Stopwatch.Elapsed.TotalMilliseconds] ))
       else
        AddStatusMessage( Format( 'Completed in %0.3f s', [Stopwatch.Elapsed.TotalSeconds] ));
  finally
    Exec := nil;
  end;


end;

// RefreshTabs
//
const
   cMargin = 4;
   cSlantMargin = 10;
   cCloseButtonSize = 12;
   cArrowButtonSize = 15;
procedure TDwsIdeForm.RefreshTabs;

   function ColorLerp(col1, col2 : TColor; f : Single) : TColor;
   var
      invF : Single;
   begin
      if f<0 then f:=0;
      if f>1 then f:=1;
      invF:=1-f;

      col1:=ColorToRGB(col1);
      col2:=ColorToRGB(col2);

      Result:=RGB(Trunc(GetRValue(col1)*invF+GetRValue(col2)*f),
                  Trunc(GetGValue(col1)*invF+GetGValue(col2)*f),
                  Trunc(GetBValue(col1)*invF+GetBValue(col2)*f));
   end;

   procedure GradVertical(Canvas:TCanvas; Rect:TRect; FromColor, ToColor:TColor) ;
   var
      mx, Y : integer;
      invHeight : Single;
      cnt : Integer;
   begin
      canvas.Brush.Style:=bsSolid;

      if Rect.Bottom>Rect.Top then
         invHeight:=1/(Rect.Bottom-Rect.Top)
      else invHeight:=1;

      mx:=Rect.Right-cSlantMargin;
      cnt := 0;
      for Y := Rect.Top to Rect.Bottom-1 do begin

         Canvas.Brush.Color := ColorLerp(FromColor, ToColor, cnt*invHeight);

         Rect.Right:=mx+cnt;
         Rect.Top:=Y;
         Rect.Bottom:=Y+1;

         Canvas.FillRect(Rect);

         Inc(cnt) ;
      end;
   end;

   procedure RenderTab(canvas : TCanvas; page : TEditorPage;
                       active, hovered, hoveredClose : Boolean);
   var
      tabRect : TRect;
      r : TRect;
      txt : String;
      closeBtnDrawDetails : TThemedElementDetails;
   begin
      tabRect:=Rect(page.FTabLeft, canvas.ClipRect.Top,
                    page.FTabLeft+page.FTabWidth, canvas.ClipRect.Bottom);
      if    (tabRect.Right<canvas.ClipRect.Left)
         or (tabRect.Left>canvas.ClipRect.Right) then Exit;

      if active then
         if hovered then
            GradVertical(canvas, tabRect, clWindow, clWindow)
         else GradVertical(canvas, tabRect, clWindow, clWindow)
      else begin
         if hovered then
            GradVertical(canvas, tabRect, clWindow, clBtnFace)
         else GradVertical(canvas, tabRect, clWindow, ColorLerp(clBtnFace, clBtnShadow, 0.5));
      end;

      canvas.Brush.Style:=bsClear;

      {$Message 'support custom tab icons depending on content' }
      SmallImages.Draw(canvas, tabRect.Left+cMargin,
                       (tabRect.Bottom-tabRect.Top-SmallImages.Height) div 2, 6, True);

      txt:=page.Caption;
      r:=tabRect;
      r.Left:=r.Left+cMargin+SmallImages.Width+cMargin;
      canvas.TextRect(r, txt, [tfLeft, tfVerticalCenter, tfSingleLine, tfNoPrefix, tfEndEllipsis]);

      if not UseThemes then begin
         Windows.DrawFrameControl(canvas.Handle, page.CloseButtonRect,
                                  DFC_CAPTION, DFCS_CAPTIONCLOSE+DFCS_FLAT);
      end else begin
         if hovered and hoveredClose then
             closeBtnDrawDetails:=IDEStyleServices.GetElementDetails(twSmallCloseButtonHot)
         else closeBtnDrawDetails:=IDEStyleServices.GetElementDetails(twSmallCloseButtonDisabled);
         IDEStyleServices.DrawElement(canvas.Handle, closeBtnDrawDetails, page.CloseButtonRect);
      end;

      canvas.Pen.Color:=clBtnShadow;
      if active then begin
         canvas.MoveTo(0, tabRect.Bottom-1);
         canvas.LineTo(tabRect.Left, tabRect.Bottom-1);
      end else begin
         canvas.MoveTo(tabRect.Left, tabRect.Bottom);
      end;
      canvas.LineTo(tabRect.Left, tabRect.Top);
      canvas.LineTo(tabRect.Right-cSlantMargin, tabRect.Top);
      canvas.LineTo(tabRect.Right-cSlantMargin+tabRect.Bottom-tabRect.Top, tabRect.Bottom);
      if active then begin
         canvas.MoveTo(tabRect.Right-cSlantMargin+tabRect.Bottom-tabRect.Top, tabRect.Bottom-1);
         canvas.LineTo(canvas.ClipRect.Right, tabRect.Bottom-1);
      end;
   end;

var
   bmp : TBitmap;
   canvas : TCanvas;
   i, x : Integer;
   availableWidth : Integer;
   page : TEditorPage;
begin
   bmp:=imgTabs.Picture.Bitmap;
   canvas:=bmp.Canvas;
   canvas.Brush.Style:=bsSolid;
   canvas.Font:=Self.Font;

   canvas.Brush.Color:=clBtnFace;
   canvas.FillRect(canvas.ClipRect);

   if FPages.Count=0 then Exit;

   if FActivePageIndex<0 then
      FActivePageIndex:=0;

   // compute tab width
   for i:=0 to FPages.Count-1 do begin
      page:=FPages[i];
      page.FTabWidth:=  cMargin + SmallImages.Width
                      + cMargin + canvas.TextWidth(page.Caption)
                      + cMargin + cCloseButtonSize + 2*cMargin + cSlantMargin;
   end;

   if FBasePageIndex>=FPages.Count then
      FBasePageIndex:=FPages.Count-1;

   availableWidth:=bmp.Width-2*cArrowButtonSize;

   while True do begin
      x:=0;
      // compute tab positions
      for i:=FBasePageIndex to FPages.Count-1 do begin
         page:=FPages[i];
         page.FTabLeft:=x;
         x:=x+page.FTabWidth;
      end;
      x:=0;
      for i:=FBasePageIndex-1 downto 0 do begin
         page:=FPages[i];
         x:=x-page.FTabWidth;
         page.FTabLeft:=x;
      end;
      if    (FBasePageIndex=FActivePageIndex)
         or (FPages[FBasePageIndex].TabRight<=availableWidth) then Break;
      Inc(FBasePageIndex);
   end;

   // render tabs (right to left for slant overlap)
   for i:=FPages.Count-1 downto 0 do begin
      if i=FActivePageIndex then continue;
      page:=FPages[i];
      RenderTab(canvas, page, False, i=FHoveredPageIndex, FHoveredCloseButton);
   end;
   if FActivePageIndex>=0 then begin
      page:=FPages[FActivePageIndex];
      RenderTab(canvas, page, True, FActivePageIndex=FHoveredPageIndex, FHoveredCloseButton);
   end;

   RefreshTabArrows;
end;

// RefreshTabArrows
//
procedure TDwsIdeForm.RefreshTabArrows;
var
   dc : THandle;
   sbElement : TThemedScrollBar;
begin
   if FPages.Count<=1 then Exit;

   FLeftArrowActive:=(FBasePageIndex>0);
   FRightArrowActive:=    (FBasePageIndex<FPages.Count-1)
                      and (FPages[FPages.Count-1].TabRight>imgTabs.Width-2*cArrowButtonSize);

   if not (FLeftArrowActive or FRightArrowActive) then Exit;

   dc:=imgTabs.Picture.Bitmap.Canvas.Handle;

   FTabArrowLeft.Top:=(imgTabs.Height-cArrowButtonSize) div 2;
   FTabArrowLeft.Bottom:=FTabArrowLeft.Top+cArrowButtonSize;
   FTabArrowLeft.Left:=imgTabs.Width-2*cArrowButtonSize;
   FTabArrowLeft.Right:=imgTabs.Width-cArrowButtonSize;
   FTabArrowRight:=FTabArrowLeft;
   OffsetRect(FTabArrowRight, cArrowButtonSize, 0);

   if not UseThemes then begin
      Windows.DrawFrameControl(dc, FTabArrowLeft, DFC_SCROLL,
                                DFCS_SCROLLLEFT+DFCS_FLAT
                               +Ord(not FLeftArrowActive)*DFCS_INACTIVE
                               +Ord(FHoveredLeftArrow)*DFCS_HOT);
      Windows.DrawFrameControl(dc, FTabArrowRight, DFC_SCROLL,
                                DFCS_SCROLLRIGHT+DFCS_FLAT
                               +Ord(not FRightArrowActive)*DFCS_INACTIVE
                               +Ord(FHoveredRightArrow)*DFCS_HOT);
   end else begin
      if FLeftArrowActive then
         if FHoveredLeftArrow then
            sbElement:=tsArrowBtnLeftHot
         else sbElement:=tsArrowBtnLeftNormal
      else sbElement:=tsArrowBtnLeftDisabled;
      IDEStyleServices.DrawElement(dc, IDEStyleServices.GetElementDetails(sbElement), FTabArrowLeft);
      if FRightArrowActive then
         if FHoveredRightArrow then
            sbElement:=tsArrowBtnRightHot
         else sbElement:=tsArrowBtnRightNormal
      else sbElement:=tsArrowBtnRightDisabled;
      IDEStyleServices.DrawElement(dc, IDEStyleServices.GetElementDetails(sbElement), FTabArrowRight);
   end;

   imgTabs.Invalidate;
end;

// IndexOfTab
//
function TDwsIdeForm.IndexOfTab(x : Integer) : Integer;
var
   i : Integer;
   page : TEditorPage;
begin
   if x<imgTabs.Width-cArrowButtonSize*2 then begin
      for i:=FBasePageIndex to FPages.Count-1 do begin
         page:=FPages[i];
         if (x>=page.FTabLeft) and (x<page.FTabLeft+page.FTabWidth) then
            Exit(i);
      end;
   end;
   Result:=-1;
end;


procedure TDwsIdeForm.actProgramResetExecute(Sender: TObject);
begin
  ResetProgram;
  //InformationDlg( 'Program Aborted' );
end;

procedure TDwsIdeForm.actProgramResetUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := daCanEndDebug in dwsDebugger1.AllowedActions;
end;

procedure TDwsIdeForm.actRunExecute(Sender: TObject);

  procedure NewRun;
  var
    Exec : IdwsProgramExecution;
  begin
    Compile( False );
    If not IsCompiled then
      Exit;

    ShowExecutableLines;

    Exec := FProgram.CreateNewExecution;
    try
      dwsDebugger1.BeginDebug( Exec );
    finally
      dwsDebugger1.EndDebug;
      ClearExecutableLines;
      if Exec.Msgs.Count > 0 then
        begin
        AddStatusMessage( 'Errors' );
        GotoScriptPos( Exec.Msgs.LastMessagePos );
        ErrorDlg(Exec.Msgs.AsInfo);
        end
       else
        AddStatusMessage( 'Program completed' );
    end;

  end;

begin
  try
    If dwsDebugger1.State = dsDebugSuspended then
      dwsDebugger1.Resume
     else
       NewRun;
  except
    on E:Exception do
      ErrorDlg( E.Message );
  end;
end;



procedure TDwsIdeForm.actRunProcedureAtCursorExecute(Sender: TObject);
var
  S : string;
begin
  S := CurrentEditor.SelText;
  RunProcedureByName( S );
end;

procedure TDwsIdeForm.actRunProcedureAtCursorUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasEditorPage and (CurrentEditor.SelLength <> 0)
end;

procedure TDwsIdeForm.actRunUnitTestsUpdate(Sender: TObject);
begin
  With Sender as TAction do
    {$IFDEF ArtDunit}
      Visible := True;
    {$ELSE}
      Visible := False;
    {$ENDIF}
end;

procedure TDwsIdeForm.actRunUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasProject;
end;

procedure TDwsIdeForm.actRunWithoutDebuggingExecute(Sender: TObject);
var
  Exec : IdwsProgramExecution;
  Stopwatch : TStopwatch;
begin
  AddStatusMessage( 'Running' );
  Application.ProcessMessages;
  Compile( False );
  if not IsCompiled then
    Exit;

  Exec := FProgram.CreateNewExecution;
  Exec.BeginProgram;
  Stopwatch := TStopwatch.Create;
  Stopwatch.Start;
  try
    Exec.RunProgram( 0 );
    Exec.EndProgram;
  finally
    Stopwatch.Stop;
  end;

  if Exec.Msgs.Count > 0 then
    begin
    AddStatusMessage( 'Errors' );
    ShowMessage(Exec.Msgs.AsInfo)
    end
   else
    If Stopwatch.Elapsed.TotalSeconds < 1.0 then
      AddStatusMessage( Format( 'Completed in %0.3f ms', [Stopwatch.Elapsed.TotalMilliseconds] ))
     else
      AddStatusMessage( Format( 'Completed in %0.3f s', [Stopwatch.Elapsed.TotalSeconds] ));
end;

procedure TDwsIdeForm.actRunWithoutDebuggingUpdate(Sender: TObject);
begin
  With Sender as TAction do
    Enabled := HasProject;
end;

procedure TDwsIdeForm.EditorPageAddNew(const AFileName: string; ALoadFile : boolean );
begin
   FPages.Add( TEditorPage.Create( Self, AFileName, ALoadFile ) );
   SetEditorCurrentPageIndex(FPages.Count-1);
end;

procedure TDwsIdeForm.ListSymbols;
var
  SL : TStringList;
  S : string;
begin
  Compile( False );
  If not IsCompiled then
    begin
    ErrorDlg( 'Unable to compile' );
    Exit;
    end;

  SL := TStringList.Create;
  try
    SymbolsToStrings( FProgram.Table, SL );

    S := SL.Text;
    ShowMessage( S );
  finally
    SL.Free
  end;

end;


procedure TDwsIdeForm.ClearCurrentLine;
var
  I : integer;
begin
  for I := 0 to EditorPageCount-1 do
    EditorPage( I ).SetCurrentLine( -1 );
end;

procedure TDwsIdeForm.ClearAllBreakpoints;
var
  I : integer;
begin
  dwsDebugger1.Breakpoints.Clean;
  for I := 0 to EditorPageCount-1 do
    EditorPage( I ).Editor.Invalidate;
end;

function TDwsIdeForm.CurrentEditor: TSynEdit;
begin
  If HasEditorPage then
    Result := EditorPage( EditorCurrentPageIndex ).Editor
   else
     Result := nil;
end;

function TDwsIdeForm.CurrentEditorPage: TEditorPage;
begin
  Result := EditorPage( FActivePageIndex );
end;

function TDwsIdeForm.ProjectSourceScript: string;
begin
  EditorSaveAllIfModified( False );
  Result := TextFileToString( ProjectSourceFileName );
end;

procedure TDwsIdeForm.DoOnClickEditorPageTabContextMenuPageItem(
  ASender: TObject);
begin
  With ASender as TMenuItem do
    EditorCurrentPageIndex := Tag;
end;

procedure TDwsIdeForm.DoOnCodeSuggestionFormSelectItem(const AItemText: string);
var
  S : string;
begin
  S := CurrentEditor.LineText;
  Insert( AItemText, S, CurrentEditor.CaretX );
  CurrentEditor.Linetext := S;
  CurrentEditor.CaretX := CurrentEditor.CaretX + Length( AItemText );
  CurrentEditor.Modified := true;
end;

procedure TDwsIdeForm.dwsDebugger1Debug(exec: TdwsExecution;
  expr: TExprBase);
begin
  //ShowMessage( 'dwsDebugger1Debug' );
end;

procedure TDwsIdeForm.dwsDebugger1DebugStart(exec: TdwsExecution);
begin
  //AddStatusMessage( 'Running' );
end;

procedure TDwsIdeForm.dwsDebugger1DebugStop(exec: TdwsExecution);
begin
//  AddStatusMessage( 'Paused' );
end;

procedure TDwsIdeForm.dwsDebugger1EnterFunc(exec: TdwsExecution;
  expr: TExprBase);
//var
//  R : TdwsExprLocationArray;
//  I : integer;
begin
//  R := Exec.GetCallStack;
//  I := Length(R);
//  if I > 0 then
//    ShowMessage( R[0].Location );
//  ShowMessage( 'dwsDebugger1EnterFunc' );
end;

procedure TDwsIdeForm.dwsDebugger1LeaveFunc(exec: TdwsExecution;
  expr: TExprBase);
begin
//  ShowMessage( 'dwsDebugger1LeaveFunc' );
end;


procedure TDwsIdeForm.GotoScriptPos( AScriptPos : TScriptPos );
var
  S : string;
  I : integer;
begin
  if AScriptPos.SourceFile <> nil then
    begin
    S := AScriptPos.SourceFile.Name;
    if S = '*MainModule*' then
      I := ProjectSourceFileIndex
     else
      I := NameToEditorPageIndex( S );
    if I <> -1 then
      begin
      EditorCurrentPageIndex := I;
      CurrentEditorPage.SetCurrentLine( AScriptPos.Line );
      end;
    end;
end;






function TDwsIdeForm.DwsIde_GetDebugger: TdwsDebugger;
begin
  Result := dwsDebugger1;
end;

procedure TDwsIdeForm.dwsDebugger1StateChanged(Sender: TObject);

  procedure UpdateDebugWindows;
  begin
    DwsIdeLocalVariablesFrame.Redraw;
    DwsIdeCallStackFrame.Redraw;
    DwsIdeWatchesFrame.Redraw;
  end;

begin
  Case dwsDebugger1.State of
   dsIdle :
     begin
     end;
   dsDebugRun :
     begin
     ClearCurrentLine;
     AddStatusMessage( 'Running' );
     end;
   dsDebugSuspending :
     begin
     end;
   dsDebugSuspended :
     begin
     AddStatusMessage( 'Paused' );
     GotoScriptPos( dwsDebugger1.CurrentScriptPos );
     UpdateDebugWindows;
     end;
   dsDebugResuming :
     begin
     end;
   dsDebugDone :
     begin
     ClearCurrentLine;
     UpdateDebugWindows;
     end
  End;
end;


procedure TDwsIdeForm.EditorChange(Sender: TObject);
begin
  if IsCompiled then
    begin
    ClearExecutableLines;
    FProgram := nil;
    end;
  //FScript.NotifyScriptModified;
end;

function TDwsIdeForm.FileIsOpenInEditor(
  const AFileName: string): boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to EditorPageCount-1 do
    if SameText( AFileName, EditorPage(I).FileName) then
      Exit;
  Result := False;
end;

function TDwsIdeForm.FileIsProjectSource(
  const AFileName: string): boolean;
begin
  Result := SameText( ExtractFileExt( AFileName ), sProjectSourceFileExt );
end;

procedure TDwsIdeForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  If dwsDebugger1.State = dsDebugSuspended then
    if ConfirmDlg( 'Abandon debugging?' ) then
      ResetProgram
     else
       Exit;

  if ProjectFilename = '' then
    Exit;

  If FileExists( ProjectFilename ) then
    SaveProjectFileAs( ProjectFileName )
   else
    If not SaveProjectAs then
      Abort;

  EditorCloseAllPages;
end;


procedure TDwsIdeForm.FormCreate(Sender: TObject);
var
   bmp : TBitmap;
begin
  FCodeProposalForm := TDwsIdeCodeProposalForm.Create( Self );
  FCodeProposalForm.OnSelectItem := DoOnCodeSuggestionFormSelectItem;

  bmp:=TBitmap.Create;
  try
     bmp.Height:=imgTabs.Height;
     bmp.Width:=imgTabs.Width;
     imgTabs.Picture.Bitmap:=bmp;
  finally
     bmp.Free;
  end;

  FPages:=TSimpleList<TEditorPage>.Create;
  FActivePageIndex:=-1;
  FHoveredPageIndex:=-1;
end;

procedure TDwsIdeForm.FormDestroy(Sender: TObject);
var
   i : Integer;
begin
  FreeAndNil( FCodeProposalForm );
  dwsDebugger1.Breakpoints.Clean;
  dwsDebugger1.Watches.Clean;
  FProgram := nil;
  for i:=0 to FPages.Count-1 do
   FPages[i].Free;
  FPages.Free;
end;

procedure TDwsIdeForm.FormShow(Sender: TObject);
begin
  BoundsRect := FIDEFormRect;
end;

function TDwsIdeForm.GetProjectSourceFileName: string;
var
  I : integer;
begin
  I := ProjectSourceFileIndex;
  If I >= 0 then
    Result := EditorPage(I).FileName
   else
     Result := '';
end;

function TDwsIdeForm.HasEditorPage: boolean;
begin
  Result := EditorCurrentPageIndex <> -1;
end;

function TDwsIdeForm.HasProject: boolean;
begin
  Result := (ProjectFileName <> '') and ((ProjectSourceFileIndex <> -1) or FileExists( ProjectSourceFileName ));
end;

procedure TDwsIdeForm.imgTabsMouseLeave(Sender: TObject);
begin
   if FHoveredPageIndex>=0 then begin
      FHoveredPageIndex:=-1;
      RefreshTabs;
   end;
end;

procedure TDwsIdeForm.imgTabsMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
   newHover : Integer;
   newHoverCloseButton, needRefresh, needRefreshArrows : Boolean;
   newHoverArrow : Boolean;
begin
   needRefresh:=False;
   newHover:=IndexOfTab(x);
   if newHover<>FHoveredPageIndex then begin
      FHoveredPageIndex:=newHover;
      needRefresh:=True;
      Application.CancelHint;
      if newHover>=0 then
         imgTabs.Hint:=FPages[newHover].Hint
      else imgTabs.Hint:='';
   end;
   if newHover>=0 then begin
      newHoverCloseButton:=PtInRect(FPages[newHover].CloseButtonRect, Point(X, Y));
      if newHoverCloseButton<>FHoveredCloseButton then begin
         FHoveredCloseButton:=newHoverCloseButton;
         needRefresh:=True;
      end;
   end;

   needRefreshArrows:=False;
   newHoverArrow:=FLeftArrowActive and PtInRect(FTabArrowLeft, Point(X, Y));
   if newHoverArrow<>FHoveredLeftArrow then begin
      FHoveredLeftArrow:=newHoverArrow;
      needRefreshArrows:=True;
   end;
   newHoverArrow:=FRightArrowActive and PtInRect(FTabArrowRight, Point(X, Y));
   if newHoverArrow<>FHoveredRightArrow then begin
      FHoveredRightArrow:=newHoverArrow;
      needRefreshArrows:=True;
   end;

   if needRefresh then
      RefreshTabs
   else if needRefreshArrows then
      RefreshTabArrows;
end;

function TDwsIdeForm.IsCompiled: boolean;
begin
  Result := Assigned( FProgram );
end;

procedure TDwsIdeForm.pcEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure AddPageList;
  var
    PageItem : TMenuItem;
    Page : TEditorPage;
    I : integer;
    S : string;
  begin
    miPages.Clear;
    for I := 0 to EditorPageCount-1 do
      begin
      PageItem := TMenuItem.Create( EditorPageTabContextMenu );
      miPages.Add( PageItem );
      Page := EditorPage(I);
      S := Format( '%s (%s)', [Page.Caption, Page.FileName] );
      PageItem.Caption := S;
      PageItem.OnClick := DoOnClickEditorPageTabContextMenuPageItem;
      PageItem.Tag     := I;
      PageItem.Checked := I = EditorCurrentPageIndex;
      end;
  end;

var
  P : TPoint;
  tabIndex : Integer;
begin
   FpcEditorLastMouseButton := Button;
   FpcEditorLastMouseXY     := Point( X, Y );

   tabIndex:=IndexOfTab(X);
   if (tabIndex>=0) then begin
      SetEditorCurrentPageIndex(tabIndex);
      if PtInRect(FPages[tabIndex].CloseButtonRect, FpcEditorLastMouseXY) then begin
         EditorPageClose(tabIndex);
         Exit;
      end;
   end;

  If FpcEditorLastMouseButton = mbRight then
    begin
    AddPageList;
    P := imgTabs.ClientToScreen( FpcEditorLastMouseXY );
    EditorPageTabContextMenu.Popup( P.X, P.Y );
    end;

  if FHoveredLeftArrow then begin
     Dec(FBasePageIndex);
     RefreshTabs;
  end;
  if FHoveredRightArrow then begin
     Inc(FBasePageIndex);
     RefreshTabs;
  end;
end;

procedure TDwsIdeForm.pnlPageControlResize(Sender: TObject);
begin
   imgTabs.Picture.Bitmap.Width:=imgTabs.Width;
   RefreshTabs;
end;

function TDwsIdeForm.ProjectSourceFileIndex: integer;
var
  S : string;
begin
  S := ExtractFileName(ProjectfileNameToProjectSourceFileName( ProjectFileName ));
  for Result := 0 to EditorPageCount-1 do
    if SameText( S, ExtractFileName( EditorPage(Result).FileName)) then
      Exit;
  Result := -1;
end;

procedure TDwsIdeForm.SetScript(const Value: TDelphiWebScript);
begin
  FScript := Value;

  if not Assigned( FScript ) then
    raise EDwsIde.Create( 'Script cannot be nil - the IDE requires a script to debug' );

  If FScript.Config.ScriptPaths.Count = 0 then
    Raise EDwsIde.Create( 'Script does not define a main path' );

  If FScript.Config.ScriptPaths.IndexOf( FScriptFolder ) = -1 then
     FScript.Config.ScriptPaths.Add( FScriptFolder );

  //FScriptFolder := FScript.Config.ScriptPaths[0];

  //OpenFileDialog.DefaultFolder    := FScriptFolder;
  //OpenProjectDialog.DefaultFolder := FScriptFolder;
  //SaveProjectDialog.DefaultFolder := FScriptFolder;

end;

procedure TDwsIdeForm.SetScriptFolder(const Value: string);
begin
  FScriptFolder := Value;

  OpenFileDialog.DefaultFolder := FScriptFolder;
  OpenProjectDialog.DefaultFolder := FScriptFolder;
  SaveProjectDialog.DefaultFolder := FScriptFolder;
end;

procedure TDwsIdeForm.SetEditorCurrentPageIndex(const Value: integer);
var
   page : TEditorPage;
begin
   if Value=FActivePageIndex then Exit;

   LockWindowUpdate(pnlPageControl.Handle);

   if FActivePageIndex>=0 then begin
    page := EditorPage( Value );
    page.Visible:=False;
    page.Parent:=nil;
   end;

  if (Value >= 0) and (Value < FPages.Count) then
    begin
    FActivePageIndex:=Value;
    RefreshTabs;
    page := EditorPage( Value );
    page.Align:=alClient;
    page.Parent:=pnlPageControl;
    page.Visible:=True;
    page.Editor.Repaint;
    end;

   LockWindowUpdate(0);
end;

procedure TDwsIdeForm.SetProjectFileName(const Value: string);
begin
  if FileExists( Value ) then
    begin
    FProjectFileName := Value;
    LoadProjectFile( FProjectFileName );
    end
   else
     FProjectFileName := '';
end;

procedure TDwsIdeForm.UpdateTimerTimer(Sender: TObject);

  procedure UpdateFormCaption;
  begin
    If ProjectfileName = '' then
      Caption := '[No project]'
     else
      Caption := Format( '%s  (%s)', [ExtractFileName(ProjectFilename), ExtractfileDir(ProjectFilename)] );
  end;

  procedure UpdateStatusBarPanels;
  resourcestring
    SModified  = 'Modified';
    SInsert    = 'Insert';
    SOverwrite = 'Overwrite';
    SReadOnly  = 'Read Only';
  var
    ptCaret: TPoint;
    Editor : TSynEdit;
  begin
    Editor := CurrentEditor;
    if Editor <> nil then
      begin
      ptCaret := TPoint(Editor.CaretXY);
      if (ptCaret.X > 0) and (ptCaret.Y > 0) then
        StatusBar.Panels[0].Text := Format(' %6d:%3d ', [ptCaret.Y, ptCaret.X])
        else
          StatusBar.Panels[0].Text := '';

      if Editor.Modified then
        StatusBar.Panels[1].Text := SModified
      else
        StatusBar.Panels[1].Text := '';

      if Editor.ReadOnly then
        StatusBar.Panels[2].Text := SReadOnly
       else
         if Editor.InsertMode then
          StatusBar.Panels[2].Text := SInsert
         else
           StatusBar.Panels[2].Text := SOverwrite;
      end
    else
      begin
      StatusBar.Panels[0].Text := '';
      StatusBar.Panels[1].Text := '';
      StatusBar.Panels[2].Text := '';
      end;
  end;

begin
  UpdateStatusBarPanels;

  UpdateFormCaption;
end;

procedure TDwsIdeForm.WMCodeSuggest(var AMessage: TMessage);
begin
  CodeSuggest( csAutoComplete );
end;

procedure TDwsIdeForm.SetProjectSourceFileName(const Value: string);
var
  I : integer;
begin
  I := ProjectSourceFileIndex;
  if I >= 0 then
    begin
    EditorPage( I ).FileName := Value;
    FPages[i].Caption := ChangeFileExt( ExtractFileName( Value ), '' ) + ' *' ;
    end;


end;



procedure TDwsIdeForm.ShowExecutableLines;
var
  I : integer;
begin
  for I := 0 to EditorPageCount-1 do
    EditorPage(I).ShowExecutableLines;
end;

procedure TDwsIdeForm.CodeSuggest( ACodeSuggestionMode : TCodeSuggestionMode );
var
  Suggestions   : IDwsSuggestions;
  ScriptPos     : TScriptPos;
  Script        : TDelphiWebScript;
  ScriptProgram : IdwsProgram;
begin
  if not HasEditorPage then
    Exit;

  Script := TDelphiWebScript.Create( nil );
  try

    Script.OnNeedUnit := FScript.OnNeedUnit;
    Script.OnInclude  := FScript.OnInclude;
    Script.Config.CompilerOptions := [ coContextMap, coSymbolDictionary];
    Script.Config.ScriptPaths.Assign( FScript.Config.ScriptPaths );

    try
      ScriptProgram := Script.Compile( ProjectSourceScript );
    except
    end;

    if ScriptProgram = nil then
      Exit;

    ScriptPos := TScriptPos.Create( ScriptProgram.SourceList[0].SourceFile, CurrentEditor.CaretY, CurrentEditor.CaretX );
    Suggestions := TdwsSuggestions.Create( ScriptProgram, ScriptPos );

    FCodeProposalForm.Open(
      CurrentEditor.ClientToScreen( CurrentEditor.RowColumnToPixels( CurrentEditor.DisplayXY )),
      ACodeSuggestionMode,
      Suggestions );

  finally
    Script.Free;
  end;
end;


procedure TDwsIdeForm.ClearExecutableLines;
var
  I : integer;
begin
  for I := 0 to EditorPageCount-1 do
    EditorPage(I).ClearExecutableLines;
end;

procedure TDwsIdeForm.EditorPageClose(AIndex: integer);
var
  Page : TEditorPage;
begin
  if AIndex = -1 then
    Exit;

  Page := EditorPage( AIndex );

  Page.SaveIfModified( True );

  if EditorCurrentPageIndex>0 then
     EditorCurrentPageIndex:=EditorCurrentPageIndex-1
  else
    If FPages.Count > 0 then
      EditorCurrentPageIndex := 0;

  FPages[AIndex].Free;
  FPages.Extract(AIndex);
  RefreshTabs;
end;

function TDwsIdeForm.EditorPageCount: integer;
begin
  Result := FPages.Count;
end;

function TDwsIdeForm.EditorPage(AIndex: integer) : TEditorPage;
begin
  Result := FPages[AIndex];
end;



procedure TDwsIdeForm.SaveProjectFileAs( const AProjectFileName : string );

  procedure SaveBreakpoints( AData : IXMLProjectConfigType );
  var
    I : integer;
    Breakpoint : IXMLBreakpointType;
  begin
    AData.Breakpoints.Clear;
    for I := 0 to dwsDebugger1.Breakpoints.Count-1 do
      begin
        Breakpoint := AData.Breakpoints.Add;
        Breakpoint.SourceName := dwsDebugger1.Breakpoints[I].SourceName;
        Breakpoint.LineNum    := dwsDebugger1.Breakpoints[I].Line;
        Breakpoint.Enabled    := dwsDebugger1.Breakpoints[I].Enabled;
      end;
  end;

  procedure SaveWatches( AData : IXMLProjectConfigType );
  var
    I : integer;
    Watch : IXMLWatchType;
  begin
    AData.Watches.Clear;
    for I := 0 to dwsDebugger1.Watches.Count-1 do
      begin
        Watch := AData.Watches.Add;
        Watch.Expression := dwsDebugger1.Watches[I].ExpressionText;
      end;
  end;

var
  I : integer;
  XMLPage          : IXMLEditorPageType;
  XMLDocument      : IXMLDocument;
  Data             : IXMLProjectConfigType;
  Page             : TEditorPage;
  S                : string;
begin
  XMLDocument := NewXMLDocument;
  Data := XMLDocument.GetDocBinding('ProjectConfig',TXMLProjectConfigType) as IXMLProjectConfigType;

  // Make an empty project source file if required
  S := ProjectfileNameToProjectSourceFileName( AProjectFilename );
  if not FileExists(S) then
    StringToTextFile( '', S );

  for I := 0 to EditorPageCount-1 do
    begin

    Page := EditorPage( I );

    Page.SaveIfModified( True );

    XMLPage := Data.EditorPages.EditorPage.Add;

    XMLPage.FileName := ExtractRelativePath(
      IncludeTrailingBackslash(FScriptFolder),
      Page.FileName );
    XMLPage.Name     := JustFileName( Page.FileName );

    end;

  Data.EditorPages.ActivePageIndex := EditorCurrentPageIndex;

  SaveBreakpoints( Data );
  SaveWatches( Data );

  XMLDocument.XML.Text := FormatXMLData( XMLDocument.XML.Text);
  XMLDocument.Active := True;

  XMLDocument.SaveToFile( AProjectFileName );

  FProjectFileName := AProjectFileName;

end;





procedure TDwsIdeForm.LoadProjectFile( const AProjectFileName : string );

  procedure LoadBreakpoints( AData : IXMLProjectConfigType );
  var
    I : integer;
    Breakpoint : IXMLBreakpointType;
  begin
    dwsDebugger1.Breakpoints.Clean;
    for I := 0 to AData.Breakpoints.Count-1 do
      begin
        Breakpoint := AData.Breakpoints[I];
        dwsDebugger1.Breakpoints.Add( Breakpoint.LineNum, Breakpoint.SourceName );
        dwsDebugger1.Breakpoints[I].Enabled := Breakpoint.Enabled;
      end;
  end;

  procedure LoadWatches( AData : IXMLProjectConfigType );
  var
    I : integer;
    Watch : IXMLWatchType;
  begin
    dwsDebugger1.Watches.Clean;
    for I := 0 to AData.Watches.Count-1 do
      begin
        Watch := AData.Watches[I];
        dwsDebugger1.Watches.Add( Watch.Expression );
      end;

    DwsIdeWatchesFrame.Redraw;
  end;


var
  I : integer;
  sFileName : string;
  XMLDocument   : IXMLDocument;
  Data          : IXMLProjectConfigType;
begin
  If not FileExists( AProjectFileName) then
    raise EDwsIde.CreateFmt( 'Project file does not exist (%s)', [AProjectFileName]);

  EditorCloseAllPages;

  FProjectFileName := AProjectFileName;

  FProgram := nil;

  XMLDocument := LoadXMLDocument( AProjectFileName );
  Data := XMLDocument.GetDocBinding('ProjectConfig',TXMLProjectConfigType) as IXMLProjectConfigType;
  for I := 0 to Data.EditorPages.EditorPage.Count-1 do
    begin
    sFileName :=
      IncludeTrailingBackslash( FScriptFolder ) + Data.EditorPages.EditorPage[I].FileName;
    if FileExists( sFileName ) and not FileIsOpenInEditor( sFileName ) then
      EditorPageAddNew( sFileName, True );
    end;
  EditorCurrentPageIndex := Data.EditorPages.ActivePageIndex;

  LoadBreakpoints( Data );
  LoadWatches( Data );

  ClearExecutableLines;


end;


procedure TDwsIdeForm.NewProjectFile( const AProjectFileName : string );
begin
  EditorCloseAllPages;
  FProjectFileName := AProjectFileName;
end;




function TDwsIdeForm.ProjectfileNameToProjectSourceFileName( const AProjectfileName : string ): string;
begin
  if AProjectFileName = '' then
    Result := ''
   else
    Result := ChangeFileExt( AProjectFileName, sProjectSourceFileExt );
end;

function TDwsIdeForm.ModifyFileNameToUniqueInProject(
           const AFileName : string ) : string;
// If exists, increments any trailing number after the file name
// until the name is unique in the name's folder. If no trailing number
// adds '1'.
var
  I          : integer;
  sD, sF, sE : string;
begin
  Result := AFileName;
  I := 0;
  While FileIsOpenInEditor( Result ) do
    begin
    Inc(I);
    sD := ExtractFileDir( Result ) + '\';
    sF := JustFileName( Result );
    while (sF <> '') and CharInSet( sF[Length(sF)-1], ['0'..'9']) do
      Delete( sF, Length(sF)-1, 1 );

    sE := ExtractFileExt( Result );
    Result := sD + sF + IntToStr(I) + sE;
    end;
end;

function TDwsIdeForm.GetExecutableLines( const AUnitName : string ) : TLineNumbers;
// Returns the executable line numbers for this unit.

  procedure AppendLineNum( ALineNum : integer );
  begin
    SetLength( Result, Length(Result) + 1 );
    Result[Length(Result)-1] := ALineNum;
  end;

var
  I : integer;
  Breakpointables : TdwsBreakpointableLines;
  Lines : Tbits;
begin
  SetLength( Result, 0 );

  Compile( False );
  if not IsCompiled then
    Exit;

  Breakpointables := TdwsBreakpointableLines.Create( FProgram );
  try
    I := Breakpointables.IndexOfSource( AUnitName );
    if I >= 0 then
      begin
      Lines := Breakpointables.SourceLines[I];
      For I := 1 to Lines.Size-1 do
        if Lines[I] then
          AppendLineNum( I );
      end;
  finally
    Breakpointables.Free;
  end;


end;



procedure TDwsIdeForm.SaveSettings(
      const AProjectFileName : string;
      const AIDEFormRect     : TRect );
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey('SOFTWARE\DwsIde\', TRUE) then
      begin
      Reg.WriteString('WorkingProjectFileName', AProjectFileName);
      Reg.WriteInteger( 'IDEFormRect_Left', AIDEFormRect.Left);
      Reg.WriteInteger( 'IDEFormRect_Top', AIDEFormRect.Top);
      Reg.WriteInteger( 'IDEFormRect_Right', AIDEFormRect.Right);
      Reg.WriteInteger( 'IDEFormRect_Bottom', AIDEFormRect.Bottom);
      end;

  finally
    Reg.Free;
  end;
end;


procedure TDwsIdeForm.LoadSettings(
           var AProjectFileName : string;
           var AIDEFormRect     : TRect );
var
  Reg : TRegistry;
begin
  AProjectFileName := '';

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\DwsIde\', False) then
      begin
      AProjectFileName    := Reg.ReadString( 'WorkingProjectFileName');
      AIDEFormRect.Left   := Reg.ReadInteger( 'IDEFormRect_Left');
      AIDEFormRect.Top    := Reg.ReadInteger( 'IDEFormRect_Top');
      AIDEFormRect.Right  := Reg.ReadInteger( 'IDEFormRect_Right');
      AIDEFormRect.Bottom := Reg.ReadInteger( 'IDEFormRect_Bottom');
      end;
  finally
    Reg.Free;
  end;
end;





{ TEditorPage }


constructor TEditorPage.Create(
                    AOwner    : TDwsIdeForm;
              const AFileName : string;
                    ALoadFile : boolean);

  procedure InitEditor;
  begin
    FEditor := TSynEdit.Create( Self );
    FEditor.OnChange := DoOnEditorChange;
    FEditor.Parent   := Self;
    FEditor.Align    := alClient;
    FEditor.BorderStyle := bsNone;
    FEditor.Gutter.Width := 50;
    FEditor.PopupMenu := AOwner.EditorPagePopupMenu;
    FEditor.WantTabs  := True;

    If Assigned( AOwner.FOptions.EditorHighlighterClass ) then
      FEditor.Highlighter := AOwner.FOptions.EditorHighlighterClass.Create( Self );
    If AOwner.FOptions.EditorFontName <> '' then
      begin
      FEditor.Font.Name := AOwner.FOptions.EditorFontName;
      FEditor.Font.Size := AOwner.FOptions.EditorFontSize;
      end
     else
      begin
      FEditor.Font.Name := 'Courier New';
      FEditor.Font.Size := 10;
      end;

    FEditor.Options := [
      eoAutoIndent,
      eoKeepCaretX,
      eoScrollByOneLess,
      eoSmartTabs,
      eoTabsToSpaces,
      eoTrimTrailingSpaces,
      eoRightMouseMovesCursor];

    FEditor.OnSpecialLineColors := SynEditorSpecialLineColors;
    FEditor.OnGutterClick := SynEditorGutterClick;
    FEditor.OnCommandProcessed := SynEditorCommandProcessed;
    FEditor.OnClick := SynEditorClick;
    FEditor.OnKeyDown := SynEditorKeyDown;

    TEditorPageSynEditPlugin.Create(Self);

  end;

begin
  inherited Create( AOwner );

  FForm := AOwner;

  FCurrentLine := -1;

  FileName := AFileName;

  //PopupMenu := AOwner.EditorPageTabContextMenu;

  InitEditor;

  if ALoadFile and FileExists( AFileName ) then
    begin
    FEditor.Lines.Text := TextFileToString( AFileName );
    InitExecutableLines;
    FEditor.ReadOnly   := FileIsReadOnly( AFileName );
    end;

  FEditor.Modified := False;

end;

destructor TEditorPage.Destroy;
begin
  FEditor.Free;

  inherited;
end;

procedure TEditorPage.DoOnEditorChange(ASender: TObject);
begin
  FForm.EditorChange( ASender );
end;

function TEditorPage.GetFilename: string;
begin
  Result := Hint;
end;

function TEditorPage.GetIsProjectSourcefile: boolean;
begin
  Result := SameText( ExtractFileExt( FileName ), sProjectSourceFileExt );
end;

function TEditorPage.GetIsReadOnly: boolean;
begin
  Result := FEditor.ReadOnly;
end;


procedure TEditorPage.AddBreakpoint(ALineNum: integer; AEnabled: boolean);
var
  BP : TdwsDebuggerBreakpoint;
  bAdded : boolean;
  I : integer;
begin
  BP:=TdwsDebuggerBreakpoint.Create;
  BP.Line:= ALineNum;

  BP.SourceName := UnitName;

  I := FForm.dwsDebugger1.Breakpoints.AddOrFind( BP, bAdded );
  If not bAdded then
    BP.Free;
  FForm.dwsDebugger1.Breakpoints[I].Enabled := AEnabled;

  Editor.InvalidateGutterLine( ALineNum );
  Editor.InvalidateLine( ALineNum );
end;


procedure TEditorPage.ClearExecutableLines;
var
  I : integer;
begin
  For I := 0 to Length( FExecutableLines )-1 do
    FExecutableLines[I] := False;
  Editor.InvalidateGutter;
end;

procedure TEditorPage.ClearBreakpoint(ALineNum: integer);
var
  Test, Found : TdwsDebuggerBreakpoint;
  I : integer;
begin
  If FForm.dwsDebugger1.Breakpoints.Count = 0 then
    Exit;

  Test :=TdwsDebuggerBreakpoint.Create;
  try
    Test.Line:= ALineNum;
    Test.SourceName := UnitName;

    I := FForm.dwsDebugger1.Breakpoints.IndexOf( Test );
    if I <> -1 then
      begin
      Found := FForm.dwsDebugger1.Breakpoints[I];
      FForm.dwsDebugger1.Breakpoints.Extract( Found );
      FreeAndNil( Found );
      end;
  finally
    FreeAndNil( Test );
  end;

  Editor.InvalidateGutterLine( ALineNum );
  Editor.InvalidateLine( ALineNum );
end;


procedure TEditorPage.InitExecutableLines;
begin
  SetLength( FExecutableLines, 0 );
  SetLength( FExecutableLines, Editor.Lines.Count );
end;


procedure TEditorPage.ShowExecutableLines;
var
  LineNumbers : TLineNumbers;
  I           : integer;
begin
  ClearExecutableLines;
  LineNumbers := FForm.GetExecutableLines( UnitName );
  for I := 0 to Length( LineNumbers )-1 do
    FExecutableLines[ LineNumbers[I] ] := True;
  Editor.InvalidateGutter;
end;

procedure TEditorPage.SetFileName(const Value: string);
begin
  Hint := Value; // << where file name is stored
  Caption := JustFileName( Value );
  if FForm.FileIsProjectSource( Value ) then
    Caption := Caption + ' *';
end;



procedure TEditorPage.SetIsReadOnly(const Value: boolean);
begin
  if Value <> IsReadOnly then
    begin
    FEditor.ReadOnly := Value;
    if FileExists( FileName ) then
      FileSetReadOnly( Filename, Value );
    end;
end;

procedure TEditorPage.PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  iLineHeight, iGutterWidth : integer;

  procedure DrawRuler( ALine, X, Y : integer );
  var
    S : string;
    R : TRect;
    I : integer;
  begin
  if (ALine = 1) or (ALine = Editor.CaretY) or (ALine mod 10 = 0) then
    begin
    S := IntToStr( ALine );
    R := Rect( X,Y, iGutterWidth-2, Y + iLineHeight );
    DrawText(
      ACanvas.Handle,
      S,
      Length(S),
      R,
      DT_RIGHT );
    end
   else
     begin
     if ALine mod 5 = 0 then
       I := 5
      else
       I := 2;
     Inc( Y, iLineHeight div 2 );
     ACanvas.MoveTo( iGutterWidth-I, Y);
     ACanvas.LineTo( iGutterWidth, Y);
     end;
  end;

var
  X, Y: integer;
  ImgIndex: integer;
  R : TRect;
begin
  iLineHeight := Editor.LineHeight;
  iGutterWidth := Editor.Gutter.Width;

  // Ruler background
  ACanvas.Brush.Color := Lighten( clBtnFace, 6 );
  R := Rect( 24, 0, iGutterWidth, Editor.Height );
  ACanvas.FillRect( R );

  // Ruler cosmetics..
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := clGray;
  ACanvas.Pen.Color := clGray;

  FirstLine := Editor.RowToLine(FirstLine);
  LastLine := Editor.RowToLine(LastLine);
  X := 4;
  while FirstLine <= LastLine do
  begin
    Y := (iLineHeight - FForm.SmallImages.Height) div 2
         + iLineHeight * (Editor.LineToRow(FirstLine) - Editor.TopLine);

    If FirstLine = FCurrentLine then
      begin
      if GetBreakpointStatus( FirstLine ) <> bpsNone then
        ImgIndex := iiCurrentLineBreakpoint
      else
        If FForm.dwsDebugger1.State = dsDebugSuspended then
          ImgIndex := iiForwardArrow
         else
          ImgIndex := iiExecutableLine
      end
     else
      Case GetBreakpointStatus( FirstLine ) of
        bpsBreakpoint :
          If IsExecutableLine( FirstLine ) then
            ImgIndex := iiBreakpoint
           else
            ImgIndex := iiBreakpointDisabled;
        bpsBreakpointDisabled :
          ImgIndex := iiBreakpointDisabled;
       else
         If IsExecutableLine( FirstLine ) then
           ImgIndex := iiExecutableLine
          else
           ImgIndex := -1;
      end;

    if ImgIndex >= 0 then
      FForm.SmallImages.Draw(ACanvas, X, Y, ImgIndex);

    DrawRuler( FirstLine, X, Y );

    Inc(FirstLine);
  end;
end;



procedure TEditorPage.SetCurrentLine(ALine: integer);
begin
  if fCurrentLine <> ALine then
  begin
    Editor.InvalidateGutterLine(fCurrentLine);
    Editor.InvalidateLine(fCurrentLine);
    fCurrentLine := ALine;
    if (fCurrentLine > 0) and (Editor.CaretY <> fCurrentLine) then
      Editor.CaretXY := BufferCoord(1, fCurrentLine);
    Editor.InvalidateGutterLine(fCurrentLine);
    Editor.InvalidateLine(fCurrentLine);
  end;
end;



function TEditorPage.IsExecutableLine( ALine : integer ) : boolean;
begin
  If ALine < Length( FExecutableLines ) then
    Result := FExecutableLines[ALine]
   else
    Result := False;
end;


function TEditorPage.GetBreakpointStatus( ALine : integer ) : TBreakpointStatus;
var
  Test, Found : TdwsDebuggerBreakpoint;
  I : integer;
begin
  Result := bpsNone;
  If FForm.dwsDebugger1.Breakpoints.Count = 0 then
    Exit;

  Test :=TdwsDebuggerBreakpoint.Create;
  try
    Test.Line:= ALine;
    Test.SourceName := UnitName;

    I := FForm.dwsDebugger1.Breakpoints.IndexOf( Test );
    if I <> -1 then
      begin
      Found := FForm.dwsDebugger1.Breakpoints[I];
      if Found.Enabled then
        Result := bpsBreakpoint
       else
        Result := bpsBreakpointDisabled;
        end;
  finally
    FreeAndNil( Test );
  end;
end;




procedure TEditorPage.SynEditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
const
  BreakpointColor = TColor($FFA0A0);
  CurrentLineColor = TColor($A0A0F0);
  CurrentLineSteppingColor = TColor($A0C0F0);
begin
  if Line = FCurrentLine then
    begin
    Special := TRUE;
    FG := clBlack;
    if FForm.dwsDebugger1.State = dsDebugSuspended then
      BG := CurrentLineSteppingColor
     else
      BG := CurrentLineColor
    end
   else
    If GetBreakpointStatus( Line ) = bpsBreakpoint then
      begin
      Special := TRUE;
      FG := clBlack;
      BG := BreakpointColor;
     end;
end;

function TEditorPage.UnitName: string;
begin
  If IsProjectSourceFile then
    Result := '*MainModule*'
   else
    result := JustFileName( FileName );
end;

procedure TEditorPage.SynEditorClick(Sender: TObject);
begin
  TSynEdit( Sender).InvalidateGutter;
end;

procedure TEditorPage.SynEditorKeyDown( Sender: TObject; var Key: Word;  Shift: TShiftState );
begin
  inherited;
//  Exit;

  if Key = VK_OEM_PERIOD then
    PostMessage( FForm.Handle, WM_CodeSuggest,  0, 0 );
end;

procedure TEditorPage.SynEditorCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  Case Command of
    ecUp, ecDown :
      TSynEdit( Sender).InvalidateGutter;
  End;
end;

procedure TEditorPage.SynEditorGutterClick(Sender: TObject;
  Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
var
  iLine : integer;
begin
  iLine := Editor.RowToLine(Line);
  If iLine < Length( FExecutableLines) then
    begin
    If GetBreakpointStatus( Line ) <> bpsNone then
      ClearBreakpoint( iLine )
     else
       AddBreakpoint( iLine, True );
    Editor.Repaint;
    end;
end;



procedure TEditorPage.SaveToFile( APromptOverwrite : boolean );
begin
  if not FileExists( FileName ) or
    not APromptOverwrite or ConfirmDlgYesNoAbort(
      Format( 'File "%s" already exists. Overwrite it?', [FileName] )) then
    begin
    StringToTextFile(
      Editor.Lines.Text,
      FileName );
    Editor.Modified := False;
    end;
end;


procedure TEditorPage.SaveIfModified( APromptOverwrite : boolean );
begin
  If Editor.Modified then
    if not APromptOverwrite or (IsProjectSourceFile and not FileExists( FileName)) or
     ConfirmDlgYesNoAbort(
      Format( 'File "%s" has changed. Save it now?',  [ ExtractFileName( FileName ) ] )) then
        SavetoFile( False );
end;


procedure TEditorPage.SaveAs;
begin
  FForm.SaveSourceDialog.FileName := ExtractFileName( FileName);
  If FForm.SaveSourceDialog.Execute then
    begin
    Filename := FForm.SaveSourceDialog.FileName;
    SavetoFile( False );
    end;

end;

// TabRight
//
function TEditorPage.TabRight : Integer;
begin
   Result:=FTabLeft+FTabWidth;
end;

// CloseButtonRect
//
function TEditorPage.CloseButtonRect : TRect;
begin
   Result.Right:=FTabLeft+FTabWidth-cMargin-cSlantMargin;
   Result.Left:=Result.Right-cCloseButtonSize;
   Result.Top:=1+(FForm.imgTabs.Height-cCloseButtonSize) div 2;
   Result.Bottom:=Result.Top+cCloseButtonSize;
end;

{$Message 'Add editor action images'}

end.
