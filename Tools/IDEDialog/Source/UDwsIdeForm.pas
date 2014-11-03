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

{$I DWS.inc}

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Themes, UxTheme, Dialogs, StdCtrls, ExtCtrls, ActnList, ComCtrls,
  StdActns, Menus, ToolWin, ActnCtrls, ImgList, Diagnostics, XMLIntf, XMLDoc,

  dwsExprs, dwsComp, dwsCompiler, dwsDebugger, dwsStringResult, dwsErrors,
  dwsFunctions, dwsUtils, dwsSymbols, dwsUnitSymbols, dwsStrings,

  SynEdit, SynEditHighlighter, SynHighlighterDWS, SynEditTypes,
  SynEditKeyCmds, SynEditMiscClasses, SynEditSearch, SynEditPlugins,
  SynMacroRecorder, SynCompletionProposal,

  UDwsIdeDefs, UDwsIdeConfig, UDwsIdeLocalVariablesFrame, UDwsIdeWatchesFrame,
  UDwsIdeCallStackFrame, UDwsIdeGotoLine;

const
  ecOpenFileUnderCursor = ecUserFirst;
  ecToggleDeclImpl = ecUserFirst + 1;

type
  EDwsIde      = class(Exception);

  TDwsIdeForm  = class;
  TEditorPage  = class;

  TBreakpointStatus = (bpsNone, bpsBreakpoint, bpsBreakpointDisabled);

  TLineChangedState = (csOriginal, csModified, csSaved);
  TLineNumbers = array of Integer;

  TEditorPage = class(TWinControl)
  private
    FEditor: TSynEdit;
    FForm: TDwsIdeForm;
    FExecutableLines: TBits;
    FLineChangedState: array of TLineChangedState;
    FTabLeft: Integer;
    FTabWidth: Integer;
    FCurrentLine: Integer;
    FUnderLine: Integer;

    function TabRight: Integer;
    function CloseButtonRect: TRect;

    function  GetFilename: TFileName;
    procedure SetFileName(const Value: TFileName);

    procedure SetCurrentLine(ALine: Integer; ACol: Integer = 1);
    procedure SynEditGutterPaint(Sender: TObject; aLine, X, Y: Integer);
    procedure SynEditorClick(Sender: TObject);
    procedure SynEditorKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure SynEditorSpecialLineColors(Sender: TObject;
                 Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure SynEditorGutterClick(Sender: TObject; Button: TMouseButton; X, Y,
      Line: Integer; Mark: TSynEditMark);
    procedure SynEditorMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SynEditorCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    function GetIsReadOnly: Boolean;
    procedure SetIsReadOnly(const Value: Boolean);
    function GetIsProjectSourcefile: Boolean;
    procedure DoOnEditorChange(ASender: TObject);

    procedure AddBreakpoint(ALineNum: Integer; AEnabled: Boolean);
    procedure ClearBreakpoint(ALineNum: Integer);
    function GetBreakpointStatus(ALine: Integer): TBreakpointStatus;

    procedure ClearExecutableLines;
    procedure InitExecutableLines;
    function IsExecutableLine(ALine: Integer): Boolean; inline;

    procedure InitLineChangeStates;
    procedure ToggleLineChangedStates;
    function GetLineChangeState(ALine: Integer): TLineChangedState; inline;
  public
    constructor Create(AOwner: TDwsIdeForm;
                 const AFileName: TFileName;
                       ALoadFile: Boolean); reintroduce;
    destructor  Destroy; override;

    procedure SaveToFile(APromptOverwrite: Boolean);
    procedure SaveIfModified(APromptOverwrite: Boolean);
    procedure SaveAs;

    procedure ToggleDeclImpl;
    procedure GotoLineNumber;
    procedure OpenFileUnderCursor;

    function  GotoIdentifier(const AIdentifier: string): Boolean;
    procedure ShowExecutableLines;

    function UnitName: string;
    property Editor: TSynEdit read FEditor;
    property FileName: TFileName read GetFilename write SetFileName;
    property IsReadOnly: Boolean read GetIsReadOnly write SetIsReadOnly;
    property IsProjectSourcefile: Boolean read GetIsProjectSourcefile;
  end;


  TOutputWindowStringResultType = class (TdwsStringResultType)
    constructor Create(AOwner: TComponent; ADwsIdeForm: TDwsIdeForm); reintroduce;
  private
    FDwsIdeForm: TDwsIdeForm;
  protected
    procedure DoAddString(Result: TdwsStringResult; var str: string); override;
    procedure DoReadLn(Result: TdwsStringResult; var str: string); override;
    procedure DoReadChar(Result: TdwsStringResult; var str: string); override;
  end;


  TIDESettingsRec = record
    FormRect: TRect;
    RightPanelWidth,
    BottomPanelHeight: Integer;
  end;


  TDwsNewProjectEvent = procedure(const AProjectFileName: string) of object;
  
  
  TDwsIdeForm = class(TForm, IDwsIde)
    ActionBuild: TAction;
    ActionClearAllBreakpoints: TAction;
    ActionCloseAllOtherPages: TAction;
    ActionClosePage: TAction;
    ActionEditClearOutputWindow: TAction;
    ActionEditCopyToClipboard: TEditCopy;
    ActionEditCut: TEditCut;
    ActionEditDelete: TEditDelete;
    ActionEditPaste: TEditPaste;
    ActionEditSelectAll: TEditSelectAll;
    ActionEditToggleReadOnly: TAction;
    ActionEditUndo: TEditUndo;
    ActionExit: TFileExit;
    ActionFileCloseAll: TAction;
    ActionFileNewIncludeFile: TAction;
    ActionFileNewProject: TAction;
    ActionFileNewUnit: TAction;
    ActionFileOpenProject: TAction;
    ActionFileSave: TAction;
    ActionFileSaveAs: TAction;
    ActionFileSaveProjectAs: TAction;
    ActionGotoHomePosition: TAction;
    ActionGotoLineNumber: TAction;
    ActionList: TActionList;
    ActionOpenFile: TAction;
    ActionProgramReset: TAction;
    ActionRun: TAction;
    ActionRunWithoutDebugging: TAction;
    ActionSearchFind: TSearchFind;
    ActionSearchReplace: TSearchReplace;
    ActionShowExecutionPoint: TAction;
    ActionStepOver: TAction;
    ActionTraceInto: TAction;
    ActionViewProjectSource: TAction;
    ActionViewSymbols: TAction;
    Debugger: TdwsDebugger;
    DwsIdeCallStackFrame: TDwsIdeCallStackFrame;
    DwsIdeLocalVariablesFrame: TDwsIdeLocalVariablesFrame;
    DwsIdeWatchesFrame: TDwsIdeWatchesFrame;
    EditorPagePopupMenu: TPopupMenu;
    EditorPageTabContextMenu: TPopupMenu;
    ImageTabs: TImage;
    ListBoxMessages: TListBox;
    MainMenu: TMainMenu;
    MemoOutputWindow: TMemo;
    MenuItemBuild: TMenuItem;
    MenuItemCloseAllOtherPages: TMenuItem;
    MenuItemClosePage: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemEditClearOutputWindow: TMenuItem;
    MenuItemEditCopy: TMenuItem;
    MenuItemEditCut: TMenuItem;
    MenuItemEditDelete: TMenuItem;
    MenuItemEditPaste: TMenuItem;
    MenuItemEditReadOnly: TMenuItem;
    MenuItemEditSelectAll: TMenuItem;
    MenuItemEditUndo: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileCloseAll: TMenuItem;
    MenuItemFileClosePage: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFileNew: TMenuItem;
    MenuItemFileNewInclude: TMenuItem;
    MenuItemFileNewProject: TMenuItem;
    MenuItemFileNewUnit: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFileOpenProject: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    MenuItemFileSaveProjectAs: TMenuItem;
    MenuItemPageTabCloseAllOtherPages: TMenuItem;
    MenuItemPageTabCloseFile: TMenuItem;
    MenuItemPageTabPages: TMenuItem;
    MenuItemPageTabReadOnly: TMenuItem;
    MenuItemPageTabSave: TMenuItem;
    MenuItemPageTabSaveAs: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemProject: TMenuItem;
    MenuItemProjectBuild: TMenuItem;
    MenuItemReadOnly: TMenuItem;
    MenuItemRun: TMenuItem;
    MenuItemRun1: TMenuItem;
    MenuItemRunClearAllBreakpoints: TMenuItem;
    MenuItemRunReset: TMenuItem;
    MenuItemRunShowExecutionPoint: TMenuItem;
    MenuItemRunStart: TMenuItem;
    MenuItemRunStepOver: TMenuItem;
    MenuItemRunTraceInto: TMenuItem;
    MenuItemRunWithoutDebugging: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSearch: TMenuItem;
    MenuItemSearchFind: TMenuItem;
    MenuItemSearchReplace: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemViewGotoHomePosition: TMenuItem;
    MenuItemViewProjectSource: TMenuItem;
    MenuItemViewSymbols: TMenuItem;
    N1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    OpenFileDialog: TFileOpenDialog;
    OpenProjectDialog: TFileOpenDialog;
    PageControlBottomWindows: TPageControl;
    PanelBottom: TPanel;
    PanelEditor: TPanel;
    PanelMain: TPanel;
    PanelRight: TPanel;
    pnlPageControl: TPanel;
    SaveProjectDialog: TFileSaveDialog;
    SaveSourceDialog: TFileSaveDialog;
    SmallImages: TImageList;
    SplitterBottom: TSplitter;
    SplitterRight: TSplitter;
    StatusBar: TStatusBar;
    SynCodeCompletion: TSynCompletionProposal;
    SynEditSearch: TSynEditSearch;
    SynMacroRecorder: TSynMacroRecorder;
    SynParameters: TSynCompletionProposal;
    TabSheetMessages: TTabSheet;
    TabSheetOutput: TTabSheet;
    UpdateTimer: TTimer;
    constructor Create(AOwner: TComponent; const AOptions: TDwsIdeOptions); reintroduce;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure ActionBuildExecute(Sender: TObject);
    procedure ActionClearAllBreakpointsExecute(Sender: TObject);
    procedure ActionEditClearOutputWindowExecute(Sender: TObject);
    procedure ActionEditClearOutputWindowUpdate(Sender: TObject);
    procedure ActionCloseAllOtherPagesExecute(Sender: TObject);
    procedure ActionCloseAllOtherPagesUpdate(Sender: TObject);
    procedure ActionClosePageExecute(Sender: TObject);
    procedure ActionClosePageUpdate(Sender: TObject);
    procedure ActionFileCloseAllExecute(Sender: TObject);
    procedure ActionFileNewIncludeFileExecute(Sender: TObject);
    procedure ActionFileNewProjectExecute(Sender: TObject);
    procedure ActionFileNewUnitExecute(Sender: TObject);
    procedure ActionFileSaveAsExecute(Sender: TObject);
    procedure ActionFileSaveAsUpdate(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionFileSaveUpdate(Sender: TObject);
    procedure ActionGotoHomePositionExecute(Sender: TObject);
    procedure ActionGotoHomePositionUpdate(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionFileOpenProjectExecute(Sender: TObject);
    procedure ActionProgramResetExecute(Sender: TObject);
    procedure ActionProgramResetUpdate(Sender: TObject);
    procedure ActionRunExecute(Sender: TObject);
    procedure ActionRunUpdate(Sender: TObject);
    procedure ActionRunWithoutDebuggingExecute(Sender: TObject);
    procedure ActionRunWithoutDebuggingUpdate(Sender: TObject);
    procedure ActionFileSaveProjectAsExecute(Sender: TObject);
    procedure ActionShowExecutionPointExecute(Sender: TObject);
    procedure ActionShowExecutionPointUpdate(Sender: TObject);
    procedure ActionStepOverExecute(Sender: TObject);
    procedure ActionStepOverUpdate(Sender: TObject);
    procedure ActionEditToggleReadOnlyExecute(Sender: TObject);
    procedure ActionEditToggleReadOnlyUpdate(Sender: TObject);
    procedure ActionTraceIntoExecute(Sender: TObject);
    procedure ActionTraceIntoUpdate(Sender: TObject);
    procedure ActionViewProjectSourceExecute(Sender: TObject);
    procedure ActionViewProjectSourceUpdate(Sender: TObject);
    procedure ActionViewSymbolsExecute(Sender: TObject);
    procedure ActionViewSymbolsUpdate(Sender: TObject);
    procedure DebuggerDebug(exec: TdwsExecution; expr: TExprBase);
    procedure DebuggerDebugStart(exec: TdwsExecution);
    procedure DebuggerDebugStop(exec: TdwsExecution);
    procedure DebuggerEnterFunc(exec: TdwsExecution; expr: TExprBase);
    procedure DebuggerLeaveFunc(exec: TdwsExecution; expr: TExprBase);
    procedure DebuggerStateChanged(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure ImageTabsMouseLeave(Sender: TObject);
    procedure ImageTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ListBoxMessagesDblClick(Sender: TObject);
    procedure pcEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlPageControlResize(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure SynCodeCompletionExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure SynCodeCompletionShow(Sender: TObject);
    procedure SynParametersExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure ActionGotoLineNumberExecute(Sender: TObject);
    procedure ActionGotoLineNumberUpdate(Sender: TObject);
  private
    FScript: TDelphiWebScript;
    FOnNewProject: TDwsNewProjectEvent;

    FpcEditorLastMouseButton: TMouseButton;
    FpcEditorLastMouseXY: TPoint;

    FProjectFileName: TFileName;

    FProgram: IdwsProgram;

    FScriptFolder: string;

    FOptions: TDwsIdeOptions;
    FGotoForm: TDwsIdeGotoLineNumber;

    FIDESettingsRec: TIDESettingsRec;

    FHomePositionCaptionSuffix: string;

    FActivePageIndex: Integer;
    FHoveredPageIndex: Integer;
    FBasePageIndex: Integer;
    FHoveredCloseButton: Boolean;
    FHoveredLeftArrow, FHoveredRightArrow: Boolean;
    FLeftArrowActive, FRightArrowActive: Boolean;
    FTabArrowLeft, FTabArrowRight: TRect;
    FPages: TSimpleList<TEditorPage>;

    procedure GotoHomePosition;
    function  CanGotoHomePosition: Boolean;
    function  TryRunSelection(ADebug: Boolean): Boolean;
    procedure DoDebugMessage(const msg: string);
    procedure EditorPageAddNew(const AFileName: TFileName; ALoadfile: Boolean );
    function  ProjectSourceScript: string;
    function  EditorPageCount: Integer;
    function  EditorPage(AIndex: Integer): TEditorPage;
    function  CurrentEditorPage: TEditorPage;
    procedure EditorPageClose(AIndex: Integer);
    procedure EditorCloseAllPages(AExceptIndex: Integer = -1);
    procedure EditorSaveAllIfModified(APromptOverwrite: Boolean);
    function  HasProject: Boolean;
    function  NameToEditorPageIndex(const AName: string): Integer;
    function OpenEditorPage(AName: string): Boolean;
    procedure SetEditorCurrentPageIndex(const Value: Integer);
    procedure SetProjectFileName(const Value: TFileName);
    procedure GotoScriptPos(AScriptPos: TScriptPos; AHiddenMainModule: Boolean = False);
    procedure ResetProgram;
    function  GetExecutableLines(const AUnitName: string): TLineNumbers;
    procedure SetScript(const Value: TDelphiWebScript);
    procedure SetScriptFolder(const Value: string);
    procedure MakeSettingsRec;
    procedure AddMessage(const AMessage: string; AScriptPos: PScriptPos = nil);
    procedure ClearMessagesWindow;
    procedure ClearOutputWindow;
    procedure ListSymbolTable(ATable: TSymbolTable);
    function UnitMainScript(const AUnitName, AIdentifier: string): string;

    function  CurrentEditor: TSynEdit;
    function  HasEditorPage: Boolean;
    function  GetProjectSourceFileName: TFileName;
    procedure SetProjectSourceFileName(const Value: TFileName);

    function  FileIsOpenInEditor(const AFileName: TFileName): Boolean;
    function  FileIsProjectSource(const AFileName: TFileName): Boolean;
    function  ProjectSourceFileIndex: Integer;
    function  ModifyFileNameToUniqueInProject(const AFileName: TFileName): string;
    function  SaveProjectAs: Boolean;
    function  ProjectFileNameToProjectSourceFileName(const AProjectfileName: TFileName): string;

    procedure DoOnClickEditorPageTabContextMenuPageItem(ASender: TObject);

    procedure LoadProjectFile(const AProjectFileName: TFileName);
    procedure NewProjectFile(const AProjectFileName: TFileName);
    procedure SaveProjectFileAs(const AProjectFileName: TFileName);

    procedure ShowExecutableLines;
    procedure ClearCurrentLine;
    procedure ClearAllBreakpoints;
    procedure ClearExecutableLines;
    procedure AddStatusMessage(const AStr: string);
    procedure Compile(ABuild: Boolean; const AScript: string = '');
    function  IsCompiled: Boolean;

    procedure ListSymbols;

    procedure LoadSettings(
           var AProjectFileName: TFileName;
           var AIDESettingsRec: TIDESettingsRec);
    procedure SaveSettings(
         const AProjectFileName: TFileName;
         const AIDESettingsRec: TIDESettingsRec);

    procedure RunFunctionMethodByName(const AUnit, AName: string; AWithDebugging, APrompt: Boolean);

    procedure RefreshTabs;
    procedure RefreshTabArrows;

    function  IndexOfTab(x: Integer): Integer;
    function GetCompiledScript: IdwsProgram;
    function GetGotoForm: TDwsIdeGotoLineNumber;

    property EditorCurrentPageIndex: Integer read FActivePageIndex write SetEditorCurrentPageIndex;
    property ProjectSourceFileName: TFileName read GetProjectSourceFileName write SetProjectSourceFileName;
    property ScriptFolder: string read FScriptFolder write SetScriptFolder;
    property GotoForm: TDwsIdeGotoLineNumber read GetGotoForm;
  public
    // IDwsIde
    // -------------------------------------------------------------------------
    function  DwsIde_GetDebugger: TdwsDebugger;
    // -------------------------------------------------------------------------

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;


    property ProjectFileName: TFileName
               read FProjectFileName
               write SetProjectFileName;

    property Script: TDelphiWebScript
               read FScript
               write SetScript;

    property OnNewProject: TDwsNewProjectEvent read FOnNewProject write FOnNewProject;
  end;


procedure DwsIDE_ShowModal(AScript: TDelphiWebScript); overload;
procedure DwsIDE_ShowModal(AScript: TDelphiWebScript; const AOptions: TDwsIdeOptions); overload;

implementation

{$R *.dfm}

uses
  Registry,
  Math,
  dwsXPlatform,
  dwsSuggestions,
  dwsDebugFunctions,
  dwsTokenizer,
  ShlObj;

resourcestring
  RStrIdeDesktopCopy = 'For this IDE demonstration, please place a copy '
    + 'of the ''DWS Script Files'' folder from project source on to your '
    + 'desktop.';
  RStrScriptFolderNotFound = 'Script folder "%s" does not exist';
  RStrScriptCannotBeNil = 'Script cannot be nil - the IDE requires a script '
    + 'to debug';
  RStrScriptDoesNotDefineMainPath = 'Script does not define a main path';
  RStrFileAlreadyExistsOverwrite = 'File "%s" already exists. Overwrite it?';
  RStrFileHasChanged = 'File "%s" has changed. Save it now?';
  RStrAbandonDebugging = 'Abandon debugging?';
  RStrCompileStarted = 'Compile started';
  RStrCompileCompleteWarnHints = 'Compile complete with hints/warnings';
  RStrRunFunctionMethod = 'Run function/method "%s"?';
  RStrRunning = 'Running';
  RStrPaused = 'Paused';
  RStrErrors = 'Errors';
  RStrNoParametersRequired = '"<no parameters required>"';
  RStrProjectFileDoesNotExist = 'Project file does not exist (%s)';
  RStrCannotRunWithoutProjectFile = 'Cannot run without a project file';
  RStrProgramCompleted = 'Program completed';


const
  CImageIndexExecutableLine = 13;
  CImageIndexForwardArrow = 16;
  CImageIndexCurrentLineBreakpoint = 15;
  CImageIndexBreakpoint = 12;
  CImageIndexBreakpointDisabled = 14;
  CImageIndex_ProjectSourceFile = 28;
  CImageIndex_Script = 26;
  CImageIndex_NonScript = 6;
  CImageIndex_IncludeFile = 29;

  CMargin = 4;
  CSlantMargin = 10;
  CCloseButtonSize = 12;
  CArrowButtonSize = 15;

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


function Lighten(AColor: TColor; AFactor: Byte): TColor;
// Lightens a color by this amount
var
  R, G, B: Byte;
begin
  AColor := ColorToRGB(AColor);

  R := GetRValue(AColor);
  G := GetGValue(AColor);
  B := GetBValue(AColor);

  Inc(R, AFactor);
  Inc(G, AFactor);
  Inc(B, AFactor);

  Result := RGB(R, G, B);
end;


function IsHostedControl(AControl: TControl): Boolean;
// Returns TRUE if this control is hosted within another control
begin
  Result := AControl.Parent <> nil;
end;


{$IFDEF DELPHI_XE2_PLUS} // >= Delphi XE2
function IDEStyleServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;
{$ELSE}
function IDEStyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$ENDIF}


procedure DwsIDE_ShowModal(AScript: TDelphiWebScript);
var
  DwsIdeOptions: TDwsIdeOptions;
begin
  if SysUtils.Win32MajorVersion >= 6 then // Vista or later...
    DwsIdeOptions := IdeOptions_VistaOrLater
  else
    DwsIdeOptions := IdeOptions_Legacy;

  DwsIDE_ShowModal(AScript, DwsIdeOptions);
end;


procedure DwsIDE_ShowModal(AScript: TDelphiWebScript; const AOptions: TDwsIdeOptions);
var
  Frm: TDwsIdeForm;
  SaveResultType: TdwsResultType;
begin
  if Assigned(AScript) then
    SaveResultType := AScript.Config.ResultType
   else
     SaveResultType := nil;
  try
    Frm := TDwsIdeForm.Create(Application, AOptions);
    try
      Frm.Script := AScript;
      Frm.ShowModal;
    finally
      Frm.Free;
    end;
  finally
    if Assigned(AScript) then
      AScript.Config.ResultType := SaveResultType;
  end;
end;


function JustFileName(const AFileName: TFileName): string;
// Returns only the file name without dir or ext
begin
  Result := ChangeFileExt(ExtractFileName(AFileName), '');
end;


procedure ErrorDlg(const AStr: string);
begin
  TaskMessageDlg('Error', AStr, mtError, [mbok], 0);
end;

function ConfirmDlg(const AStr: string): Boolean;
begin
  Result := TaskMessageDlg('Confirm', AStr, mtConfirmation, [mbYes, mbNo], 0) = idYes;
end;

function ConfirmDlgYesNoAbort(const AStr: string): Boolean;
begin
  Result := False;
  case TaskMessageDlg('Confirm', AStr, mtError, [mbYes, mbNo, mbCancel], 0) of
    idYes:
      Result := True;
    idNo:
      Exit;
    else
      Abort;
  end;
end;



procedure SymbolsToStrings(ATable: TSymbolTable; AStrings: TStrings);
// Dumps this table symbol names to AStrings recursively.

  procedure AddSymbolTable(ATable: TSymbolTable);
  var
    I: Integer;
    Sym: TSymbol;
  begin
    for I := 0 to ATable.Count - 1 do
      begin
      Sym := ATable.Symbols[I];
      if Sym is TUnitSymbol then
        AddSymbolTable(TUnitSymbol(Sym).Table)
       else
        AStrings.Add(Sym.Name + '   ' + Sym.ToString +  '  (' + Sym.ClassName + ')');
      end;
  end;

begin
  AddSymbolTable(ATable);
end;



{ TEditorPageSynEditPlugin }

type
  TEditorPageSynEditPlugin = class(TSynEditPlugin)
  protected
    FPage: TEditorPage;
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
    procedure PaintTransient(ACanvas: TCanvas; ATransientType: TTransientType);
      override;
  public
    constructor Create(APage: TEditorPage);
  end;

constructor TEditorPageSynEditPlugin.Create(APage: TEditorPage);
begin
  inherited Create(APage.Editor);
  FPage := APage;
end;

procedure TEditorPageSynEditPlugin.LinesInserted(FirstLine, Count: Integer);
var
  I, iLineCount: Integer;
begin
  // Track the executable lines
  iLineCount := FPage.Editor.Lines.Count;
  FPage.FExecutableLines.Size := iLineCount;
  for I := iLineCount - 1 downto FirstLine + Count do
    FPage.FExecutableLines[i] := FPage.FExecutableLines[I - Count];
  for I := FirstLine + Count - 1 downto FirstLine do
    FPage.FExecutableLines[i] := False;

  SetLength(FPage.FLineChangedState, iLineCount);
  for I := iLineCount - 1 downto FirstLine + Count do
    FPage.FLineChangedState[i] := FPage.FLineChangedState[I - Count];
  for I := FirstLine + Count - 1 downto FirstLine - 1 do
    FPage.FLineChangedState[i] := csModified;

  // Track the breakpoint lines in the debugger
  with FPage.FForm.Debugger do
    for I := 0 to Breakpoints.Count - 1 do
      if Breakpoints[I].SourceName = FPage.UnitName then
        if Breakpoints[I].Line >= FirstLine then
           Breakpoints[I].Line := Breakpoints[I].Line + Count;

  // Redraw the gutter for updated icons.
  FPage.Editor.InvalidateGutter;
end;

procedure TEditorPageSynEditPlugin.PaintTransient(ACanvas: TCanvas;
  ATransientType: TTransientType);
var
  Pt: TPoint;
  Rct: TRect;
  MouseBufferCoord: TBufferCoord;
  Attri: TSynHighlighterAttributes;
  TokenType, Start: Integer;
  TokenName: String;
  OldFont: TFont;
begin
  // only handle after transient
  if ATransientType <> ttAfter then
    Exit;

  // only continue if [CTRL] is pressed
  if not (ssCtrl in KeyboardStateToShiftState) then
    Exit;

  Pt := Editor.ScreenToClient(Mouse.CursorPos);
  MouseBufferCoord := Editor.DisplayToBufferPos(
    Editor.PixelsToRowColumn(Pt.X, Pt.Y));
  Editor.GetHighlighterAttriAtRowColEx(MouseBufferCoord, TokenName, TokenType,
    Start, Attri);

  if TtkTokenKind(TokenType) <> tkIdentifier then
    Exit;

  with Editor do
    Pt := RowColumnToPixels(BufferToDisplayPos(WordStartEx(MouseBufferCoord)));

  Rct := Rect(Pt.X, Pt.Y, Pt.X + Editor.CharWidth * Length(TokenName),
    Pt.Y + Editor.LineHeight);

  OldFont := TFont.Create;
  try
    OldFont.Assign(ACanvas.Font);
    ACanvas.Font.Color := clBlue;
    ACanvas.Font.Style := [fsUnderline];
    ACanvas.TextRect(Rct, Pt.X, Pt.Y, TokenName);
    ACanvas.Font := OldFont;
  finally
    OldFont.Free;
  end;
end;

procedure TEditorPageSynEditPlugin.LinesDeleted(FirstLine, Count: Integer);
var
  I: Integer;
begin
  // Track the executable lines
  for I := FirstLine - 1 to FPage.FExecutableLines.Size - Count - 1 do
    FPage.FExecutableLines[i] := FPage.FExecutableLines[I + Count];
  FPage.FExecutableLines.Size := FPage.FExecutableLines.Size - Count;

  // Track the executable lines
  for I := FirstLine - 1 to High(FPage.FLineChangedState) - Count do
    FPage.FLineChangedState[i] := FPage.FLineChangedState[I + Count];
  SetLength(FPage.FLineChangedState, Length(FPage.FLineChangedState) - Count);

  // Track the breakpoint lines in the debugger
  with FPage.FForm.Debugger do
    for I := 0 to Breakpoints.Count - 1 do
      if Breakpoints[I].SourceName = FPage.UnitName then
        if Breakpoints[I].Line >= FirstLine then
           Breakpoints[I].Line := Breakpoints[I].Line - Count;

  // Redraw the gutter for updated icons.
  FPage.Editor.InvalidateGutter;
end;


{ TEditorPage }

constructor TEditorPage.Create(AOwner: TDwsIdeForm; const AFileName: TFileName;
  ALoadFile: Boolean);

  procedure InitEditor;
  begin
    FEditor := TSynEdit.Create(Self);
    FEditor.OnChange := DoOnEditorChange;
    FEditor.Parent  := Self;
    FEditor.Align   := alClient;
    FEditor.BorderStyle := bsNone;
    FEditor.Gutter.Width := 64;
    FEditor.PopupMenu := AOwner.EditorPagePopupMenu;
    FEditor.WantTabs := True;
    FEditor.FontSmoothing := fsmClearType;

    if Assigned(AOwner.FOptions.EditorHighlighterClass) then
      FEditor.Highlighter := AOwner.FOptions.EditorHighlighterClass.Create(Self);
    if AOwner.FOptions.EditorFontName <> '' then
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
      eoRightMouseMovesCursor,
      eoDragDropEditing,
      eoEnhanceEndKey,
      eoGroupUndo,
      eoSmartTabDelete
      ];

    with FEditor.Keystrokes.Add do
    begin
      Command := ecGotoXY;
      Key := 47;
      Shift := [ssAlt];
    end;

    with FEditor.Keystrokes.Add do
    begin
      Command := ecOpenFileUnderCursor;
      Key := VK_RETURN;
      Shift := [ssCtrl];
    end;

    with FEditor.Keystrokes.Add do
    begin
      Command := ecToggleDeclImpl;
      Key := VK_UP;
      Shift := [ssCtrl, ssShift];
    end;

    with FEditor.Keystrokes.Add do
    begin
      Command := ecToggleDeclImpl;
      Key := VK_DOWN;
      Shift := [ssCtrl, ssShift];
    end;

    FForm.SynMacroRecorder.AddEditor(FEditor);
    FForm.SynParameters.AddEditor(FEditor);
    FForm.SynCodeCompletion.AddEditor(FEditor);
    with FForm.SynCodeCompletion.Columns.Add do
    begin
      // eventually uncomment this line with older SynEdit code versions
      ColumnWidth := 50;
    end;
    with FForm.SynCodeCompletion.Columns.Add do
      DefaultFontStyle := [fsBold];

    FEditor.OnSpecialLineColors := SynEditorSpecialLineColors;
    FEditor.OnGutterClick := SynEditorGutterClick;
    FEditor.OnGutterPaint := SynEditGutterPaint;
    FEditor.OnMouseMove := SynEditorMouseMove;
    FEditor.OnCommandProcessed := SynEditorCommandProcessed;
    FEditor.OnClick := SynEditorClick;
    FEditor.OnKeyDown := SynEditorKeyDown;

    TEditorPageSynEditPlugin.Create(Self);
  end;

begin
  inherited Create(AOwner);

  FForm := AOwner;
  FCurrentLine := -1;
  FExecutableLines := TBits.Create;

  FileName := AFileName;

  InitEditor;

  if ALoadFile and FileExists(AFileName) then
  begin
    FEditor.Lines.Text := LoadTextFromFile(AFileName);
    FEditor.ReadOnly  := FileIsReadOnly(AFileName);
  end;
  InitExecutableLines;
  InitLineChangeStates;

  FEditor.Modified := False;
end;

destructor TEditorPage.Destroy;
begin
  FForm.SynParameters.RemoveEditor(FEditor);
  FForm.SynCodeCompletion.RemoveEditor(FEditor);
  FForm.SynMacroRecorder.RemoveEditor(FEditor);
  FExecutableLines.Free;
  FEditor.Free;

  inherited;
end;

procedure TEditorPage.DoOnEditorChange(ASender: TObject);
begin
  FLineChangedState[FEditor.CaretY - 1] := csModified;
  FForm.EditorChange(ASender);
end;

function TEditorPage.GetFilename: TFileName;
begin
  Result := Hint;
end;

function TEditorPage.GetIsProjectSourceFile: Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), sDwsIdeProjectSourceFileExt);
end;

function TEditorPage.GetIsReadOnly: Boolean;
begin
  Result := FEditor.ReadOnly;
end;

function TEditorPage.GotoIdentifier(const AIdentifier: string): Boolean;
var
  I: Integer;
  S: string;
  bImplementation: Boolean;
begin
  Result := False;
  S := UpperCase(AIdentifier);
  bImplementation := False;
  for I := 0 to FEditor.Lines.Count - 1 do
  begin
    if Pos('IMPLEMENTATION', UpperCase(FEditor.Lines[I])) <> 0 then
      bImplementation := True
    else
    if bImplementation then
    begin
      Result := Pos(S, UpperCase(FEditor.Lines[I])) <> 0;
      if Result then
      begin
        FEditor.CaretY := I + 1;
        FEditor.CaretX := 1;
        FEditor.SearchReplace(AIdentifier, '', []); // << selects the identifier
        Exit;
      end;
    end;
  end;
end;

// AddBreakpoint
//
procedure TEditorPage.AddBreakpoint(ALineNum: Integer; AEnabled: Boolean);
var
  BP: TdwsDebuggerBreakpoint;
  bAdded: Boolean;
  I: Integer;
begin
  BP := TdwsDebuggerBreakpoint.Create;
  BP.Line := ALineNum;

  BP.SourceName := UnitName;

  I := FForm.Debugger.Breakpoints.AddOrFind(BP, bAdded);
  if not bAdded then
    BP.Free;
  FForm.Debugger.Breakpoints[I].Enabled := AEnabled;

  Editor.InvalidateGutterLine(ALineNum);
  Editor.InvalidateLine(ALineNum);
end;

// ClearBreakpoint
//
procedure TEditorPage.ClearBreakpoint(ALineNum: Integer);
var
  Test, Found: TdwsDebuggerBreakpoint;
  I: Integer;
begin
  if FForm.Debugger.Breakpoints.Count = 0 then
    Exit;

  Test := TdwsDebuggerBreakpoint.Create;
  try
    Test.Line := ALineNum;
    Test.SourceName := UnitName;

    I := FForm.Debugger.Breakpoints.IndexOf(Test);
    if I <> -1 then
    begin
      Found := FForm.Debugger.Breakpoints[I];
      FForm.Debugger.Breakpoints.Extract(Found);
      FreeAndNil(Found);
    end;
  finally
    FreeAndNil(Test);
  end;

  Editor.InvalidateGutterLine(ALineNum);
  Editor.InvalidateLine(ALineNum);
end;

// GetBreakpointStatus
//
function TEditorPage.GetBreakpointStatus(ALine: Integer): TBreakpointStatus;
var
  Test, Found: TdwsDebuggerBreakpoint;
  I: Integer;
begin
  Result := bpsNone;
  if FForm.Debugger.Breakpoints.Count = 0 then
    Exit;

  Test := TdwsDebuggerBreakpoint.Create;
  try
    Test.Line := ALine;
    Test.SourceName := UnitName;

    I := FForm.Debugger.Breakpoints.IndexOf(Test);
    if I <> -1 then
    begin
      Found := FForm.Debugger.Breakpoints[I];
      if Found.Enabled then
        Result := bpsBreakpoint
      else
        Result := bpsBreakpointDisabled;
    end;
  finally
    FreeAndNil(Test);
  end;
end;

// ClearExecutableLines
//
procedure TEditorPage.ClearExecutableLines;
var
  I: Integer;
begin
  for I := 0 to FExecutableLines.Size do
    FExecutableLines[I] := False;

  Editor.InvalidateGutter;
end;

// InitExecutableLines
//
procedure TEditorPage.InitExecutableLines;
begin
  FExecutableLines.Size := 0;
  if Editor.Lines.Count = 0 then
    FExecutableLines.Size := 1
  else
    FExecutableLines.Size := Editor.Lines.Count;
end;

// ShowExecutableLines
//
procedure TEditorPage.ShowExecutableLines;
var
  LineNumbers: TLineNumbers;
  I: Integer;
begin
  ClearExecutableLines;
  LineNumbers := FForm.GetExecutableLines(UnitName);
  for I := 0 to High(LineNumbers) do
    FExecutableLines[ LineNumbers[I] ] := True;
  Editor.InvalidateGutter;
end;

// InitLineStates
//
procedure TEditorPage.InitLineChangeStates;
begin
  SetLength(FLineChangedState, 0);
  if Editor.Lines.Count = 0 then
    SetLength(FLineChangedState, 1)
  else
    SetLength(FLineChangedState, Editor.Lines.Count);
end;

// ToggleLineChangedStates
//
procedure TEditorPage.ToggleLineChangedStates;
var
  Index: Integer;
begin
  for Index := 0 to High(FLineChangedState) do
    if FLineChangedState[Index] = csModified then
      FLineChangedState[Index] := csSaved;

  FEditor.InvalidateGutter;
end;

// SetFileName
//
procedure TEditorPage.SetFileName(const Value: TFileName);
begin
  Hint := Value; // << where file name is stored
  Caption := JustFileName(Value);
  if FForm.FileIsProjectSource(Value) then
    Caption := Caption + ' *';
end;

// SetIsReadOnly
//
procedure TEditorPage.SetIsReadOnly(const Value: Boolean);
begin
  if Value <> IsReadOnly then
  begin
    FEditor.ReadOnly := Value;
    if FileExists(FileName) then
      FileSetReadOnly(Filename, Value);
  end;
end;

// SetCurrentLine
//
procedure TEditorPage.SetCurrentLine(ALine: Integer; ACol: Integer = 1);
begin
  if FCurrentLine <> ALine then
  begin
    Editor.InvalidateGutterLine(FCurrentLine);
    Editor.InvalidateLine(FCurrentLine);
    FCurrentLine := ALine;
    if (FCurrentLine > 0) and (Editor.CaretY <> FCurrentLine) then
      Editor.CaretXY := BufferCoord(ACol, FCurrentLine);
    Editor.InvalidateGutterLine(FCurrentLine);
    Editor.InvalidateLine(FCurrentLine);
  end;
end;

// IsExecutableLine
//
function TEditorPage.IsExecutableLine(ALine: Integer): Boolean;
begin
  if ALine < FExecutableLines.Size then
    Result := FExecutableLines[ALine]
  else
    Result := False;
end;

// GetLineChangeState
//
function TEditorPage.GetLineChangeState(ALine: Integer): TLineChangedState;
begin
  if ALine < Length(FLineChangedState) then
    Result := FLineChangedState[ALine]
  else
    Result := csOriginal;
end;

// UnitName
//
function TEditorPage.UnitName: string;
begin
  if IsProjectSourceFile then
    Result := SYS_MainModule
  else
    Result := JustFileName(FileName);
end;

// SynEditorSpecialLineColors
//
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
    if FForm.Debugger.State = dsDebugSuspended then
      BG := CurrentLineSteppingColor
    else
      BG := CurrentLineColor
  end
  else
  if GetBreakpointStatus(Line) = bpsBreakpoint then
  begin
    Special := TRUE;
    FG := clBlack;
    BG := BreakpointColor;
  end;
end;

// SynEditGutterPaint
//
procedure TEditorPage.SynEditGutterPaint(Sender: TObject; aLine, X,
  Y: Integer);
var
  GutterWidth: Integer;
  ImgIndex: Integer;
  R: TRect;
  LineNumText: string;
  LineNumTextRect: TRect;
  Wdth: Integer;
label
  DrawGutter;
begin
  GutterWidth := FEditor.Gutter.Width - 5;

  // Ruler background
  if Y = 0 then
  begin
    FEditor.Canvas.Brush.Color := Lighten(clBtnFace, 6);
    R := Rect(24, 0, GutterWidth, FEditor.Height);
    FEditor.Canvas.FillRect(R);
  end;

  // Ruler cosmetics..
  FEditor.Canvas.Brush.Style := bsClear;
  FEditor.Canvas.Font.Color := clGray;
  FEditor.Canvas.Pen.Color := clGray;

  if ALine = FCurrentLine then
  begin
    if GetBreakpointStatus(ALine) <> bpsNone then
      ImgIndex := CImageIndexCurrentLineBreakpoint
    else
      if FForm.Debugger.State = dsDebugSuspended then
        ImgIndex := CImageIndexForwardArrow
      else
        ImgIndex := CImageIndexExecutableLine
  end
  else
    case GetBreakpointStatus(ALine) of
      bpsBreakpoint :
        if IsExecutableLine(ALine) then
          ImgIndex := CImageIndexBreakpoint
         else
          ImgIndex := CImageIndexBreakpointDisabled;
      bpsBreakpointDisabled :
        ImgIndex := CImageIndexBreakpointDisabled;
     else
       if IsExecutableLine(ALine) then
         ImgIndex := CImageIndexExecutableLine
        else
         ImgIndex := -1;
    end;

  if ImgIndex >= 0 then
    FForm.SmallImages.Draw(FEditor.Canvas, X, Y, ImgIndex);

  case GetLineChangeState(aLine - 1) of
    csModified: FEditor.Canvas.Brush.Color := clYellow;
    csSaved: FEditor.Canvas.Brush.Color := clLime;
    csOriginal: goto DrawGutter;
  end;

  R := Rect(GutterWidth - 3, y, GutterWidth, y + FEditor.LineHeight);
  FEditor.Canvas.FillRect(R);
  FEditor.Canvas.Brush.Style := bsClear;

DrawGutter:
  Dec(GutterWidth, 4);
  if (ALine = 1) or (aLine = FEditor.CaretY) or (ALine mod 10 = 0) then
  begin
    LineNumText := IntToStr(aLine);
    LineNumTextRect := Rect(x, y, GutterWidth, y + FEditor.LineHeight);
    FEditor.Canvas.TextRect(LineNumTextRect, LineNumText, [tfVerticalCenter,
      tfSingleLine, tfRight]);
  end
  else
  begin
    FEditor.Canvas.Pen.Color := FEditor.Gutter.Font.Color;
    if (aLine mod 5) = 0 then
      Wdth := 5
    else
      Wdth := 2;
    Inc(y, FEditor.LineHeight div 2);
    FEditor.Canvas.MoveTo(GutterWidth - Wdth, y);
    FEditor.Canvas.LineTo(GutterWidth, y);
  end;
end;

// SynEditorClick
//
procedure TEditorPage.SynEditorClick(Sender: TObject);
var
  ScriptProgram: IdwsProgram;
  CursorPos: TBufferCoord;
  Symbol: TSymbol;
  SymbolPosList: TSymbolPositionList;
begin
  TSynEdit(Sender).InvalidateGutter;

  if not (ssCTRL in KeyboardStateToShiftState) then
    Exit;

  ScriptProgram := FForm.GetCompiledScript;
  if not Assigned(ScriptProgram) then
    Exit;

  if not FEditor.GetPositionOfMouse(CursorPos) then
    Exit;

  CursorPos := Editor.WordStartEx(CursorPos);
  Symbol := ScriptProgram.SymbolDictionary.FindSymbolAtPosition(
    CursorPos.Char, CursorPos.Line, UnitName);

  if not Assigned(Symbol) then
    Exit;

  SymbolPosList := ScriptProgram.SymbolDictionary.FindSymbolPosList(Symbol);

  if not Assigned(SymbolPosList) or (SymbolPosList.Count = 0) then
    Exit;

  FForm.OpenEditorPage(SymbolPosList[0].ScriptPos.SourceFile.Name);
  FForm.CurrentEditor.CaretX := SymbolPosList[0].ScriptPos.Col;
  FForm.CurrentEditor.CaretY := SymbolPosList[0].ScriptPos.Line;
end;

// SynEditorKeyDown
//
procedure TEditorPage.SynEditorKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
  inherited;

  SetCurrentLine(-1);
end;

// SynEditorMouseMove
//
procedure TEditorPage.SynEditorMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  MouseCoord: TDisplayCoord;
  UnderLine: Integer;
begin
  if FUnderLine > 0 then
    FEditor.InvalidateLine(FUnderLine);

  if ssCtrl in Shift then
  begin
    MouseCoord := FEditor.PixelsToRowColumn(X, Y);
    UnderLine := FEditor.DisplayToBufferPos(MouseCoord).Line;
    if UnderLine <> FUnderLine then
    begin
      FUnderLine := UnderLine;
      FEditor.InvalidateLine(FUnderLine);
    end;
  end
  else
    FUnderLine := -1;
end;

// SynEditorCommandProcessed
//
procedure TEditorPage.SynEditorCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  case Command of
    ecGotoXY:
      GotoLineNumber;
    ecOpenFileUnderCursor:
      OpenFileUnderCursor;
    ecToggleDeclImpl:
      ToggleDeclImpl;
  end;
end;

// SynEditorGutterClick
//
procedure TEditorPage.SynEditorGutterClick(Sender: TObject;
  Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
var
  iLine: Integer;
begin
  iLine := Editor.RowToLine(Line);
  if iLine < FExecutableLines.Size then
  begin
    if GetBreakpointStatus(Line) <> bpsNone then
      ClearBreakpoint(iLine)
    else
      AddBreakpoint(iLine, True);
    Editor.Repaint;
  end;
end;

// SaveToFile
//
procedure TEditorPage.SaveToFile(APromptOverwrite: Boolean);
begin
  if not FileExists(FileName) or
    not APromptOverwrite or ConfirmDlgYesNoAbort(
      Format(RStrFileAlreadyExistsOverwrite, [FileName])) then
  begin
    SaveTextToUTF8File(FileName, Editor.Lines.Text);
    ToggleLineChangedStates;
    Editor.Modified := False;
  end;
end;

// SaveIfModified
//
procedure TEditorPage.SaveIfModified(APromptOverwrite: Boolean);
begin
  if Editor.Modified then
    if not APromptOverwrite or (IsProjectSourceFile and not FileExists(FileName)) or
     ConfirmDlgYesNoAbort(
      Format(RStrFileHasChanged,  [ ExtractFileName(FileName) ])) then
        SaveToFile(False);
  Editor.Modified := False;
end;

// SaveAs
//
procedure TEditorPage.SaveAs;
begin
  FForm.SaveSourceDialog.FileName := ExtractFileName(FileName);
  if FForm.SaveSourceDialog.Execute then
  begin
    Filename := FForm.SaveSourceDialog.FileName;
    SavetoFile(False);
    ToggleLineChangedStates;
  end;
end;

// TabRight
//
function TEditorPage.TabRight: Integer;
begin
  Result := FTabLeft + FTabWidth;
end;

// CloseButtonRect
//
function TEditorPage.CloseButtonRect: TRect;
begin
  Result.Right := FTabLeft + FTabWidth - CMargin - CSlantMargin;
  Result.Left := Result.Right - CCloseButtonSize;
  Result.Top := 1 + (FForm.ImageTabs.Height - CCloseButtonSize) div 2;
  Result.Bottom := Result.Top + CCloseButtonSize;
end;

// ToggleDeclImpl
//
procedure TEditorPage.ToggleDeclImpl;
var
  ScriptProgram: IdwsProgram;
  ScriptPos: TScriptPos;
  Symbol: TSymbol;
  SymDict: TdwsSymbolDictionary;
  Context: TdwsSourceContext;
  SymbolPositionList: TSymbolPositionList;
  SymbolPosition: TSymbolPosition;
begin
  ScriptProgram := FForm.GetCompiledScript;
  if not Assigned(ScriptProgram) then
    Exit;

  SymDict := ScriptProgram.SymbolDictionary;
  Assert(Assigned(SymDict));

  Context := ScriptProgram.SourceContextMap.FindContext(FEditor.CaretX,
    FEditor.CaretY, UnitName);

  repeat
    Symbol := Context.ParentSym;
    if Symbol is TFuncSymbol then
    begin
      // retrieve symbol position list
      SymbolPositionList := SymDict.FindSymbolPosList(Symbol);
      if SymbolPositionList <> nil then
      begin
        // get declaration position
        SymbolPosition := SymbolPositionList.FindUsage(suDeclaration);
        if Assigned(SymbolPosition) then
        begin
          ScriptPos := SymbolPosition.ScriptPos;

          // check if current position is declaration
          if Context.IsPositionInContext(ScriptPos) then
            ScriptPos := TFuncSymbol(Symbol).SourcePosition;

          if (ScriptPos.Line > 0) and (ScriptPos.Col > 0) then
          begin
            FEditor.CaretXY := BufferCoord(ScriptPos.Col, ScriptPos.Line);
            Exit;
          end;
        end;
      end;
    end;

    Context := Context.Parent;
  until Context = nil;
end;

// GotoLineNumber
//
procedure TEditorPage.GotoLineNumber;
begin
  with FForm.GotoForm do
    case ShowModal of
      mrOk:
        FEditor.GotoLineAndCenter(LineNumber);
    end;
end;

// OpenFileUnderCursor
//
procedure TEditorPage.OpenFileUnderCursor;
var
  ScriptProgram: IdwsProgram;
  Symbol: TSymbol;
  WordStart: TBufferCoord;
begin
  ScriptProgram := FForm.GetCompiledScript;
  if not Assigned(ScriptProgram) then
    Exit;

  WordStart := FEditor.WordStart;
  Symbol := ScriptProgram.SymbolDictionary.FindSymbolAtPosition(
    WordStart.Char, WordStart.Line, UnitName);

  if (Symbol is TUnitMainSymbol) then
  begin
    FForm.OpenEditorPage(TUnitMainSymbol(Symbol).Name);
    // eventually move caret position here...
  end;
end;





{ TDwsIdeForm }

constructor TDwsIdeForm.Create(AOwner: TComponent; const AOptions: TDwsIdeOptions);
begin
  inherited Create(AOwner);

  FOptions := AOptions;
end;

procedure TDwsIdeForm.AfterConstruction;
var
  sProjectFileName: TFileName;
  S: string;
begin
  inherited;

  // Set up callback links
  DwsIdeLocalVariablesFrame.DwsIde := Self;
  DwsIdeWatchesFrame.DwsIde := Self;
  DwsIdeCallStackFrame.DwsIde := Self;

  ClearOutputWindow;

  // Set the script folder
  if FOptions.ScriptFolder <> '' then // we have a supplied script folder..
  begin
    ScriptFolder := IncludeTrailingBackslash(FOptions.ScriptFolder);
    if not DirectoryExists(ScriptFolder) then
      raise Exception.CreateFmt(RStrScriptFolderNotFound, [ScriptFolder]);
  end
  else
  begin
    // Use the default folder - the desktop for demo
    ScriptFolder := IncludeTrailingBackslash(GetDesktopPath) + 'DWS Script Files';
    if not DirectoryExists(ScriptFolder) then
      raise Exception.Create(RStrIdeDesktopCopy);
  end;

  // Get the previously saved project settings from registry...
  MakeSettingsRec;
  LoadSettings(sProjectFileName, FIDESettingsRec);

  // Try to get a project name from the supplied project name (which might be a *.dws or *.dwsproj)
  // if there is one, this becomes our project file name.
  if FOptions.ProjectName <> '' then
    sProjectFileName := ScriptFolder + ChangeFileExt(FOptions.ProjectName, sDwsIdeProjectFileExt {eg '.dwsproj' });

  if FileExists(sProjectFileName) then
    LoadProjectFile(sProjectFileName) // << load the dwsproj if possible
  else
  begin
    S := ScriptFolder + ChangeFileExt(FOptions.ProjectName, sDwsIdeProjectSourceFileExt {eg '.dws' }); // try loading the main dws file...
    if FileExists(S) then
    begin
      // Here we've got a dws (main) file, so load it and make a project file from it too..
      FProjectFileName := ChangeFileExt(S, sDwsIdeProjectFileExt);
      EditorPageAddNew(S, True);
      SaveProjectFileAs(FProjectFileName);
    end
    else
       sProjectFileName := ScriptFolder + '\ExampleScript' + sDwsIdeProjectFileExt; {eg '.dwsproj'};

    if FileExists(sProjectFileName) then // we've got the example files, so load them...
      LoadProjectFile(sProjectFileName)
    else
      ActionFileNewProjectExecute(nil); // could not find anything, make a new blank project
  end;
end;

procedure TDwsIdeForm.BeforeDestruction;
begin
  MakeSettingsRec;
  SaveSettings(ProjectFileName, FIDESettingsRec);

  inherited;
end;

procedure TDwsIdeForm.FormCreate(Sender: TObject);
var
   bmp: TBitmap;
begin
  Debugger.OnDebugMessage := DoDebugMessage;

  bmp := TBitmap.Create;
  try
    bmp.Height := ImageTabs.Height;
    bmp.Width := ImageTabs.Width;
    ImageTabs.Picture.Bitmap := bmp;
  finally
    bmp.Free;
  end;

  FPages := TSimpleList<TEditorPage>.Create;
  FActivePageIndex := -1;
  FHoveredPageIndex := -1;
end;

procedure TDwsIdeForm.FormDestroy(Sender: TObject);
var
   i: Integer;
begin
  ClearMessagesWindow;
  Debugger.Breakpoints.Clean;
  Debugger.Watches.Clean;
  FProgram := nil;
  for i := 0 to FPages.Count - 1 do
    FPages[i].Free;
  FPages.Free;

  if Assigned(FGotoForm) then
    FGotoForm.Free;
end;

procedure TDwsIdeForm.FormShow(Sender: TObject);

  procedure EnsureVisible;
  var
    I: Integer;
    Ri: TRect;
  const
    Margin = 100;  // Number of pixels to be seen, at least
  begin
    I := 0;
    while I < Screen.MonitorCount do
    begin
      // Compute the intersection between screen and form
      Windows.IntersectRect(Ri, BoundsRect, Screen.Monitors[I].BoundsRect);

      // Check the intersection is large enough
      if (Ri.Right - Ri.Left > Margin) and (Ri.Bottom - Ri.Top > Margin) then
        Break;
      Inc(I);
    end;

    if I >= Screen.MonitorCount then
    begin
      // Form is outside of any monitor.
      // Move to center of main monitor
      Top := (Screen.Height - Height) div 2;
      Left := (Screen.Width  - Width)  div 2;
    end;
  end;

var
  I: Integer;
begin
  BoundsRect := FIDESettingsRec.FormRect;
  EnsureVisible;

  I := Max(FIDESettingsRec.RightPanelWidth, 30);
  I := Min(I, Width - 30);
  PanelRight.Width := I;

  I := Max(FIDESettingsRec.BottomPanelHeight, 30);
  I := Min(I, Height - 30);
  PanelBottom.Height := I;

  if CanGotoHomePosition then
    GotoHomePosition;
end;

procedure TDwsIdeForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Debugger.State = dsDebugSuspended then
    if ConfirmDlg(RStrAbandonDebugging) then
      ResetProgram
    else
      Exit;

  if ProjectFilename = '' then
    Exit;

  if FileExists(ProjectFilename) then
    SaveProjectFileAs(ProjectFileName)
  else
    if not SaveProjectAs then
      Abort;

  EditorCloseAllPages;
end;

procedure TDwsIdeForm.AddStatusMessage(const AStr: string);
begin
  StatusBar.Panels[3].Text := AStr;
end;

procedure TDwsIdeForm.AddMessage(const AMessage: string; AScriptPos: PScriptPos = nil);
begin
  ListboxMessages.Items.AddObject(AMessage, TObject(AScriptPos));
  PageControlBottomWindows.ActivePage := TabSheetMessages;
end;

procedure TDwsIdeForm.ClearMessagesWindow;
begin
  ListboxMessages.Clear;
end;

procedure TDwsIdeForm.ClearOutputWindow;
begin
  MemoOutputWindow.Clear;
end;

procedure TDwsIdeForm.Compile(ABuild: Boolean; const AScript: string = '');

  procedure AddMessageInfo;
  var
    I: Integer;
    sPos: PScriptPos;
  begin
    for I := 0 to FProgram.Msgs.Count - 1 do
    begin
      if FProgram.Msgs[I] is TScriptMessage then
         sPos := @TScriptMessage(FProgram.Msgs[I]).ScriptPos
       else
         sPos := nil;
      AddMessage('  ' + FProgram.Msgs[I].AsInfo, sPos);
    end;
  end;

var
  sScript: string;
begin
  if ABuild or not IsCompiled then
  begin
    ClearMessagesWindow;
    AddMessage(RStrCompileStarted);

    FScript.Config.CompilerOptions :=
      FScript.Config.CompilerOptions + [coSymbolDictionary];

    if AScript <> '' then
      sScript := AScript
    else
      sScript := ProjectSourceScript;

    if sScript = '' then
      sScript := UnitMainScript(CurrentEditorPage.UnitName, ''); // make a main file that simply uses the current page

    FProgram := FScript.Compile(sScript);

    if FProgram.Msgs.HasErrors then // did not compile - errors
    begin
      AddStatusMessage('Error(s)');
      AddMessageInfo;
      AddMessage('Compile complete - error(s)');
      GotoScriptPos(FProgram.Msgs.LastMessagePos, AScript <> '');
      ErrorDlg(FProgram.Msgs[ FProgram.Msgs.Count - 1 ].AsInfo);
    end
    else
      AddStatusMessage('Compiled');
      if FProgram.Msgs.Count = 0 then // perfect compile!
        AddMessage('Compile complete - success')
      else // hints or warnings...
      begin
        AddMessageInfo;
        AddMessage(RStrCompileCompleteWarnHints);
      end;
  end;
end;

function TDwsIdeForm.NameToEditorPageIndex(const AName: string): Integer;
begin
  for Result := 0 to EditorPageCount - 1 do
    if SameText(EditorPage(Result).UnitName, AName) then
      Exit;
  Result := -1;
end;

procedure TDwsIdeForm.EditorCloseAllPages(AExceptIndex: Integer = -1);
var
  I: Integer;
begin
  for I := EditorPageCount - 1 downto 0 do
    if I <> AExceptIndex then
      EditorPageClose(I);
end;

procedure TDwsIdeForm.EditorSaveAllIfModified(APromptOverwrite: Boolean);
var
  I: Integer;
begin
  for I := 0 to EditorPageCount - 1 do
    EditorPage(I).SaveIfModified(APromptOverwrite);
end;

function TDwsIdeForm.SaveProjectAs: Boolean;
var
  sFilename: TFileName;
begin
  SaveProjectDialog.FileName := ExtractFileName(FProjectFileName);
  Result := SaveProjectDialog.Execute;
  if Result then
  begin
    sFilename := SaveProjectDialog.FileName;
    ProjectSourceFileName := ProjectfileNameToProjectSourceFileName(SaveProjectDialog.FileName);
    SaveProjectFileAs(sFileName);
  end;
end;

procedure TDwsIdeForm.MakeSettingsRec;
begin
  FIDESettingsRec.FormRect := BoundsRect;
  FIDESettingsRec.RightPanelWidth := PanelRight.Width;
  FIDESettingsRec.BottomPanelHeight := PanelBottom.Height;
end;

procedure TDwsIdeForm.ResetProgram;
begin
  Debugger.EndDebug;
end;

function TDwsIdeForm.UnitMainScript(const AUnitName, AIdentifier: string): string;
const
  sScriptTemplate =
      'uses %s;'#13#10
    + 'begin'#13#10
    + '%s'#13#10
    + 'end;'#13#10;
begin
  Result := Format(sScriptTemplate, [AUnitName, AIdentifier]);
end;

procedure TDwsIdeForm.DoDebugMessage(const msg: string);
begin
  MemoOutputWindow.Lines.Add('ODS: ' + msg);
  PageControlBottomWindows.ActivePage := TabSheetOutput;
end;

procedure TDwsIdeForm.RunFunctionMethodByName(const AUnit, AName: string; AWithDebugging, APrompt: Boolean);
var
  Exec: IdwsProgramExecution;
  Stopwatch: TStopwatch;
  sScript: string;
begin
  if APrompt and not ConfirmDlg(Format(RStrRunFunctionMethod, [AName])) then
    Exit;

  EditorSaveAllIfModified(False);

  sScript := UnitMainScript(AUnit, AName);
  Compile(True, sScript);
  if not IsCompiled then
    Exit;

  AddStatusMessage(RStrRunning);
  Application.ProcessMessages;
  ShowExecutableLines;

  Exec := FProgram.CreateNewExecution;
  Stopwatch := TStopwatch.Create;
  Stopwatch.Start;
  try
    if AWithDebugging then
      Debugger.BeginDebug(Exec)
    else
    begin
      Exec.BeginProgram;
      Exec.RunProgram(0);
    end;
  finally
    Stopwatch.Stop;
    if AWithDebugging then
      Debugger.EndDebug
    else
      Exec.EndProgram;

    ClearExecutableLines;
    if Exec.Msgs.Count > 0 then
    begin
      AddStatusMessage('Errors');
      GotoScriptPos(Exec.Msgs.LastMessagePos, True);
      ErrorDlg(Exec.Msgs.AsInfo);
    end
    else
      if Stopwatch.Elapsed.TotalSeconds < 1.0 then
        AddStatusMessage(Format('Completed in %0.3f ms', [Stopwatch.Elapsed.TotalMilliseconds]))
      else
        AddStatusMessage(Format('Completed in %0.3f s', [Stopwatch.Elapsed.TotalSeconds]));
  end;
end;

// RefreshTabs
//
procedure TDwsIdeForm.RefreshTabs;

  function ColorLerp(col1, col2: TColor; f: Single): TColor;
  var
    invF: Single;
  begin
    f := EnsureRange(f, 0, 1);
    invF := 1 - f;

    col1 := ColorToRGB(col1);
    col2 := ColorToRGB(col2);

    Result := RGB(Trunc(GetRValue(col1) * invF + GetRValue(col2) * f),
                  Trunc(GetGValue(col1) * invF + GetGValue(col2) * f),
                  Trunc(GetBValue(col1) * invF + GetBValue(col2) * f));
  end;

  procedure GradVertical(Canvas: TCanvas; Rect: TRect; FromColor, ToColor: TColor);
  var
    mx, Y: Integer;
    invHeight: Single;
    cnt: Integer;
  begin
    Canvas.Brush.Style := bsSolid;

    if Rect.Bottom>Rect.Top then
      invHeight := 1 / (Rect.Bottom - Rect.Top)
    else
      invHeight := 1;

    mx := Rect.Right-cSlantMargin;
    cnt := 0;
    for Y := Rect.Top to Rect.Bottom - 1 do
    begin
      Canvas.Brush.Color := ColorLerp(FromColor, ToColor, cnt*invHeight);

      Rect.Right := mx + cnt;
      Rect.Top := Y;
      Rect.Bottom := Y + 1;

      Canvas.FillRect(Rect);

      Inc(cnt) ;
    end;
  end;

  procedure RenderTab(canvas: TCanvas; page: TEditorPage;
    active, hovered, hoveredClose: Boolean);

    function GetImageIndexFromContent: Integer;
    var
      sExt: string;
    begin
      sExt := ExtractFileExt(Page.FileName);

      if Page.IsProjectSourcefile then
        Result := CImageIndex_ProjectSourceFile
      else
        if SameText(sExt, sDwsIdeProjectSourceFileExt) {eg DWS}
          or SameText(sExt, sDwsIdeProjectSourceFileExt2) {eg PAS} then
          Result := CImageIndex_Script
        else
          if SameText(sExt, '.INC') then
            Result := CImageIndex_IncludeFile
          else
            Result := CImageIndex_NonScript;
    end;

  var
    tabRect: TRect;
    r: TRect;
    txt: string;
    closeBtnDrawDetails: TThemedElementDetails;
  begin
    tabRect := Rect(page.FTabLeft, canvas.ClipRect.Top,
      page.FTabLeft + page.FTabWidth, canvas.ClipRect.Bottom);
    if    (tabRect.Right<canvas.ClipRect.Left)
      or (tabRect.Left>canvas.ClipRect.Right) then Exit;

    if active then
      if hovered then
        GradVertical(canvas, tabRect, clWindow, clWindow)
      else
        GradVertical(canvas, tabRect, clWindow, clWindow)
    else
    begin
      if hovered then
        GradVertical(canvas, tabRect, clWindow, clBtnFace)
      else
        GradVertical(canvas, tabRect, clWindow, ColorLerp(clBtnFace, clBtnShadow, 0.5));
    end;

    canvas.Brush.Style := bsClear;

    SmallImages.Draw(canvas, tabRect.Left + CMargin, (tabRect.Bottom -
      tabRect.Top - SmallImages.Height) div 2, GetImageIndexFromContent, True);

    txt := page.Caption;
    r := tabRect;
    r.Left := r.Left + CMargin + SmallImages.Width + CMargin;
    Canvas.TextRect(r, txt, [tfLeft, tfVerticalCenter, tfSingleLine, tfNoPrefix, tfEndEllipsis]);

    if not UseThemes then
    begin
      Windows.DrawFrameControl(canvas.Handle, page.CloseButtonRect,
        DFC_CAPTION, DFCS_CAPTIONCLOSE + DFCS_FLAT);
    end
    else
    begin
      if hovered and hoveredClose then
        closeBtnDrawDetails := IDEStyleServices.GetElementDetails(twSmallCloseButtonHot)
      else
        closeBtnDrawDetails := IDEStyleServices.GetElementDetails(twSmallCloseButtonDisabled);

      IDEStyleServices.DrawElement(canvas.Handle, closeBtnDrawDetails, page.CloseButtonRect);
    end;

    Canvas.Pen.Color := clBtnShadow;
    if active then
    begin
      Canvas.MoveTo(0, tabRect.Bottom - 1);
      Canvas.LineTo(tabRect.Left, tabRect.Bottom - 1);
    end
    else
      Canvas.MoveTo(tabRect.Left, tabRect.Bottom);
    Canvas.LineTo(tabRect.Left, tabRect.Top);
    Canvas.LineTo(tabRect.Right-cSlantMargin, tabRect.Top);
    Canvas.LineTo(tabRect.Right-cSlantMargin + tabRect.Bottom-tabRect.Top, tabRect.Bottom);
    if active then
    begin
      Canvas.MoveTo(tabRect.Right - CSlantMargin + tabRect.Bottom - tabRect.Top, tabRect.Bottom - 1);
      Canvas.LineTo(canvas.ClipRect.Right, tabRect.Bottom - 1);
    end;
  end;

var
  bmp: TBitmap;
  canvas: TCanvas;
  i, x: Integer;
  availableWidth: Integer;
  page: TEditorPage;
begin
  bmp := ImageTabs.Picture.Bitmap;
  canvas := bmp.Canvas;
  canvas.Brush.Style := bsSolid;
  canvas.Font := Self.Font;

  canvas.Brush.Color := clBtnFace;
  canvas.FillRect(canvas.ClipRect);

  // check if pages are available
  if FPages.Count = 0 then
    Exit;

  if FActivePageIndex < 0 then
    FActivePageIndex := 0;

  // compute tab width
  for i := 0 to FPages.Count - 1 do
  begin
    page := FPages[i];
    page.FTabWidth :=  CMargin + SmallImages.Width
      + CMargin + canvas.TextWidth(page.Caption)
      + CMargin + CCloseButtonSize + 2*CMargin + CSlantMargin;
  end;

  if FBasePageIndex>=FPages.Count then
    FBasePageIndex := FPages.Count - 1;

  availableWidth := bmp.Width - 2 * CArrowButtonSize;

  while True do
  begin
    x := 0;
    // compute tab positions
    for i := FBasePageIndex to FPages.Count - 1 do
    begin
      page := FPages[i];
      page.FTabLeft := x;
      x := x + page.FTabWidth;
    end;

    x := 0;
    for i := FBasePageIndex - 1 downto 0 do
    begin
      page := FPages[i];
      x := x-page.FTabWidth;
      page.FTabLeft := x;
    end;
    if    (FBasePageIndex=FActivePageIndex)
      or (FPages[FBasePageIndex].TabRight<=availableWidth) then Break;
    Inc(FBasePageIndex);
  end;

  // render tabs (right to left for slant overlap)
  for i := FPages.Count - 1 downto 0 do
  begin
    if i = FActivePageIndex then
      Continue;
    page := FPages[i];
    RenderTab(canvas, page, False, i=FHoveredPageIndex, FHoveredCloseButton);
  end;
  if FActivePageIndex >= 0 then
  begin
    page := FPages[FActivePageIndex];
    RenderTab(canvas, page, True, FActivePageIndex=FHoveredPageIndex, FHoveredCloseButton);
  end;

  RefreshTabArrows;
end;

// RefreshTabArrows
//
procedure TDwsIdeForm.RefreshTabArrows;
var
  dc: THandle;
  sbElement: TThemedScrollBar;
begin
  if FPages.Count <= 1 then Exit;

  FLeftArrowActive := (FBasePageIndex > 0);
  FRightArrowActive :=    (FBasePageIndex < FPages.Count - 1)
    and (FPages[FPages.Count - 1].TabRight > ImageTabs.Width - 2 * CArrowButtonSize);

  if not (FLeftArrowActive or FRightArrowActive) then
    Exit;

  dc := ImageTabs.Picture.Bitmap.Canvas.Handle;

  FTabArrowLeft.Top := (ImageTabs.Height - cArrowButtonSize) div 2;
  FTabArrowLeft.Bottom := FTabArrowLeft.Top + CArrowButtonSize;
  FTabArrowLeft.Left := ImageTabs.Width - 2 * CArrowButtonSize;
  FTabArrowLeft.Right := ImageTabs.Width - cArrowButtonSize;
  FTabArrowRight := FTabArrowLeft;
  OffsetRect(FTabArrowRight, CArrowButtonSize, 0);

  if not UseThemes then
  begin
    Windows.DrawFrameControl(dc, FTabArrowLeft, DFC_SCROLL,
      DFCS_SCROLLLEFT + DFCS_FLAT
      + Ord(not FLeftArrowActive)*DFCS_INACTIVE
      + Ord(FHoveredLeftArrow)*DFCS_HOT);
    Windows.DrawFrameControl(dc, FTabArrowRight, DFC_SCROLL,
      DFCS_SCROLLRIGHT + DFCS_FLAT
      + Ord(not FRightArrowActive)*DFCS_INACTIVE
      + Ord(FHoveredRightArrow)*DFCS_HOT);
  end
  else
  begin
    if FLeftArrowActive then
      if FHoveredLeftArrow then
        sbElement := tsArrowBtnLeftHot
      else
        sbElement := tsArrowBtnLeftNormal
    else
      sbElement := tsArrowBtnLeftDisabled;

    IDEStyleServices.DrawElement(dc, IDEStyleServices.GetElementDetails(sbElement), FTabArrowLeft);
    if FRightArrowActive then
      if FHoveredRightArrow then
        sbElement := tsArrowBtnRightHot
      else
        sbElement := tsArrowBtnRightNormal
    else
      sbElement := tsArrowBtnRightDisabled;
    IDEStyleServices.DrawElement(dc, IDEStyleServices.GetElementDetails(sbElement), FTabArrowRight);
  end;

  ImageTabs.Invalidate;
end;

// IndexOfTab
//
function TDwsIdeForm.IndexOfTab(x: Integer): Integer;
var
  i: Integer;
  page: TEditorPage;
begin
  if x < ImageTabs.Width - CArrowButtonSize * 2 then
    for i := FBasePageIndex to FPages.Count - 1 do
    begin
      page := FPages[i];
      if (x>=page.FTabLeft) and (x<page.FTabLeft + page.FTabWidth) then
        Exit(i);
    end;

  Result := -1;
end;


function TDwsIdeForm.TryRunSelection(ADebug: Boolean): Boolean;
var
  S: string;
begin
  Result := CurrentEditor.SelAvail;
  if Result then
  begin
    S := CurrentEditor.SelText;
    if IsValidIdentifier(S) then
      RunFunctionMethodByName(CurrentEditorPage.UnitName, S, ADebug, True {prompt});
  end;
end;

procedure TDwsIdeForm.EditorPageAddNew(const AFileName: TFileName; ALoadFile: Boolean);
begin
  FPages.Add(TEditorPage.Create(Self, AFileName, ALoadFile));
  SetEditorCurrentPageIndex(FPages.Count - 1);
end;

procedure TDwsIdeForm.ListSymbols;
begin
  ListSymbolTable(FProgram.Table);
end;

procedure TDwsIdeForm.ClearCurrentLine;
var
  I: Integer;
begin
  for I := 0 to EditorPageCount - 1 do
    EditorPage(I).SetCurrentLine(-1);
end;

procedure TDwsIdeForm.ClearAllBreakpoints;
var
  I: Integer;
begin
  Debugger.Breakpoints.Clean;
  for I := 0 to EditorPageCount - 1 do
    EditorPage(I).Editor.Invalidate;
end;

function TDwsIdeForm.CurrentEditor: TSynEdit;
begin
  if HasEditorPage then
    Result := EditorPage(EditorCurrentPageIndex).Editor
  else
     Result := nil;
end;

function TDwsIdeForm.CurrentEditorPage: TEditorPage;
begin
  Result := EditorPage(FActivePageIndex);
end;

function TDwsIdeForm.ProjectSourceScript: string;
var
  sFileName: TFileName;
begin
  EditorSaveAllIfModified(False);
  sFileName := ProjectSourceFileName;
  if FileExists(sFileName) then
    Result := LoadTextFromFile(sFileName)
  else
     Result := '';
end;

procedure TDwsIdeForm.DoOnClickEditorPageTabContextMenuPageItem(
  ASender: TObject);
begin
  with ASender as TMenuItem do
    EditorCurrentPageIndex := Tag;
end;

procedure TDwsIdeForm.DebuggerDebug(exec: TdwsExecution;
  expr: TExprBase);
begin
  OutputDebugString('dwsDebugger: Debug');
end;

procedure TDwsIdeForm.DebuggerDebugStart(exec: TdwsExecution);
begin
  OutputDebugString('dwsDebugger: Start');
end;

procedure TDwsIdeForm.DebuggerDebugStop(exec: TdwsExecution);
begin
  OutputDebugString('dwsDebugger: Paused');
end;

procedure TDwsIdeForm.DebuggerEnterFunc(exec: TdwsExecution;
  expr: TExprBase);
//var
//  R: TdwsExprLocationArray;
//  I: Integer;
begin
//  R := Exec.GetCallStack;
//  I := Length(R);
//  if I > 0 then
//    ShowMessage(R[0].Location);

  OutputDebugString('dwsDebugger: EnterFunc');
end;

procedure TDwsIdeForm.DebuggerLeaveFunc(exec: TdwsExecution;
  expr: TExprBase);
begin
  OutputDebugString('dwsDebugger: LeaveFunc');
end;

procedure TDwsIdeForm.DebuggerStateChanged(Sender: TObject);

  procedure UpdateDebugWindows;
  begin
    DwsIdeLocalVariablesFrame.Redraw;
    DwsIdeCallStackFrame.Redraw;
    DwsIdeWatchesFrame.Redraw;
  end;

begin
  case Debugger.State of
    dsIdle:;
    dsDebugRun :
      begin
        ClearCurrentLine;
        AddStatusMessage(RStrRunning);
      end;
    dsDebugSuspending :;
    dsDebugSuspended :
      begin
        AddStatusMessage(RStrPaused);
        GotoScriptPos(Debugger.CurrentScriptPos);
        UpdateDebugWindows;
      end;
    dsDebugResuming :;
    dsDebugDone :
      begin
        ClearCurrentLine;
        UpdateDebugWindows;
      end
  end;
end;


procedure TDwsIdeForm.GotoHomePosition;
begin
  FHomePositionCaptionSuffix := '';
  if FOptions.HomePositionFileName <> '' then
    if OpenEditorPage(FOptions.HomePositionFileName) then
    begin
      FHomePositionCaptionSuffix := FOptions.HomePositionFileName;
      if FOptions.HomePositionFileIdentifier <> '' then
      begin
        CurrentEditorPage.GotoIdentifier(FOptions.HomePositionFileIdentifier);
        FHomePositionCaptionSuffix := Format('%s%s [%s]', [
          FOptions.HomePositionFileName,
          sDwsIdeProjectSourceFileExt2,
          FOptions.HomePositionFileIdentifier]);
      end;
    end;
end;

function TDwsIdeForm.CanGotoHomePosition: Boolean;
begin
  Result := FOptions.HomePositionFileName <> '';
end;

procedure TDwsIdeForm.GotoScriptPos(AScriptPos: TScriptPos; AHiddenMainModule: Boolean = False);
var
  S: string;
  I: Integer;
begin
  if AScriptPos.SourceFile <> nil then
  begin
    S := AScriptPos.SourceFile.Name;
    if S = SYS_MainModule then
    begin
      if AHiddenMainModule then
        I := -1
      else
        I := ProjectSourceFileIndex
    end
    else
      I := NameToEditorPageIndex(S);
    if I <> -1 then
    begin
      EditorCurrentPageIndex := I;
      CurrentEditorPage.SetCurrentLine(AScriptPos.Line, AScriptPos.Col);
      CurrentEditor.SetFocus;
    end;
  end;
end;

function TDwsIdeForm.DwsIde_GetDebugger: TdwsDebugger;
begin
  Result := Debugger;
end;

procedure TDwsIdeForm.EditorChange(Sender: TObject);
begin
  if IsCompiled then
  begin
    ClearExecutableLines;
    ClearMessagesWindow;
    FProgram := nil;
  end;
  //FScript.NotifyScriptModified;
end;

function TDwsIdeForm.FileIsOpenInEditor(const AFileName: TFileName): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to EditorPageCount - 1 do
    if SameText(AFileName, EditorPage(I).FileName) then
      Exit;
  Result := False;
end;

function TDwsIdeForm.FileIsProjectSource(const AFileName: TFileName): Boolean;
begin
  Result := SameText(ExtractFileExt(AFileName), sDwsIdeProjectSourceFileExt);
end;

function TDwsIdeForm.GetProjectSourceFileName: TFileName;
var
  I: Integer;
begin
  I := ProjectSourceFileIndex;
  if I >= 0 then
    Result := EditorPage(I).FileName
  else
    Result := '';
end;

function TDwsIdeForm.HasEditorPage: Boolean;
begin
  Result := EditorCurrentPageIndex <> -1;
end;

function TDwsIdeForm.HasProject: Boolean;
begin
  Result := (ProjectFileName <> '') and ((ProjectSourceFileIndex <> -1) or FileExists(ProjectSourceFileName));
end;

procedure TDwsIdeForm.ImageTabsMouseLeave(Sender: TObject);
begin
  if FHoveredPageIndex >= 0 then
  begin
    FHoveredPageIndex := -1;
    RefreshTabs;
  end;
end;

procedure TDwsIdeForm.ImageTabsMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  newHover: Integer;
  newHoverCloseButton, needRefresh, needRefreshArrows: Boolean;
  newHoverArrow: Boolean;
begin
  needRefresh := False;
  newHover := IndexOfTab(x);
  if newHover <> FHoveredPageIndex then
  begin
    FHoveredPageIndex := newHover;
    needRefresh := True;
    Application.CancelHint;
    if newHover >= 0 then
      ImageTabs.Hint := FPages[newHover].Hint
    else
      ImageTabs.Hint := '';
  end;

  if newHover >= 0 then
  begin
    newHoverCloseButton := PtInRect(FPages[newHover].CloseButtonRect, Point(X, Y));
    if newHoverCloseButton<>FHoveredCloseButton then
    begin
      FHoveredCloseButton := newHoverCloseButton;
      needRefresh := True;
    end;
  end;

  needRefreshArrows := False;
  newHoverArrow := FLeftArrowActive and PtInRect(FTabArrowLeft, Point(X, Y));
  if newHoverArrow<>FHoveredLeftArrow then
  begin
    FHoveredLeftArrow := newHoverArrow;
    needRefreshArrows := True;
  end;

  newHoverArrow := FRightArrowActive and PtInRect(FTabArrowRight, Point(X, Y));
  if newHoverArrow<>FHoveredRightArrow then
  begin
    FHoveredRightArrow := newHoverArrow;
    needRefreshArrows := True;
  end;

  if needRefresh then
    RefreshTabs
  else
  if needRefreshArrows then
    RefreshTabArrows;
end;

function TDwsIdeForm.IsCompiled: Boolean;
begin
  Result := Assigned(FProgram) and not FProgram.Msgs.HasErrors;
end;

procedure TDwsIdeForm.ListBoxMessagesDblClick(Sender: TObject);
var
  I: Integer;
  sPos: PScriptPos;
begin
  I := ListBoxMessages.ItemIndex;
  if I < 0 then
    Exit;
  sPos := PScriptPos(ListBoxMessages.Items.Objects[I]);
  if Assigned(FProgram) and Assigned(sPos) then
    GotoScriptPos(sPos^);
end;

procedure TDwsIdeForm.pcEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure AddPageList;
  var
    PageItem: TMenuItem;
    Page: TEditorPage;
    I: Integer;
    S: string;
  begin
    MenuItemPageTabPages.Clear;
    for I := 0 to EditorPageCount - 1 do
    begin
      PageItem := TMenuItem.Create(EditorPageTabContextMenu);
      MenuItemPageTabPages.Add(PageItem);
      Page := EditorPage(I);
      S := Format('%s (%s)', [Page.Caption, Page.FileName]);
      PageItem.Caption := S;
      PageItem.OnClick := DoOnClickEditorPageTabContextMenuPageItem;
      PageItem.Tag    := I;
      PageItem.Checked := I = EditorCurrentPageIndex;
    end;
  end;

var
  P: TPoint;
  tabIndex: Integer;
begin
  FpcEditorLastMouseButton := Button;
  FpcEditorLastMouseXY := Point(X, Y);

  tabIndex := IndexOfTab(X);
  if (tabIndex>=0) then
  begin
    SetEditorCurrentPageIndex(tabIndex);
    if PtInRect(FPages[tabIndex].CloseButtonRect, FpcEditorLastMouseXY) then
    begin
      EditorPageClose(tabIndex);
      Exit;
    end;
  end;

  if FpcEditorLastMouseButton = mbRight then
  begin
    AddPageList;
    P := ImageTabs.ClientToScreen(FpcEditorLastMouseXY);
    EditorPageTabContextMenu.Popup(P.X, P.Y);
  end;

  if FHoveredLeftArrow then
  begin
    Dec(FBasePageIndex);
    RefreshTabs;
  end;
  if FHoveredRightArrow then
  begin
    Inc(FBasePageIndex);
    RefreshTabs;
  end;
end;

procedure TDwsIdeForm.pnlPageControlResize(Sender: TObject);
begin
  ImageTabs.Picture.Bitmap.Width := ImageTabs.Width;
  RefreshTabs;
end;

function TDwsIdeForm.ProjectSourceFileIndex: Integer;
var
  S: string;
begin
  S := ExtractFileName(ProjectfileNameToProjectSourceFileName(ProjectFileName));
  for Result := 0 to EditorPageCount - 1 do
    if SameText(S, ExtractFileName(EditorPage(Result).FileName)) then
      Exit;
  Result := -1;
end;

procedure TDwsIdeForm.SetScript(const Value: TDelphiWebScript);
begin
  FScript := Value;

  if not Assigned(FScript) then
    raise EDwsIde.Create(RStrScriptCannotBeNil);

  if FScript.Config.ScriptPaths.Count = 0 then
    raise EDwsIde.Create(RStrScriptDoesNotDefineMainPath);

  if FScript.Config.ScriptPaths.IndexOf(FScriptFolder) = -1 then
    FScript.Config.ScriptPaths.Add(FScriptFolder);

  // Script result type has been saved before calling the IDE form
  // so we can load an 'output window' connection here..
  FScript.Config.ResultType := TOutputWindowStringResultType.Create(FScript, Self);
end;

procedure TDwsIdeForm.SetScriptFolder(const Value: string);
begin
  FScriptFolder := Value;

  OpenFileDialog.DefaultFolder := FScriptFolder;
  OpenProjectDialog.DefaultFolder := FScriptFolder;
  SaveProjectDialog.DefaultFolder := FScriptFolder;
end;

procedure TDwsIdeForm.SetEditorCurrentPageIndex(const Value: Integer);
var
  page: TEditorPage;
begin
  if Value = FActivePageIndex then
    Exit;

  if Value > FPages.Count - 1 then
    Exit;

  LockWindowUpdate(pnlPageControl.Handle);
  try
    if FActivePageIndex>=0 then
    begin
      page := EditorPage(Value);
      page.Visible := False;
      page.Parent := nil;

      // Disconnect search items
      page.Editor.SearchEngine := nil;
      if ActionSearchFind.Dialog <> nil then
        ActionSearchFind.Dialog.CloseDialog;
      if ActionSearchReplace.Dialog <> nil then
        ActionSearchReplace.Dialog.CloseDialog;
    end;

    if (Value >= 0) and (Value < FPages.Count) then
    begin
      FActivePageIndex := Value;
      RefreshTabs;
      page := EditorPage(Value);
      page.Align := alClient;
      page.Parent := pnlPageControl;
      page.Visible := True;
      page.Editor.Repaint;

      // Connect the search engine
      page.Editor.SearchEngine := SynEditSearch;

      // Focus the new editor now ...
      ActiveControl := page.Editor;
    end;
  finally
    LockWindowUpdate(0);
  end;
end;

procedure TDwsIdeForm.SetProjectFileName(const Value: TFileName);
begin
  if FileExists(Value) then
  begin
    FProjectFileName := Value;
    LoadProjectFile(FProjectFileName);
  end
  else
    FProjectFileName := '';
end;

procedure TDwsIdeForm.UpdateTimerTimer(Sender: TObject);

  procedure UpdateFormCaption;
  begin
    if FHomePositionCaptionSuffix <> '' then
      Caption := Format('Home: %s', [FHomePositionCaptionSuffix])
    else
    if ProjectfileName = '' then
      Caption := '[No project]'
    else
      Caption := Format('%s  (%s)', [ExtractFileName(ProjectFilename), ExtractfileDir(ProjectFilename)]);
  end;

  procedure UpdateStatusBarPanels;
  resourcestring
    SModified  = 'Modified';
    SInsert    = 'Insert';
    SOverwrite = 'Overwrite';
    SReadOnly  = 'Read Only';
  const
    MacroRecorderStates: array[ TSynMacroState ] of string = (
      'Stopped', 'Recording', 'Playing', 'Paused');
  var
    ptCaret: TPoint;
    Editor: TSynEdit;
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
        begin
          if SynMacroRecorder.State <> msStopped then
            StatusBar.Panels[2].Text := UpperCase(MacroRecorderStates[SynMacroRecorder.State])
          else
            StatusBar.Panels[2].Text := SInsert
        end
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

procedure TDwsIdeForm.SetProjectSourceFileName(const Value: TFileName);
var
  I: Integer;
begin
  I := ProjectSourceFileIndex;
  if I >= 0 then
  begin
    EditorPage(I).FileName := Value;
    FPages[i].Caption := ChangeFileExt(ExtractFileName(Value), '') + ' *' ;
  end;
end;

procedure TDwsIdeForm.ShowExecutableLines;
var
  I: Integer;
begin
  for I := 0 to EditorPageCount - 1 do
    EditorPage(I).ShowExecutableLines;
end;

procedure TDwsIdeForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  SuggestionIndex: Integer;
  Proposal: TSynCompletionProposal;
  SourceFile: TSourceFile;
  ScriptPos: TScriptPos;
  ScriptProgram: IdwsProgram;
  Suggestions: IdwsSuggestions;
  Item, AddOn: string;
begin
  CanExecute := False;

  Assert(Sender is TSynCompletionProposal);

  // check the proposal type
  Proposal := TSynCompletionProposal(Sender);
  Proposal.InsertList.Clear;
  Proposal.ItemList.Clear;

  if Assigned(Proposal.Form) then
  begin
    Proposal.Form.DoubleBuffered := True;
    Proposal.Resizeable := True;
    Proposal.Form.Resizeable := True;
    Proposal.Form.BorderStyle := bsSizeToolWin;
  end;

  // use this handler only in case the kind is set to ctCode!
  Assert(Kind = ctCode);

  // get script program
  ScriptProgram := GetCompiledScript;
  if ScriptProgram = nil then
    Exit;

  // ok, get the compiled "program" from DWS
  SourceFile := ScriptProgram.SourceList.MainScript.SourceFile;
  ScriptPos := TScriptPos.Create(SourceFile, CurrentEditor.CaretY, CurrentEditor.CaretX);
  Suggestions := TDWSSuggestions.Create(ScriptProgram, ScriptPos,
    [soNoReservedWords]);

  // now populate the suggestion box
  for SuggestionIndex := 0 to Suggestions.Count - 1 do
  begin
    // discard empty suggestions
    if Suggestions.Caption[SuggestionIndex] = '' then
      Continue;

    with CurrentEditor.Highlighter.KeywordAttribute do
      Item := '\color{' + ColorToString(Foreground) + '}';

    case Suggestions.Category[SuggestionIndex] of
      scUnit:
        Item := Item + 'unit';
      scType:
        Item := Item + 'type';
      scClass:
        Item := Item + 'class';
      scRecord:
        Item := Item + 'record';
      scInterface:
        Item := Item + 'interface';
      scFunction:
        Item := Item + 'function';
      scProcedure:
        Item := Item + 'procedure';
      scMethod:
        Item := Item + 'method';
      scConstructor:
        Item := Item + 'constructor';
      scDestructor:
        Item := Item + 'destructor';
      scProperty:
        Item := Item + 'property';
      scEnum:
        Item := Item + 'enum';
      scElement:
        Item := Item + 'element';
      scParameter:
        Item := Item + 'param';
      scVariable:
        Item := Item + 'var';
      scConst:
        Item := Item + 'const';
      scReservedWord:
        Item := Item + 'reserved';
    end;

    Item := Item + ' \column{}';
    with CurrentEditor.Highlighter.IdentifierAttribute do
      Item := Item + '\color{' + ColorToString(Foreground) + '}';
    Item := Item + Suggestions.Code[SuggestionIndex];
    AddOn := Suggestions.Caption[SuggestionIndex];
    Delete(AddOn, 1, Length(Suggestions.Code[SuggestionIndex]));
    Item := Item + '\style{-B}' + AddOn;
    Proposal.ItemList.Add(Item);
    Proposal.InsertList.Add(Suggestions.Code[SuggestionIndex]);
  end;

  CanExecute := True;
end;

procedure TDwsIdeForm.SynCodeCompletionShow(Sender: TObject);
var
  CompletionProposalForm: TSynBaseCompletionProposalForm;
begin
  inherited;

  if (Sender <> nil) and (Sender is TSynBaseCompletionProposalForm) then
  begin
    CompletionProposalForm := TSynBaseCompletionProposalForm(Sender);
    try
      CompletionProposalForm.DoubleBuffered := True;

      if CompletionProposalForm.Height > 300 then
        CompletionProposalForm.Height := 300
    except
      on Exception do;
    end;
  end;
end;

procedure TDwsIdeForm.SynParametersExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);

  procedure GetParameterInfosForCursor(const AProgram: IdwsProgram; Col,
    Line: Integer; var ParameterInfos: TStrings; InfoPosition: Integer = 0);

    procedure ParamsToInfo(const AParams: TParamsSymbolTable);
    var
      y: Integer;
      ParamsStr: string;
    begin
      ParamsStr := '';
      if (AParams <> nil) and (AParams.Count > 0) then
      begin
        if InfoPosition >= AParams.Count then
          Exit;

        ParamsStr := '"' + AParams[0].Description + ';"';
        for y := 1 to AParams.Count - 1 do
          ParamsStr := ParamsStr + ',"' + AParams[y].Description + ';"';
      end else
      if InfoPosition > 0 then
        Exit;

      if (ParameterInfos.IndexOf(ParamsStr) < 0) then
        ParameterInfos.Add(ParamsStr);
    end;

  var
    Overloads: TFuncSymbolList;

    procedure CollectMethodOverloads(methSym: TMethodSymbol);
    var
      Member: TSymbol;
      Struct: TCompositeTypeSymbol;
      LastOverloaded: TMethodSymbol;
    begin
      LastOverloaded := methSym;
      Struct := methSym.StructSymbol;
      repeat
        for Member in Struct.Members do
        begin
          if not UnicodeSameText(Member.Name, methSym.Name) then
            Continue;
          if not (Member is TMethodSymbol) then
            Continue;

          LastOverloaded := TMethodSymbol(Member);
          if not Overloads.ContainsChildMethodOf(LastOverloaded) then
            Overloads.Add(LastOverloaded);
        end;

        Struct := Struct.Parent;
      until (Struct = nil) or not LastOverloaded.IsOverloaded;
    end;

  var
    ItemIndex: Integer;
    FuncSymbol: TFuncSymbol;

    SymbolDictionary: TdwsSymbolDictionary;
    Symbol, TestSymbol: TSymbol;
  begin
    // make sure the string list is present
    Assert(Assigned(ParameterInfos));

    // ensure a compiled program is assigned
    if not Assigned(AProgram) then
      Exit;

    SymbolDictionary := AProgram.SymbolDictionary;
    Symbol := SymbolDictionary.FindSymbolAtPosition(Col, Line, MSG_MainModule);

    if (Symbol is TSourceMethodSymbol) then
    begin
      Overloads := TFuncSymbolList.Create;
      try
        CollectMethodOverloads(TSourceMethodSymbol(Symbol));
        for ItemIndex := 0 to Overloads.Count - 1 do
        begin
          FuncSymbol := Overloads[ItemIndex];
          ParamsToInfo(FuncSymbol.Params);
        end;
      finally
        Overloads.Free;
      end;
    end else
    if (Symbol is TFuncSymbol) then
    begin
      ParamsToInfo(TFuncSymbol(Symbol).Params);

      if TFuncSymbol(Symbol).IsOverloaded then
      begin
        for ItemIndex := 0 to SymbolDictionary.Count - 1 do
        begin
          TestSymbol := SymbolDictionary.Items[ItemIndex].Symbol;

          if (TestSymbol.ClassType = Symbol.ClassType) and
            SameText(TFuncSymbol(TestSymbol).Name, TFuncSymbol(Symbol).Name) and
            (TestSymbol <> Symbol) then
            ParamsToInfo(TFuncSymbol(TestSymbol).Params);
        end;
      end
    end;

    // check if no parameters at all is an option, if so: replace and move to top
    ItemIndex := ParameterInfos.IndexOf('');
    if ItemIndex >= 0 then
    begin
      ParameterInfos.Delete(ItemIndex);
      ParameterInfos.Insert(0, RStrNoParametersRequired);
    end;
  end;


var
  LineText: string;
  Proposal: TSynCompletionProposal;
  LocLine: string;
  TmpX: Integer;
  TmpLocation, StartX, ParenCounter: Integer;
  ParameterInfoList: TStrings;
  ScriptProgram: IdwsProgram;
begin
  CanExecute := False;
  Assert(Kind = ctParams);

  // get script program
  ScriptProgram := GetCompiledScript;
  if ScriptProgram = nil then
    Exit;

  // check the proposal type
  if Sender is TSynCompletionProposal then
  begin
    Proposal := TSynCompletionProposal(Sender);
    Proposal.InsertList.Clear;
    Proposal.ItemList.Clear;
    ParameterInfoList := TStrings(Proposal.ItemList);

    // get current line
    LineText := CurrentEditor.LineText;

    with CurrentEditor do
    begin
      // get current compiled program
      if not Assigned(ScriptProgram) then
        Exit;

      LocLine := LineText;

      //go back from the cursor and find the first open paren
      TmpX := CaretX;
      if TmpX > Length(LocLine) then
        TmpX := Length(LocLine)
      else Dec(TmpX);
      TmpLocation := 0;

      while (TmpX > 0) and not CanExecute do
      begin
        if LocLine[TmpX] = ',' then
        begin
          Inc(TmpLocation);
          Dec(TmpX);
        end else if LocLine[TmpX] = ')' then
        begin
          // we found a close, go till it's opening paren
          ParenCounter := 1;
          Dec(TmpX);
          while (TmpX > 0) and (ParenCounter > 0) do
          begin
            if LocLine[TmpX] = ')' then
              Inc(ParenCounter)
            else
            if LocLine[TmpX] = '(' then
              Dec(ParenCounter);
            Dec(TmpX);
          end;
          if TmpX > 0 then Dec(TmpX);  // eat the open paren
        end else if LocLine[TmpX] = '(' then
        begin
          // we have a valid open paren, lets see what the word before it is
          StartX := TmpX;
          while (TmpX > 0) and not IsIdentChar(LocLine[TmpX])do
            Dec(TmpX);
          if TmpX > 0 then
          begin
            while (TmpX > 0) and IsIdentChar(LocLine[TmpX]) do
              Dec(TmpX);
            Inc(TmpX);

            GetParameterInfosForCursor(ScriptProgram, TmpX,
              CurrentEditor.CaretY, ParameterInfoList, TmpLocation);

            CanExecute := ParameterInfoList.Count > 0;

            if not CanExecute then
            begin
              TmpX := StartX;
              Dec(TmpX);
            end
            else
              TSynCompletionProposal(Sender).Form.CurrentIndex := TmpLocation;
          end;
        end else Dec(TmpX)
      end;
    end;
  end;
end;

procedure TDwsIdeForm.ListSymbolTable(ATable: TSymbolTable);
var
  SL: TStringList;
  S: string;
begin
  SL := TStringList.Create;
  try
    SymbolsToStrings(ATable, SL);

    S := SL.Text;
    ShowMessage(S);
  finally
    SL.Free
  end;
end;

procedure TDwsIdeForm.ClearExecutableLines;
var
  I: Integer;
begin
  for I := 0 to EditorPageCount - 1 do
    EditorPage(I).ClearExecutableLines;
end;

procedure TDwsIdeForm.EditorPageClose(AIndex: Integer);
var
  Page: TEditorPage;
begin
  if AIndex = -1 then
    Exit;

  Page := EditorPage(AIndex);

  Page.SaveIfModified(True);

  if EditorCurrentPageIndex>0 then
     EditorCurrentPageIndex := EditorCurrentPageIndex - 1
  else
    if FPages.Count > 0 then
      EditorCurrentPageIndex := 0;

  FPages[AIndex].Free;
  FPages.Extract(AIndex);

  if FPages.Count = 0 then
    FActivePageIndex := -1;

  RefreshTabs;
end;

function TDwsIdeForm.EditorPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TDwsIdeForm.EditorPage(AIndex: Integer): TEditorPage;
begin
  Result := FPages[AIndex];
end;

procedure TDwsIdeForm.SaveProjectFileAs(const AProjectFileName: TFileName);

  procedure SaveBreakpoints(AData: IXMLProjectConfigType);
  var
    I: Integer;
    Breakpoint: IXMLBreakpointType;
  begin
    AData.Breakpoints.Clear;
    for I := 0 to Debugger.Breakpoints.Count - 1 do
    begin
      Breakpoint := AData.Breakpoints.Add;
      Breakpoint.SourceName := Debugger.Breakpoints[I].SourceName;
      Breakpoint.LineNum := Debugger.Breakpoints[I].Line;
      Breakpoint.Enabled := Debugger.Breakpoints[I].Enabled;
    end;
  end;

  procedure SaveWatches(AData: IXMLProjectConfigType);
  var
    I: Integer;
    Watch: IXMLWatchType;
  begin
    AData.Watches.Clear;
    for I := 0 to Debugger.Watches.Count - 1 do
    begin
      Watch := AData.Watches.Add;
      Watch.Expression := Debugger.Watches[I].ExpressionText;
    end;
  end;

var
  I: Integer;
  XMLPage: IXMLEditorPageType;
  XMLDocument: IXMLDocument;
  Data: IXMLProjectConfigType;
  Page: TEditorPage;
  S: string;
begin
  XMLDocument := NewXMLDocument;
  Data := XMLDocument.GetDocBinding('ProjectConfig', TXMLProjectConfigType) as IXMLProjectConfigType;

  // Make an empty project source file if required
  S := ProjectfileNameToProjectSourceFileName(AProjectFilename);
  if not FileExists(S) then
    SaveTextToUTF8File(S, '');

  for I := 0 to EditorPageCount - 1 do
  begin
    Page := EditorPage(I);

    Page.SaveIfModified(True);

    XMLPage := Data.EditorPages.EditorPage.Add;

    XMLPage.FileName := ExtractRelativePath(
      IncludeTrailingBackslash(FScriptFolder),
      Page.FileName);
    XMLPage.Name := JustFileName(Page.FileName);
  end;

  Data.EditorPages.ActivePageIndex := EditorCurrentPageIndex;

  SaveBreakpoints(Data);
  SaveWatches(Data);

  XMLDocument.XML.Text := FormatXMLData(XMLDocument.XML.Text);
  XMLDocument.Active := True;

  XMLDocument.SaveToFile(AProjectFileName);

  FProjectFileName := AProjectFileName;
end;

procedure TDwsIdeForm.LoadProjectFile(const AProjectFileName: TFileName);

  procedure LoadBreakpoints(AData: IXMLProjectConfigType);
  var
    I, iEditorPage: Integer;
    Breakpoint: IXMLBreakpointType;
  begin
    Debugger.Breakpoints.Clean;
    for I := 0 to AData.Breakpoints.Count - 1 do
    begin
      Breakpoint := AData.Breakpoints[I];

      iEditorPage := NameToEditorPageIndex(Breakpoint.SourceName);
      if iEditorPage <> -1 then
        if (BreakPoint.LineNum >= 1) then
          if BreakPoint.LineNum <= EditorPage(iEditorPage).FEditor.Lines.Count then
          begin
            Debugger.Breakpoints.Add(Breakpoint.LineNum, Breakpoint.SourceName);
            Debugger.Breakpoints[I].Enabled := Breakpoint.Enabled;
          end;
    end;
  end;

  procedure LoadWatches(AData: IXMLProjectConfigType);
  var
    I: Integer;
    Watch: IXMLWatchType;
  begin
    Debugger.Watches.Clean;
    for I := 0 to AData.Watches.Count - 1 do
    begin
      Watch := AData.Watches[I];
      Debugger.Watches.Add(Watch.Expression);
    end;

    DwsIdeWatchesFrame.Redraw;
  end;

var
  I: Integer;
  sFileName: TFileName;
  XMLDocument: IXMLDocument;
  Data: IXMLProjectConfigType;
begin
  if not FileExists(AProjectFileName) then
    raise EDwsIde.CreateFmt(RStrProjectFileDoesNotExist, [AProjectFileName]);

  EditorCloseAllPages;

  ClearOutputWindow;
  ClearMessagesWindow;

  FProjectFileName := AProjectFileName;

  FProgram := nil;

  XMLDocument := LoadXMLDocument(AProjectFileName);
  Data := XMLDocument.GetDocBinding('ProjectConfig', TXMLProjectConfigType) as IXMLProjectConfigType;
  for I := 0 to Data.EditorPages.EditorPage.Count - 1 do
  begin
    sFileName := IncludeTrailingBackslash(FScriptFolder)
      + Data.EditorPages.EditorPage[I].FileName;
    if FileExists(sFileName) and not FileIsOpenInEditor(sFileName) then
      EditorPageAddNew(sFileName, True);
  end;
  EditorCurrentPageIndex := Data.EditorPages.ActivePageIndex;

  LoadBreakpoints(Data);
  LoadWatches(Data);

  ClearExecutableLines;
end;

procedure TDwsIdeForm.NewProjectFile(const AProjectFileName: TFileName);
begin
  EditorCloseAllPages;

  ClearOutputWindow;
  ClearMessagesWindow;

  FProjectFileName := AProjectFileName;
end;

function TDwsIdeForm.OpenEditorPage(AName: string): Boolean;
var
  I: Integer;
  S: string;
begin
  I := NameToEditorPageIndex(AName);
  Result := I <> -1;
  if Result then
    EditorCurrentPageIndex := I
  else
  begin
    S := ScriptFolder + ChangeFileExt(AName, sDwsIdeProjectSourceFileExt2);
    Result := FileExists(S);
    if Result then
      EditorPageAddNew(S, True);
  end;
end;

function TDwsIdeForm.ProjectfileNameToProjectSourceFileName(const AProjectfileName: TFileName): string;
begin
  if AProjectFileName = '' then
    Result := ''
  else
    Result := ChangeFileExt(AProjectFileName, sDwsIdeProjectSourceFileExt);
end;

function TDwsIdeForm.ModifyFileNameToUniqueInProject(
           const AFileName: TFileName): string;
// if exists, increments any trailing number after the file name
// until the name is unique in the name's folder. if no trailing number
// adds '1'.
var
  I: Integer;
  sD, sF, sE: string;
begin
  Result := AFileName;
  I := 0;
  while FileIsOpenInEditor(Result) do
  begin
    Inc(I);
    sD := ExtractFileDir(Result) + '\';
    sF := JustFileName(Result);
    while (sF <> '') and CharInSet(sF[Length(sF) - 1], ['0'..'9']) do
      Delete(sF, Length(sF) - 1, 1);

    sE := ExtractFileExt(Result);
    Result := sD + sF + IntToStr(I) + sE;
  end;
end;

function TDwsIdeForm.GetCompiledScript: IdwsProgram;
var
  Code: string;
begin
  Result := nil;

  if not HasEditorPage then
    Exit;

  try
    if HasProject then
      Code := ProjectSourceScript // Use the main file, eg dws
    else
      Code := UnitMainScript(CurrentEditorPage.UnitName, ''); // make a main file that simply uses the current page

    Result := FScript.Compile(Code);
  except
    Result := nil;
  end;
end;

function TDwsIdeForm.GetExecutableLines(const AUnitName: string): TLineNumbers;
// Returns the executable line numbers for this unit.

  procedure AppendLineNum(ALineNum: Integer);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := ALineNum;
  end;

var
  I: Integer;
  Breakpointables: TdwsBreakpointableLines;
  Lines: Tbits;
begin
  SetLength(Result, 0);

  Compile(False);
  if not IsCompiled then
    Exit;

  Breakpointables := TdwsBreakpointableLines.Create(FProgram);
  try
    Lines := Breakpointables.SourceLines[ AUnitName ];
    if Lines <> nil then
    begin
      for I := 1 to Lines.Size - 1 do
        if Lines[I] then
          AppendLineNum(I);
    end;
  finally
    Breakpointables.Free;
  end;
end;

function TDwsIdeForm.GetGotoForm: TDwsIdeGotoLineNumber;
begin
  if not Assigned(FGotoForm) then
    FGotoForm := TDwsIdeGotoLineNumber.Create(Self);

  Result := FGotoForm;
end;

{$REGION 'Load/Save Settings'}
procedure TDwsIdeForm.SaveSettings(
      const AProjectFileName: TFileName;
      const AIDESettingsRec: TIDESettingsRec);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey('SOFTWARE\DwsIde\', TRUE) then
    begin
      Reg.WriteString('WorkingProjectFileName', AProjectFileName);

      if not IsHostedControl(Self) then
      begin
        Reg.WriteInteger('IDEFormRect_Left',   AIDESettingsRec.FormRect.Left);
        Reg.WriteInteger('IDEFormRect_Top',    AIDESettingsRec.FormRect.Top);
        Reg.WriteInteger('IDEFormRect_Right',  AIDESettingsRec.FormRect.Right);
        Reg.WriteInteger('IDEFormRect_Bottom', AIDESettingsRec.FormRect.Bottom);
      end;

      Reg.WriteInteger('RightPanelWidth',      AIDESettingsRec.RightPanelWidth);
      Reg.WriteInteger('BottomPanelHeight',    AIDESettingsRec.BottomPanelHeight);

      if Assigned(FGotoForm) then
        Reg.WriteInteger('Recent Goto Position', FGotoForm.LineNumber);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TDwsIdeForm.LoadSettings(
           var AProjectFileName: TFileName;
           var AIDESettingsRec: TIDESettingsRec);
var
  Reg: TRegistry;

  procedure ReadString(const AName: string; var AValue: string);
  begin
    if Reg.ValueExists(AName) then
      AValue := Reg.ReadString(AName)
  end;

  procedure ReadInteger(const AName: string; var AValue: Integer);
  begin
    if Reg.ValueExists(AName) then
      AValue := Reg.ReadInteger(AName)
  end;

begin
  AProjectFileName := '';

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\DwsIde\', False) then
    begin
      ReadString('WorkingProjectFileName', string(AProjectFileName));

      if not IsHostedControl(Self) then
      begin
        ReadInteger('IDEFormRect_Left',   AIDESettingsRec.FormRect.Left);
        ReadInteger('IDEFormRect_Top',    AIDESettingsRec.FormRect.Top);
        ReadInteger('IDEFormRect_Right',  AIDESettingsRec.FormRect.Right);
        ReadInteger('IDEFormRect_Bottom', AIDESettingsRec.FormRect.Bottom);
      end;

      ReadInteger('RightPanelWidth',      AIDESettingsRec.RightPanelWidth);
      ReadInteger('BottomPanelHeight',    AIDESettingsRec.BottomPanelHeight);
    end;
  finally
    Reg.Free;
  end;
end;
{$ENDREGION}

{$REGION 'Action Handler'}
procedure TDwsIdeForm.ActionGotoLineNumberExecute(Sender: TObject);
begin
  CurrentEditorPage.GotoLineNumber;
end;

procedure TDwsIdeForm.ActionGotoLineNumberUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := CurrentEditor <> nil;
end;

procedure TDwsIdeForm.ActionBuildExecute(Sender: TObject);
begin
  Compile(True);
  if IsCompiled then
    ShowExecutableLines;
end;

procedure TDwsIdeForm.ActionClearAllBreakpointsExecute(Sender: TObject);
begin
  ClearAllBreakpoints;
end;

procedure TDwsIdeForm.ActionEditClearOutputWindowExecute(Sender: TObject);
begin
  ClearOutputWindow;
end;

procedure TDwsIdeForm.ActionEditClearOutputWindowUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := MemoOutputWindow.Text <> '';
end;

procedure TDwsIdeForm.ActionCloseAllOtherPagesExecute(Sender: TObject);
begin
  EditorCloseAllPages(EditorCurrentPageIndex);
end;

procedure TDwsIdeForm.ActionCloseAllOtherPagesUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := EditorPageCount > 1;
end;

procedure TDwsIdeForm.ActionClosePageExecute(Sender: TObject);
begin
  EditorPageClose(EditorCurrentPageIndex);
end;

procedure TDwsIdeForm.ActionClosePageUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := EditorCurrentPageIndex <> -1;
end;

procedure TDwsIdeForm.ActionFileCloseAllExecute(Sender: TObject);
begin
  EditorCloseAllPages;
  ProjectFileName := '';
end;

procedure TDwsIdeForm.ActionFileNewIncludeFileExecute(Sender: TObject);
var
  sFileName: TFileName;
begin
  sFileName := Format('%sIncludeFile1.inc',
    [IncludeTrailingBackslash(FScriptFolder)]);
  sFileName := ModifyFileNameToUniqueInProject(sFileName);

  EditorPageAddNew(sFileName, False);
end;

procedure TDwsIdeForm.ActionFileNewProjectExecute(Sender: TObject);
var
  sFileName: TFileName;
begin
  sFileName := Format('%sProject1%s',
    [IncludeTrailingBackslash(FScriptFolder), sDwsIdeProjectFileExt]);
  sFileName := ModifyFileNameToUniqueInProject(sFileName);

  // Allow the project files to be pre-created by the app (e.g. using template files)
  if Assigned(FOnNewProject) then
    FOnNewProject(sFileName);

  if Assigned(FOnNewProject) and FileExists(sFileName) then
    LoadProjectFile(sFileName)
  else
  begin
    NewProjectFile(sFileName);
    sFileName := ProjectfileNameToProjectSourceFileName(sFileName);
    EditorPageAddNew(sFileName, False);
  end;
end;

procedure TDwsIdeForm.ActionFileNewUnitExecute(Sender: TObject);
var
  sFileName: TFileName;
begin
  sFileName := Format('%sUnit1%s',
    [IncludeTrailingBackslash(FScriptFolder), sDwsIdeProjectSourceFileExt2]);
  sFileName := ModifyFileNameToUniqueInProject(sFileName);

  EditorPageAddNew(sFileName, False);
end;

procedure TDwsIdeForm.ActionFileSaveAsExecute(Sender: TObject);
begin
  if HasEditorPage then
    CurrentEditorPage.SaveAs;
end;

procedure TDwsIdeForm.ActionFileSaveAsUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.ActionFileSaveExecute(Sender: TObject);
begin
  if HasEditorPage then
    CurrentEditorPage.SaveToFile(False {dont prompt});
end;

procedure TDwsIdeForm.ActionFileSaveUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := HasEditorPage and CurrentEditor.Modified;
end;

procedure TDwsIdeForm.ActionGotoHomePositionExecute(Sender: TObject);
begin
  GotoHomePosition;
end;

procedure TDwsIdeForm.ActionGotoHomePositionUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := CanGotoHomePosition;
end;

procedure TDwsIdeForm.ActionOpenFileExecute(Sender: TObject);
var
  FileName: TFileName;
  I: Integer;
begin
  if not OpenFileDialog.Execute then
    Exit;

  for I := 0 to OpenFileDialog.Files.Count - 1 do
  begin
    FileName := OpenFileDialog.Files[I];
    if (FileName <> '') and not FileIsOpenInEditor(FileName) then
      EditorPageAddNew(FileName, True);
  end;
end;

procedure TDwsIdeForm.ActionFileOpenProjectExecute(Sender: TObject);
begin
  if OpenProjectDialog.Execute then
    LoadProjectFile(OpenProjectDialog.FileName);
end;

procedure TDwsIdeForm.ActionProgramResetExecute(Sender: TObject);
begin
  ResetProgram;
  //InformationDlg('Program Aborted');
end;

procedure TDwsIdeForm.ActionProgramResetUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := daCanEndDebug in Debugger.AllowedActions;
end;

procedure TDwsIdeForm.ActionRunExecute(Sender: TObject);

  procedure NewRun;
  var
    Exec: IdwsProgramExecution;
  begin
    if not HasProject then
      raise EDwsIde.Create(RStrCannotRunWithoutProjectFile);

    Compile(False);
    if not IsCompiled then
      Exit;

    ShowExecutableLines;

    Exec := FProgram.CreateNewExecution;
    try
      Debugger.BeginDebug(Exec);
    finally
      Debugger.EndDebug;
      ClearExecutableLines;
      if Exec.Msgs.Count > 0 then
      begin
        AddStatusMessage(RStrErrors);
        GotoScriptPos(Exec.Msgs.LastMessagePos);
        ErrorDlg(Exec.Msgs.AsInfo);
      end
      else
        AddStatusMessage(RStrProgramCompleted);
    end;

  end;

begin
  try
    if Debugger.State = dsDebugSuspended then
      Debugger.Resume
    else
      if not TryRunSelection(True) then
        NewRun;
  except
    on E:Exception do
      ErrorDlg(E.Message);
  end;
end;

procedure TDwsIdeForm.ActionRunUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.ActionRunWithoutDebuggingExecute(Sender: TObject);

  procedure RunAll;
  var
    Exec: IdwsProgramExecution;
    Stopwatch: TStopwatch;
  begin
    if not HasProject then
      raise EDwsIde.Create(RStrCannotRunWithoutProjectFile);

    AddStatusMessage(RStrRunning);
    Application.ProcessMessages;
    Compile(False);
    if not IsCompiled then
      Exit;

    Exec := FProgram.CreateNewExecution;
    Exec.BeginProgram;
    Stopwatch := TStopwatch.Create;
    Stopwatch.Start;
    try
      Exec.RunProgram(0);
      Exec.EndProgram;
    finally
      Stopwatch.Stop;
    end;

    if Exec.Msgs.Count > 0 then
    begin
      AddStatusMessage(RStrErrors);
      ShowMessage(Exec.Msgs.AsInfo)
    end
    else
      if Stopwatch.Elapsed.TotalSeconds < 1.0 then
        AddStatusMessage(Format('Completed in %0.3f ms', [Stopwatch.Elapsed.TotalMilliseconds]))
      else
        AddStatusMessage(Format('Completed in %0.3f s', [Stopwatch.Elapsed.TotalSeconds]));
  end;

begin
  if not TryRunSelection(False) then
    RunAll;
end;

procedure TDwsIdeForm.ActionRunWithoutDebuggingUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := HasEditorPage;
end;

procedure TDwsIdeForm.ActionFileSaveProjectAsExecute(Sender: TObject);
begin
  SaveProjectAs;
end;

procedure TDwsIdeForm.ActionShowExecutionPointExecute(Sender: TObject);
begin
  GotoScriptPos(Debugger.CurrentScriptPos);
end;

procedure TDwsIdeForm.ActionShowExecutionPointUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := Debugger.State = dsDebugSuspended;
end;

procedure TDwsIdeForm.ActionStepOverExecute(Sender: TObject);
begin
  Debugger.StepOver;
end;

procedure TDwsIdeForm.ActionStepOverUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := daCanStep in Debugger.AllowedActions;
end;

procedure TDwsIdeForm.ActionEditToggleReadOnlyExecute(Sender: TObject);
var
  Page: TEditorPage;
begin
  Page := CurrentEditorPage;
  Page.IsReadOnly := not Page.IsReadOnly;
end;

procedure TDwsIdeForm.ActionEditToggleReadOnlyUpdate(Sender: TObject);
begin
  with Sender as TAction do
  begin
    Enabled := HasEditorPage;
    Checked := Enabled and CurrentEditorPage.IsReadOnly;
  end;
end;

procedure TDwsIdeForm.ActionTraceIntoExecute(Sender: TObject);
begin
  Debugger.StepDetailed;
end;

procedure TDwsIdeForm.ActionTraceIntoUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := daCanStep in Debugger.AllowedActions;
end;

procedure TDwsIdeForm.ActionViewProjectSourceExecute(Sender: TObject);
var
  I: Integer;
begin
  I := ProjectSourceFileIndex;
  if I >= 0 then
    EditorCurrentPageIndex := I
  else
    EditorPageAddNew(ProjectfileNameToProjectSourceFileName(ProjectFileName), True);
end;

procedure TDwsIdeForm.ActionViewProjectSourceUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := ProjectFileName <> '';
end;

procedure TDwsIdeForm.ActionViewSymbolsExecute(Sender: TObject);
begin
  ListSymbols;
end;

procedure TDwsIdeForm.ActionViewSymbolsUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := Assigned(FProgram) and (FProgram.Table.Count > 0);
end;
{$ENDREGION}


{ TOutputWindowsStringResultType }

constructor TOutputWindowStringResultType.Create(AOwner: TComponent; ADwsIdeForm: TDwsIdeForm);
begin
  inherited Create(AOwner);
  FDwsIDEForm := ADwsIdeForm;
end;

procedure TOutputWindowStringResultType.DoAddString(result: TdwsStringResult;
  var str: string);
begin
  FDwsIdeForm.MemoOutputWindow.Lines.Add('STD: ' + str);
end;

procedure TOutputWindowStringResultType.DoReadChar(result: TdwsStringResult;
  var str: string);
var
  c: Char;
begin
  Read(c);
  str := c;
end;

procedure TOutputWindowStringResultType.DoReadLn(result: TdwsStringResult;
  var str: string);
begin
  ReadLn(str);
end;

end.
