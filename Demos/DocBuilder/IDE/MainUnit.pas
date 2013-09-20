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
{    Current maintainer: Christian-W. Budde                            }
{                                                                      }
{**********************************************************************}
unit MainUnit;

interface

{$IFNDEF UseDebugger}
  Error! //-> Please define 'UseDebugger' globally
{$ENDIF}

{$I dws.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, StdActns, ActnList, ExtDlgs, ComCtrls,
  Types, SyncObjs, ImgList, dwsComp, dwsExprs, dwsSymbols, dwsErrors,
  dwsSuggestions, dwsStrings, dwsUnitSymbols, dwsDocBuilder, dwsCompiler,
  dwsHtmlFilter, dwsSymbolsLibModule, dwsClassesLibModule, dwsDebugger,

  SynEdit, SynEditHighlighter, SynHighlighterDWS, SynCompletionProposal,
  SynEditMiscClasses, SynEditSearch, SynEditOptionsDialog, SynEditPlugins,
  SynMacroRecorder, SynHighlighterHtml, SynHighlighterMulti, SynEditTypes;

type
  TRescanThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TBreakpointStatus = (bpsNone, bpsBreakpoint, bpsBreakpointDisabled);
  TLineNumbers = array of Integer;

  TFrmBasic = class(TForm)
    AcnBuildBuild: TAction;
    AcnBuildOptions: TAction;
    AcnBuildReset: TAction;
    AcnBuildStart: TAction;
    AcnBuildStepOver: TAction;
    AcnBuildTraceInto: TAction;
    AcnEditCopy: TEditCopy;
    AcnEditCut: TEditCut;
    AcnEditDelete: TEditDelete;
    AcnEditPaste: TEditPaste;
    AcnEditSelectAll: TEditSelectAll;
    AcnEditUndo: TEditUndo;
    AcnFileExit: TFileExit;
    AcnFileNew: TAction;
    AcnFileOpen: TFileOpen;
    AcnFileSaveScriptAs: TFileSaveAs;
    AcnFileScriptSave: TAction;
    AcnOptions: TAction;
    AcnSearchFind: TSearchFind;
    AcnViewLocalVariables: TAction;
    AcnViewPreview: TAction;
    ActionList: TActionList;
    DelphiWebScript: TDelphiWebScript;
    dwsClassesLib: TdwsClassesLib;
    dwsDebugger: TdwsDebugger;
    dwsHtmlFilter: TdwsHtmlFilter;
    dwsSymbolsLib: TdwsSymbolsLib;
    ListBoxCompiler: TListBox;
    MainMenu: TMainMenu;
    MnuBuildBuild: TMenuItem;
    MnuBuildOptions: TMenuItem;
    MnuBuildStart: TMenuItem;
    MnuEdit: TMenuItem;
    MnuEditCopy: TMenuItem;
    MnuEditCut: TMenuItem;
    MnuEditDelete: TMenuItem;
    MnuEditPaste: TMenuItem;
    MnuEditSaveAs: TMenuItem;
    MnuEditSearch: TMenuItem;
    MnuEditUndo: TMenuItem;
    MnuFile: TMenuItem;
    MnuFileNew: TMenuItem;
    mnuLocalVariables: TMenuItem;
    MnuOptions: TMenuItem;
    MnuSaveMessagesAs: TMenuItem;
    MnuSaveOutputAs: TMenuItem;
    MnuScript: TMenuItem;
    MnuScriptExit: TMenuItem;
    MnuScriptOpen: TMenuItem;
    MnuSearch: TMenuItem;
    MnuSelectAll: TMenuItem;
    MnuView: TMenuItem;
    MnuViewPreview: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    PopupMenuMessages: TPopupMenu;
    PopupMenuOutput: TPopupMenu;
    ProposalImages: TImageList;
    SplitterHorizontal: TSplitter;
    SplitterVertical: TSplitter;
    StatusBar: TStatusBar;
    SynCompletionProposal: TSynCompletionProposal;
    SynDWSSyn: TSynDWSSyn;
    SynEdit: TSynEdit;
    SynEditOptionsDialog: TSynEditOptionsDialog;
    SynEditSearch: TSynEditSearch;
    SynHTMLSyn: TSynHTMLSyn;
    SynMacroRecorder: TSynMacroRecorder;
    SynMultiSyn: TSynMultiSyn;
    SynParameters: TSynCompletionProposal;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AcnBuildBuildExecute(Sender: TObject);
    procedure AcnBuildDebuggerUpdate(Sender: TObject);
    procedure AcnBuildOptionsExecute(Sender: TObject);
    procedure AcnBuildResetExecute(Sender: TObject);
    procedure AcnBuildResetUpdate(Sender: TObject);
    procedure AcnBuildStartExecute(Sender: TObject);
    procedure AcnBuildStepOverExecute(Sender: TObject);
    procedure AcnBuildTraceIntoExecute(Sender: TObject);
    procedure AcnFileNewExecute(Sender: TObject);
    procedure AcnFileOpenAccept(Sender: TObject);
    procedure AcnFileSaveScriptAsAccept(Sender: TObject);
    procedure AcnFileScriptSaveExecute(Sender: TObject);
    procedure AcnOptionsExecute(Sender: TObject);
    procedure AcnViewLocalVariablesExecute(Sender: TObject);
    procedure AcnViewPreviewExecute(Sender: TObject);
    procedure dwsDebuggerStateChanged(Sender: TObject);
    procedure MnuSaveMessagesAsClick(Sender: TObject);
    procedure MnuScriptExitClick(Sender: TObject);
    procedure PanelRightDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure PanelRightDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure PanelRightGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure PanelRightUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure SynCompletionProposalExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    procedure SynCompletionProposalPaintItem(Sender: TObject; Index: Integer;
      TargetCanvas: TCanvas; ItemRect: TRect; var CustomDraw: Boolean);
    procedure SynCompletionProposalShow(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditGutterClick(Sender: TObject; Button: TMouseButton; X, Y,
      Line: Integer; Mark: TSynEditMark);
    procedure SynEditGutterPaint(Sender: TObject; aLine, X, Y: Integer);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure SynParametersExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
  private
    FRecentScriptName: TFileName;
    FRescanThread: TRescanThread;
    FCompiledProgram: IdwsProgram;
    FCriticalSection: TCriticalSection;
    FSyncEvent: TEvent;
    FSymbolUnit: TSymbolUnit;
    FAbortBuild: Boolean;
    FOutputPath: string;
    FCurrentLine: Integer;
    FExecutableLines: TBits;

    procedure SourceChanged;
    procedure BeforeBuildContent(Sender: TDocumentationBuilder;
      const FileName: TFileName; const Symbol: TSymbol; var Content: string);
    procedure ClearBreakpoint(ALineNum: Integer);
    procedure ClearExecutableLines;
    procedure InitExecutableLines;
    procedure ShowExecutableLines;
    function GetBreakpointStatus(ALine: Integer): TBreakpointStatus;
    function IsExecutableLine(ALine: Integer): Boolean;
    function GetExecutableLines: TLineNumbers;
    procedure AddBreakpoint(ALineNum: Integer; AEnabled: Boolean);
    procedure SetCurrentLine(ALine: Integer; ACol: Integer = 1);
    procedure ClearCurrentLine;
  public
    procedure CompileScript;
    procedure UpdateCompilerOutput;
    procedure ShowDockPanel(MakeVisible: Boolean);

    property SyncEvent: TEvent read FSyncEvent;
  end;

var
  FrmBasic: TFrmBasic;

implementation

{$R *.dfm}

uses
  Math, Registry, DockingUtils, dwsUtils, PreviewUnit, OptionsUnit,
  LocalVariables;

{ TRescanThread }

procedure TRescanThread.Execute;
var
  SourceChanged: Boolean;
begin
  SourceChanged := True;
  repeat
    if SourceChanged and (not Terminated) then
    begin
      FrmBasic.CompileScript;
      Sleep(100);
    end;
    SourceChanged := FrmBasic.SyncEvent.WaitFor(5000) = wrSignaled;
  until Terminated;
end;


{ TEditorPageSynEditPlugin }

type
  TEditorPageSynEditPlugin = class(TSynEditPlugin)
  protected
    FEditor: TSynEdit;
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  public
    constructor Create(Editor: TSynEdit);
  end;

constructor TEditorPageSynEditPlugin.Create(Editor: TSynEdit);
begin
  inherited Create(Editor);
  FEditor := Editor;
end;

procedure TEditorPageSynEditPlugin.LinesInserted(FirstLine, Count: Integer);
var
  Index, LineCount : Integer;
begin
  // Track the executable lines
  LineCount := FEditor.Lines.Count;
  FrmBasic.FExecutableLines.Size := LineCount;
  for Index := LineCount - 1 downto FirstLine + Count do
    FrmBasic.FExecutableLines[Index] := FrmBasic.FExecutableLines[Index - Count];
  for Index := FirstLine + Count - 1 downto FirstLine do
    FrmBasic.FExecutableLines[Index] := False;

  // Track the breakpoint lines in the debugger
  with FrmBasic.dwsDebugger do
    for Index := 0 to Breakpoints.Count - 1 do
      if Breakpoints[Index].SourceName = SYS_MainModule then
        if Breakpoints[Index].Line >= FirstLine then
           Breakpoints[Index].Line := Breakpoints[Index].Line + Count;

  // Redraw the gutter for updated icons
  FEditor.InvalidateGutter;
end;

procedure TEditorPageSynEditPlugin.LinesDeleted(FirstLine, Count: Integer);
var
  Index: Integer;
begin
  // Track the executable lines
  for Index := FirstLine - 1 to FrmBasic.FExecutableLines.Size - Count - 1 do
    FrmBasic.FExecutableLines[Index] := FrmBasic.FExecutableLines[Index + Count];
  FrmBasic.FExecutableLines.Size := FrmBasic.FExecutableLines.Size - Count;

  // Track the breakpoint lines in the debugger
  with FrmBasic.dwsDebugger do
    for Index := 0 to Breakpoints.Count - 1 do
      if Breakpoints[Index].SourceName = SYS_MainModule then
        if Breakpoints[Index].Line >= FirstLine then
           Breakpoints[Index].Line := Breakpoints[Index].Line - Count;

  // Redraw the gutter for updated icons
  FEditor.InvalidateGutter;
end;


{ TFrmBasic }

procedure TFrmBasic.FormCreate(Sender: TObject);
begin
  FSymbolUnit := TSymbolUnit.Create(nil, 0);
  FSymbolUnit.Script := DelphiWebScript;
  FCriticalSection := TCriticalSection.Create;
  FSyncEvent := TEvent.Create;
  FRescanThread := TRescanThread.Create;
  FOutputPath := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\Docs\');
  FExecutableLines := TBits.Create;
  FCurrentLine := -1;
  TEditorPageSynEditPlugin.Create(SynEdit);
  SynEdit.ControlStyle := SynEdit.ControlStyle + [csOpaque];
end;

procedure TFrmBasic.FormDestroy(Sender: TObject);
begin
  if Assigned(FRescanThread) then
  begin
    FRescanThread.Terminate;
    FSyncEvent.SetEvent;
    FRescanThread.WaitFor;
    FreeAndNil(FRescanThread);
  end;

  FreeAndNil(FExecutableLines);
  FreeAndNil(FSyncEvent);

  FreeAndNil(FCriticalSection);
  FreeAndNil(FSymbolUnit);
end;

procedure TFrmBasic.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    FAbortBuild := True;
end;

procedure TFrmBasic.FormShow(Sender: TObject);
begin
  FRecentScriptName := ChangeFileExt(Application.ExeName, '.dws');
  if FileExists(FRecentScriptName) then
    SynEdit.Lines.LoadFromFile(FRecentScriptName);

  with TRegistry.Create do
  try
    if OpenKey('Software\DWS\DocBuilder\', False) then
    begin
      SynEdit.TopLine := ReadInteger('TopLine');
      SynEdit.CaretX := ReadInteger('CaretX');
      SynEdit.CaretY := ReadInteger('CaretY');
      Left := ReadInteger('Left');
      Top := ReadInteger('Top');
      Width := ReadInteger('Width');
      Height := ReadInteger('Height');
    end;
    CloseKey;
  finally
    Free;
  end;

  {$IFDEF UseDebugger}
  FrmLocalVariables.ManualDock(PanelRight);
  FrmLocalVariables.Show;
  {$ENDIF}

  InitExecutableLines;
  SourceChanged;
end;

procedure TFrmBasic.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SynEdit.Lines.SaveToFile(FRecentScriptName);

  with TRegistry.Create do
  try
    OpenKey('Software\DWS\DocBuilder\', True);
    WriteInteger('TopLine', SynEdit.TopLine);
    WriteInteger('CaretX', SynEdit.CaretX);
    WriteInteger('CaretY', SynEdit.CaretY);
    WriteInteger('Left', Left);
    WriteInteger('Top', Top);
    WriteInteger('Width', Width);
    WriteInteger('Height', Height);
    CloseKey;
  finally
    Free;
  end;
end;

procedure TFrmBasic.ShowDockPanel(MakeVisible: Boolean);
begin
  //Don't try to hide a panel which has visible dock clients.
  if not MakeVisible and (PanelRight.VisibleDockClientCount > 1) then
    Exit;

  SplitterHorizontal.Visible := MakeVisible;
  if MakeVisible then
  begin
    PanelRight.Width := 204;
    SplitterHorizontal.Left := PanelRight.Width + SplitterHorizontal.Width;
  end
  else
    PanelRight.Width := 0;

  if MakeVisible and (FrmLocalVariables <> nil) then
    FrmLocalVariables.Show;
end;

procedure TFrmBasic.CompileScript;
begin
  FSyncEvent.ResetEvent;

  FCriticalSection.Enter;
  try
    FCompiledProgram := DelphiWebScript.Compile(SynEdit.Lines.Text);
  finally
    FCriticalSection.Leave;
  end;

  FRescanThread.Synchronize(UpdateCompilerOutput);
end;

procedure TFrmBasic.dwsDebuggerStateChanged(Sender: TObject);
begin
  case dwsDebugger.State of
    dsDebugRun, dsDebugDone:
      ClearCurrentLine;
    dsDebugSuspended :
      begin
        SetCurrentLine(dwsDebugger.CurrentScriptPos.Line,
          dwsDebugger.CurrentScriptPos.Col);
        FrmLocalVariables.Redraw;
      end;
  end;
end;

procedure TFrmBasic.UpdateCompilerOutput;
begin
  if not Assigned(FCompiledProgram) then
    Exit;

  ShowExecutableLines;
  StatusBar.SimpleText := 'Compiled';
  ListBoxCompiler.Items.Text := FCompiledProgram.Msgs.AsInfo;
end;

procedure TFrmBasic.ClearExecutableLines;
var
  I : Integer;
begin
  for I := 0 to FExecutableLines.Size - 1 do
    FExecutableLines[I] := False;
  SynEdit.InvalidateGutter;
end;

procedure TFrmBasic.ClearBreakpoint(ALineNum: Integer);
var
  Test, Found : TdwsDebuggerBreakpoint;
  I : Integer;
begin
  if dwsDebugger.Breakpoints.Count = 0 then
    Exit;

  Test := TdwsDebuggerBreakpoint.Create;
  try
    Test.Line := ALineNum;
    Test.SourceName := SYS_MainModule;

    I := dwsDebugger.Breakpoints.IndexOf(Test);
    if I <> -1 then
    begin
      Found := dwsDebugger.Breakpoints[I];
      dwsDebugger.Breakpoints.Extract(Found);
      FreeAndNil(Found);
    end;
  finally
    FreeAndNil(Test);
  end;

  SynEdit.InvalidateGutterLine(ALineNum);
  SynEdit.InvalidateLine(ALineNum);
end;

procedure TFrmBasic.SetCurrentLine(ALine: integer; ACol: Integer = 1);
begin
  if FCurrentLine <> ALine then
  begin
    SynEdit.InvalidateGutterLine(FCurrentLine);
    SynEdit.InvalidateLine(FCurrentLine);
    FCurrentLine := ALine;
    if (FCurrentLine > 0) and (SynEdit.CaretY <> FCurrentLine) then
      SynEdit.CaretXY := BufferCoord(ACol, FCurrentLine);
    SynEdit.InvalidateGutterLine(FCurrentLine);
    SynEdit.InvalidateLine(FCurrentLine);
  end;
end;

procedure TFrmBasic.ClearCurrentLine;
begin
  SetCurrentLine(-1);
end;

procedure TFrmBasic.InitExecutableLines;
begin
  FrmBasic.FExecutableLines.Size := SynEdit.Lines.Count;
end;

function TFrmBasic.GetExecutableLines: TLineNumbers;
// Returns the executable line numbers for this unit.

  procedure AppendLineNum(ALineNum: Integer);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := ALineNum;
  end;

var
  I : Integer;
  Breakpointables: TdwsBreakpointableLines;
  Lines : Tbits;
begin
  SetLength(Result, 0);
  if (not Assigned(FCompiledProgram)) or FCompiledProgram.Msgs.HasErrors then
    Exit;

  Breakpointables := TdwsBreakpointableLines.Create(FCompiledProgram);
  try
    Lines := Breakpointables.SourceLines[SYS_MainModule];
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

procedure TFrmBasic.ShowExecutableLines;
var
  LineNumbers: TLineNumbers;
  I: Integer;
begin
  ClearExecutableLines;
  LineNumbers := GetExecutableLines;
  for I := 0 to Min(FExecutableLines.Size - 1, High(LineNumbers)) do
    FExecutableLines[LineNumbers[I]] := True;
  SynEdit.InvalidateGutter;
end;

function TFrmBasic.IsExecutableLine(ALine: Integer): Boolean;
begin
  if ALine < FExecutableLines.Size then
    Result := FExecutableLines[ALine]
  else
    Result := False;
end;

procedure TFrmBasic.MnuSaveMessagesAsClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    Filter := 'Text (*.txt)|*.txt';
    if Execute then
      ListBoxCompiler.Items.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TFrmBasic.MnuScriptExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmBasic.PanelRightDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
begin
  if (Sender as TPanel).DockClientCount = 1 then
    ShowDockPanel(True);
  (Sender as TPanel).DockManager.ResetBounds(True);
end;

procedure TFrmBasic.PanelRightDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := Source.Control = FrmLocalVariables;
  if Accept then
  begin
    ARect.TopLeft := PanelRight.ClientToScreen(Point(-FrmLocalVariables.Width, 0));
    ARect.BottomRight := PanelRight.ClientToScreen(Point(0, PanelRight.Height));
    Source.DockRect := ARect;
  end;
end;

procedure TFrmBasic.PanelRightGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := DockClient is TFrmLocalVariables;
end;

procedure TFrmBasic.PanelRightUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  if ((Sender as TPanel).DockClientCount = 1) then
    ShowDockPanel(False);
end;

procedure TFrmBasic.BeforeBuildContent(Sender: TDocumentationBuilder;
  const FileName: TFileName; const Symbol: TSymbol; var Content: string);
begin
  if FAbortBuild then
  begin
    Content := '';
    Exit;
  end;

  ListBoxCompiler.Items.Append(Format('Build: %s', [FileName]));
  Application.ProcessMessages;
end;

procedure TFrmBasic.AcnBuildStartExecute(Sender: TObject);
begin
  if dwsDebugger.State = dsDebugSuspended then
  begin
    dwsDebugger.Resume;
    Exit;
  end;

  AcnBuildBuild.Execute;
  if not FAbortBuild then
  begin
    FrmPreview.Show;
    FrmPreview.WebBrowser.Navigate(FOutputPath + 'index.html');
  end;
end;

procedure TFrmBasic.AcnBuildStepOverExecute(Sender: TObject);
begin
  dwsDebugger.StepOver;
end;

procedure TFrmBasic.AcnBuildTraceIntoExecute(Sender: TObject);
begin
  dwsDebugger.StepDetailed;
end;

procedure TFrmBasic.AcnBuildBuildExecute(Sender: TObject);
var
  DocBuilder: TDocumentationBuilder;
  Prog: IdwsProgram;
begin
  FAbortBuild := False;
  FCriticalSection.Enter;
  try
    // compile code
    Prog := DelphiWebScript.Compile('uses ' + FSymbolUnit.UnitName + ';');

    if Prog.Msgs.HasErrors then
      Exit;

    DocBuilder := TDocumentationBuilder.Create(Prog);
    try
      DocBuilder.OnBeginBuildContent := BeforeBuildContent;
      DocBuilder.TemplateSource := SynEdit.Text;
      {$IFDEF UseDebugger}
      if dwsDebugger.Breakpoints.Count > 0 then
        DocBuilder.Debugger := dwsDebugger;
      {$ENDIF}
      DocBuilder.Build(FOutputPath);

      if DocBuilder.Aborted then
      begin
        FAbortBuild := True;
        Exit;
      end;
    finally
      FreeAndNil(DocBuilder);
    end;
  finally
    FCriticalSection.Leave;
  end;

  AcnViewPreview.Enabled := True;
  MnuView.Visible := True;
end;

procedure TFrmBasic.AcnBuildOptionsExecute(Sender: TObject);
begin
  with TFormOptions.Create(nil) do
  try
    case ShowModal of
      mrOk:
        begin
          FOutputPath := ExpandFileName(EdtOutputDir.Text);
        end;
    end;
  finally
    Free;
  end;
end;

procedure TFrmBasic.AcnFileNewExecute(Sender: TObject);
begin
  SynEdit.Clear;
end;

procedure TFrmBasic.AcnFileOpenAccept(Sender: TObject);
begin
  SynEdit.Lines.LoadFromFile(AcnFileOpen.Dialog.Filename);
end;

procedure TFrmBasic.AcnFileSaveScriptAsAccept(Sender: TObject);
begin
  SynEdit.Lines.SaveToFile(AcnFileSaveScriptAs.Dialog.Filename);
end;

procedure TFrmBasic.AcnFileScriptSaveExecute(Sender: TObject);
begin
  SynEdit.Lines.SaveToFile(FRecentScriptName);
end;

procedure TFrmBasic.AcnOptionsExecute(Sender: TObject);
var
  SynEditorOptionsContainer: TSynEditorOptionsContainer;
begin
  SynEditorOptionsContainer := TSynEditorOptionsContainer.Create(nil);
  SynEditorOptionsContainer.Assign(SynEdit);
  SynEditOptionsDialog.Execute(SynEditorOptionsContainer);
  SynEdit.Assign(SynEditorOptionsContainer);
end;

procedure TFrmBasic.AcnBuildResetExecute(Sender: TObject);
begin
  dwsDebugger.EndDebug;
end;

procedure TFrmBasic.AcnBuildResetUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := daCanEndDebug in dwsDebugger.AllowedActions;
end;

procedure TFrmBasic.AcnBuildDebuggerUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := dwsDebugger.State = dsDebugSuspended;
end;

procedure TFrmBasic.AcnViewLocalVariablesExecute(Sender: TObject);
begin
  FrmLocalVariables.Visible := AcnViewLocalVariables.Checked;
  ShowDockPanel(FrmLocalVariables.Visible)
end;

procedure TFrmBasic.AcnViewPreviewExecute(Sender: TObject);
begin
  FrmPreview.Visible := AcnViewPreview.Checked;
end;

procedure TFrmBasic.SourceChanged;
begin
  FSyncEvent.SetEvent;
end;

procedure TFrmBasic.SynCompletionProposalExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  SuggestionIndex: Integer;
  Proposal: TSynCompletionProposal;
  SourceFile: TSourceFile;
  ScriptPos: TScriptPos;
  Suggestions: IdwsSuggestions;
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

  // ok, get the compiled "program" from DWS
  if Assigned(FCompiledProgram) then
  begin
    SourceFile := FCompiledProgram.SourceList.MainScript.SourceFile;
    ScriptPos := TScriptPos.Create(SourceFile, SynEdit.CaretY, SynEdit.CaretX);
    Suggestions := TDWSSuggestions.Create(FCompiledProgram, ScriptPos,
      [soNoReservedWords]);

    // now populate the suggestion box
    for SuggestionIndex := 0 to Suggestions.Count - 1 do
    begin
      Proposal.ItemList.AddObject(Suggestions.Caption[SuggestionIndex],
        TObject(Suggestions.Category[SuggestionIndex]));
      Proposal.InsertList.Add(Suggestions.Code[SuggestionIndex]);
    end;
  end;

  CanExecute := True;
end;

procedure TFrmBasic.SynParametersExecute(Kind: SynCompletionType;
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
    Overloads : TFuncSymbolList;

    procedure CollectMethodOverloads(methSym : TMethodSymbol);
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
      ParameterInfos.Insert(0, '"<no parameters required>"');
    end;
  end;


var
  LineText: String;
  Proposal: TSynCompletionProposal;
  LocLine: string;
  TmpX: Integer;
  TmpLocation, StartX, ParenCounter: Integer;
  ParameterInfoList: TStrings;
begin
  CanExecute := False;
  Assert(Kind = ctParams);

  // check the proposal type
  if Sender is TSynCompletionProposal then
  begin
    Proposal := TSynCompletionProposal(Sender);
    Proposal.InsertList.Clear;
    Proposal.ItemList.Clear;
    ParameterInfoList := TStrings(Proposal.ItemList);

    // get current line
    LineText := SynEdit.LineText;

    with TSynCompletionProposal(Sender).Editor do
    begin
      // get current compiled program
      if not Assigned(FCompiledProgram) then
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

            GetParameterInfosForCursor(FCompiledProgram, TmpX, SynEdit.CaretY,
              ParameterInfoList, TmpLocation);

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

procedure TFrmBasic.SynCompletionProposalPaintItem(Sender: TObject;
  Index: Integer; TargetCanvas: TCanvas; ItemRect: TRect;
  var CustomDraw: Boolean);
var
  Offset: TPoint;
  ItemName, ItemHighlight: string;
  ImageIndex: Integer;
begin
  inherited;

  if Assigned(SynCompletionProposal.Images) then
  begin
    TargetCanvas.FillRect(ItemRect);

    ImageIndex := -1;
    case TdwsSuggestionCategory(SynCompletionProposal.ItemList.Objects[index]) of
      scFunction, scProcedure:
        ImageIndex := 0;
      scConstructor, scDestructor, scMethod:
        ImageIndex := 1;
      scProperty:
        ImageIndex := 2;
      scType, scRecord, scInterface:
        ImageIndex := 3;
      scClass:
        ImageIndex := 4;
      scUnit:
        ImageIndex := 5;
      scReservedWord:
        ImageIndex := 6;
    end;
    if ImageIndex >= 0 then
      SynCompletionProposal.Images.Draw(TargetCanvas, ItemRect.Left,
        ItemRect.Top, ImageIndex);

    Offset.X := ItemRect.Left + 18;
    Offset.Y := ItemRect.Top;

    ItemHighlight := SynCompletionProposal.InsertList[index];
    ItemName := SynCompletionProposal.ItemList[index];

    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    TargetCanvas.TextOut(Offset.X, Offset.Y, ItemHighlight);
    Delete(ItemName, 1, Length(ItemHighlight));
    Inc(Offset.X, TargetCanvas.TextWidth(ItemHighlight));
    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
    TargetCanvas.TextOut(Offset.X, Offset.Y, ItemName);

    CustomDraw := True;
  end;
end;

procedure TFrmBasic.SynCompletionProposalShow(Sender: TObject);
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

function TFrmBasic.GetBreakpointStatus(ALine: Integer): TBreakpointStatus;
var
  Test, Found: TdwsDebuggerBreakpoint;
  Index: Integer;
begin
  Result := bpsNone;
  if dwsDebugger.Breakpoints.Count = 0 then
    Exit;

  Test := TdwsDebuggerBreakpoint.Create;
  try
    Test.Line := ALine;
    Test.SourceName := SYS_MainModule;

    Index := dwsDebugger.Breakpoints.IndexOf(Test);
    if Index <> -1 then
    begin
      Found := dwsDebugger.Breakpoints[Index];
      if Found.Enabled then
        Result := bpsBreakpoint
      else
        Result := bpsBreakpointDisabled;
    end;
  finally
    FreeAndNil(Test);
  end;
end;

procedure TFrmBasic.AddBreakpoint(ALineNum: Integer; AEnabled: Boolean);
var
  BP: TdwsDebuggerBreakpoint;
  Added: Boolean;
  I: Integer;
begin
  BP := TdwsDebuggerBreakpoint.Create;
  BP.Line := ALineNum;

  BP.SourceName := SYS_MainModule;

  I := dwsDebugger.Breakpoints.AddOrFind(BP, Added);
  if not Added then
    BP.Free;
  dwsDebugger.Breakpoints[I].Enabled := AEnabled;

  SynEdit.InvalidateGutterLine(ALineNum);
  SynEdit.InvalidateLine(ALineNum);
end;

procedure TFrmBasic.SynEditChange(Sender: TObject);
begin
  SourceChanged;
end;

procedure TFrmBasic.SynEditGutterClick(Sender: TObject; Button: TMouseButton; X,
  Y, Line: Integer; Mark: TSynEditMark);
var
  IntLine : Integer;
begin
  IntLine := SynEdit.RowToLine(Line);
  if IntLine < FExecutableLines.Size then
  begin
    if GetBreakpointStatus(Line) <> bpsNone then
      ClearBreakpoint(IntLine)
    else
      AddBreakpoint(IntLine, True);
    SynEdit.Repaint;
  end;
end;

procedure TFrmBasic.SynEditGutterPaint(Sender: TObject; ALine, X, Y: Integer);
var
  StrLineNumber: string;
  LineNumberRect: TRect;
  GutterWidth, Offset: Integer;
  OldFont: TFont;
  ImgIndex: Integer;
begin
  with TSynEdit(Sender), Canvas do
  begin
    Brush.Style := bsClear;
    Font.Color := clGray;
    Pen.Color := clGray;
    GutterWidth := Gutter.Width - 5;

    Offset := (LineHeight - ProposalImages.Height) div 2
         + LineHeight * (SynEdit.LineToRow(ALine) - SynEdit.TopLine);

    if ALine = FCurrentLine then
    begin
      if GetBreakpointStatus(ALine) <> bpsNone then
        ImgIndex := 10
      else
        if dwsDebugger.State = dsDebugSuspended then
          ImgIndex := 9
        else
          ImgIndex := 11
    end
    else
      case GetBreakpointStatus(ALine) of
        bpsBreakpoint :
          if IsExecutableLine(ALine) then
            ImgIndex := 7
          else
            ImgIndex := 8;
        bpsBreakpointDisabled :
          ImgIndex := 8;
        else
          if IsExecutableLine(ALine) then
            ImgIndex := 11
          else
            ImgIndex := -1;
      end;

    if ImgIndex >= 0 then
      ProposalImages.Draw(Canvas, X, Offset, ImgIndex);

    if (ALine = 1) or (ALine = CaretY) or ((ALine mod 10) = 0) then
    begin
      StrLineNumber := IntToStr(ALine);
      LineNumberRect := Rect(x, y, GutterWidth, y + LineHeight);
      OldFont := TFont.Create;
      try
        OldFont.Assign(Canvas.Font);
        Canvas.Font := Gutter.Font;
        Canvas.TextRect(LineNumberRect, StrLineNumber, [tfVerticalCenter,
          tfSingleLine, tfRight]);
        Canvas.Font := OldFont;
      finally
        OldFont.Free;
      end;
    end
    else
    begin
      Canvas.Pen.Color := Gutter.Font.Color;
      if (ALine mod 5) = 0 then
        Offset := 5
      else
        Offset := 2;
      Inc(y, LineHeight div 2);
      Canvas.MoveTo(GutterWidth - Offset, y);
      Canvas.LineTo(GutterWidth, y);
    end;
  end;
end;

procedure TFrmBasic.SynEditSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
const
  BreakpointColor = TColor($FFC0C0);
  CurrentLineColor = TColor($A0A0F0);
  CurrentLineSteppingColor = TColor($A0C0F0);
begin
  if Line = FCurrentLine then
  begin
    Special := True;
    FG := clBlack;
    if dwsDebugger.State = dsDebugSuspended then
      BG := CurrentLineSteppingColor
     else
      BG := CurrentLineColor
  end
  else
  if GetBreakpointStatus(Line) = bpsBreakpoint then
  begin
    Special := True;
    FG := clBlack;
    BG := BreakpointColor;
  end;
end;

end.
