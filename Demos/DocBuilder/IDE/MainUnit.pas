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

{$I dws.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, StdActns, ActnList, ExtDlgs, ComCtrls,
  Types, SyncObjs, ImgList, dwsComp, dwsExprs, dwsSymbols, dwsErrors,
  dwsSuggestions, dwsStrings, dwsUnitSymbols, dwsDocBuilder, dwsCompiler,
  dwsHtmlFilter, dwsSymbolsLibModule, dwsClassesLibModule,

  SynEdit, SynEditHighlighter, SynHighlighterDWS, SynCompletionProposal,
  SynEditMiscClasses, SynEditSearch, SynEditOptionsDialog, SynEditPlugins,
  SynMacroRecorder, SynHighlighterHtml, SynHighlighterMulti;

type
  TRescanThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TFrmBasic = class(TForm)
    AcnBuildDoc: TAction;
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
    ActionList: TActionList;
    DelphiWebScript: TDelphiWebScript;
    MainMenu: TMainMenu;
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
    MnuOptions: TMenuItem;
    MnuSaveMessagesAs: TMenuItem;
    MnuSaveOutputAs: TMenuItem;
    MnuScript: TMenuItem;
    MnuScriptCompile: TMenuItem;
    MnuScriptExit: TMenuItem;
    MnuScriptOpen: TMenuItem;
    MnuSearch: TMenuItem;
    MnuSelectAll: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    PopupMenuMessages: TPopupMenu;
    PopupMenuOutput: TPopupMenu;
    ProposalImages: TImageList;
    SplitterVertical: TSplitter;
    StatusBar: TStatusBar;
    SynCompletionProposal: TSynCompletionProposal;
    SynDWSSyn: TSynDWSSyn;
    SynEditOptionsDialog: TSynEditOptionsDialog;
    SynEditSearch: TSynEditSearch;
    SynMacroRecorder: TSynMacroRecorder;
    SynParameters: TSynCompletionProposal;
    SynEdit: TSynEdit;
    SynMultiSyn: TSynMultiSyn;
    SynHTMLSyn: TSynHTMLSyn;
    dwsHtmlFilter: TdwsHtmlFilter;
    ListBoxCompiler: TListBox;
    dwsSymbolsLib: TdwsSymbolsLib;
    dwsClassesLib: TdwsClassesLib;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AcnBuildDocExecute(Sender: TObject);
    procedure AcnFileNewExecute(Sender: TObject);
    procedure AcnFileOpenAccept(Sender: TObject);
    procedure AcnFileSaveScriptAsAccept(Sender: TObject);
    procedure AcnFileScriptSaveExecute(Sender: TObject);
    procedure AcnOptionsExecute(Sender: TObject);
    procedure MnuSaveMessagesAsClick(Sender: TObject);
    procedure MnuScriptExitClick(Sender: TObject);
    procedure SynCompletionProposalExecute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure SynCompletionProposalPaintItem(Sender: TObject; Index: Integer; TargetCanvas: TCanvas; ItemRect: TRect; var CustomDraw: Boolean);
    procedure SynCompletionProposalShow(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditGutterPaint(Sender: TObject; aLine, X, Y: Integer);
    procedure SynParametersExecute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FRecentScriptName: TFileName;
    FRescanThread: TRescanThread;
    FCompiledProgram: IdwsProgram;
    FCriticalSection: TCriticalSection;
    FSyncEvent: TEvent;
    FSymbolUnit: TSymbolUnit;
    FAbortBuild: Boolean;

    procedure SourceChanged;
    procedure BeforeBuildContent(Sender: TDocumentationBuilder;
      const FileName: TFileName; const Symbol: TSymbol; var Content: string);
  public
    procedure CompileScript;
    procedure UpdateCompilerOutput;

    property SyncEvent: TEvent read FSyncEvent;
  end;

var
  FrmBasic: TFrmBasic;

implementation

{$R *.dfm}

uses
  Math, Registry, dwsUtils;

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


{ TFrmBasic }

procedure TFrmBasic.FormCreate(Sender: TObject);
begin
  FSymbolUnit := TSymbolUnit.Create(nil, 0);;
  FSymbolUnit.Script := DelphiWebScript;
  FCriticalSection := TCriticalSection.Create;
  FSyncEvent := TEvent.Create;
  FRescanThread := TRescanThread.Create;
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
    end;
    CloseKey;
  finally
    Free;
  end;

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
    CloseKey;
  finally
    Free;
  end;
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

procedure TFrmBasic.UpdateCompilerOutput;
begin
  if not Assigned(FCompiledProgram) then
    Exit;

  StatusBar.SimpleText := 'Compiled';
  ListBoxCompiler.Items.Text := FCompiledProgram.Msgs.AsInfo;
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

procedure TFrmBasic.AcnBuildDocExecute(Sender: TObject);
var
  DocBuilder: TDocumentationBuilder;
begin
  FAbortBuild := False;
  FCriticalSection.Enter;
  try
    DocBuilder := TDocumentationBuilder.Create(
      DelphiWebScript.Compile('uses ' + FSymbolUnit.UnitName + ';'));
    try
      DocBuilder.OnBeginBuildContent := BeforeBuildContent;
      DocBuilder.Build(ExtractFilePath(ParamStr(0)) + 'Docs\');
    finally
      FreeAndNil(DocBuilder);
    end;
  finally
    FCriticalSection.Leave;
  end;

  with TDocumentationBuilder.Create(FCompiledProgram) do
  try
    Build(ExtractFilePath(Application.ExeName) + 'Docs\');
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
    SourceFile := TSourceFile.Create;
    try
      SourceFile.Code := SynEdit.Lines.Text;
      ScriptPos := TScriptPos.Create(SourceFile, SynEdit.CaretX, SynEdit.CaretY);
    finally
      SourceFile.Free;
    end;
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

procedure TFrmBasic.SynEditChange(Sender: TObject);
begin
  SourceChanged;
end;

procedure TFrmBasic.SynEditGutterPaint(Sender: TObject; ALine, X, Y: Integer);
var
  StrLineNumber: string;
  LineNumberRect: TRect;
  GutterWidth, Offset: Integer;
  OldFont: TFont;
begin
  with TSynEdit(Sender), Canvas do
  begin
    Brush.Style := bsClear;
    GutterWidth := Gutter.Width - 5;
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

end.

