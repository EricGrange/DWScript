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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsOverviewDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, ToolWin, StdCtrls, TypInfo,
  dwsExprs, dwsScriptSource, dwsSymbolDictionary, dwsSymbols, dwsUtils,
  dwsJSON;

type
   TIconIndex = (
      iiClass = 0, iiInterface, iiEnum, iiType, iiRecord, iiHelper,
      iiMethodPrivate, iiMethodProtected, iiMethodPublic, iiFunction,
      iiSource
      );
   TIconIndexSet = set of TIconIndex;

  TdwsOverviewDialog = class(TForm)
    TreeView: TTreeView;
    ImageList: TImageList;
    ToolBar: TToolBar;
    CBSort: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeViewDblClick(Sender: TObject);
    procedure TreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure CBSortChange(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

   private
      { Private declarations }
      FProg : IdwsProgram;
      FScriptPos : TScriptPos;
      FOnGoToScriptPos : TNotifyEvent;
      FFilter : TIconIndexSet;
      FFilterButtons : array [TIconIndex] of TToolButton;
      FExpandedNodes : TStringList;

      procedure FilterChanged(sender : TObject);

      procedure ValidateSelection;

      procedure CollectExpandedNodes(parent : TTreeNode);
      procedure ApplyExpandedNodes(parent : TTreeNode);

      procedure RefreshTree;
      procedure SortSymbols(list : TList);
      procedure AddSymbolsOfSourceFile(root : TTreeNode; const sourceFile : TSourceFile);
      procedure AddSymbolsOfComposite(parent : TTreeNode; const sourceFile : TSourceFile);
      procedure AddSymbolLocations(parent : TTreeNode; symbol : TSymbol; const sourceFile : TSourceFile);

  public
      { Public declarations }
      procedure Execute(const aProg : IdwsProgram; const aScriptPos : TScriptPos);

      procedure SavePreferences(wr : TdwsJSONWriter);
      procedure LoadPreferences(prefs : TdwsJSONValue);

      function  SavePreferencesAsJSON : String;
      procedure LoadPreferencesAsJSON(const j : String);

      property Filter : TIconIndexSet read FFilter write FFilter;
      property ScriptPos : TScriptPos read FScriptPos;
      property OnGoToScriptPos : TNotifyEvent read FOnGoToScriptPos write FOnGoToScriptPos;
  end;

implementation

{$R *.dfm}

const cIconIndexHints : array [TIconIndex] of String = (
      'Classes', 'Interfaces', 'Enumerations', 'other Types', 'Records', 'Helpers',
      'Private Methods', 'Protected Methods', 'Public & Published Methods', 'Functions & Procedures',
      'Source file'
      );

// ------------------
// ------------------ TdwsOverviewDialog ------------------
// ------------------

procedure TdwsOverviewDialog.FormCreate(Sender: TObject);
var
   i : TIconIndex;
   tb : TToolButton;
begin
   FExpandedNodes := TStringList.Create;

   for i := iiFunction downto iiClass do begin
      if i in [iiHelper] then begin
         tb := TToolButton.Create(ToolBar);
         tb.Style := tbsSeparator;
         tb.Parent := ToolBar;
         tb.Width := 7;
         tb.Tag := -1;
      end;

      tb := TToolButton.Create(ToolBar);
      tb.ImageIndex := Ord(i);
      tb.Parent := ToolBar;
      tb.Style := tbsCheck;
      tb.Tag := Ord(i);
      tb.OnClick := FilterChanged;
      tb.Hint := 'Show or hide ' + cIconIndexHints[i];
      Include(FFilter, i);
      FFilterButtons[i] := tb;
   end;
end;

procedure TdwsOverviewDialog.FormDestroy(Sender: TObject);
begin
   FExpandedNodes.Free;
end;

procedure TdwsOverviewDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
   if TreeView.Items.Count > 0 then begin
      CollectExpandedNodes(nil);
      TreeView.Items.Clear;
   end;
   FProg := nil;
end;

procedure TdwsOverviewDialog.FormDeactivate(Sender: TObject);
begin
   if Visible then
      Close;
end;

procedure TdwsOverviewDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if Key = 27 then Close;
end;

// Execute
//
procedure TdwsOverviewDialog.Execute(const aProg : IdwsProgram; const aScriptPos : TScriptPos);
begin
   FProg := aProg;
   FScriptPos := aScriptPos;

   if aProg.Msgs.HasErrors then
      Caption := 'Overview - INCOMPLETE because the code has errors'
   else Caption := 'Overview';

   RefreshTree;

   Show;
end;

// SavePreferences
//
procedure TdwsOverviewDialog.SavePreferences(wr : TdwsJSONWriter);
var
   i : TIconIndex;
begin
   wr.BeginObject;

   wr.BeginArray('Filters');
   for i := Low(TIconIndex) to High(TIconIndex) do begin
      if i in FFilter then begin
         wr.WriteString(Copy(GetEnumName(TypeInfo(TIconIndex), Ord(i)), 3));
      end;
   end;
   wr.EndArray;

   wr.WriteString('Sort', CBSort.Text);

   wr.EndObject;
end;

// LoadPreferences
//
procedure TdwsOverviewDialog.LoadPreferences(prefs : TdwsJSONValue);
var
   i, k : Integer;
   filters : TdwsJSONValue;
   iconIndex : TIconIndex;
begin
   if prefs = nil then Exit;
   filters := prefs.Items['Filters'];
   FFilter := [];
   for iconIndex := Low(FFilterButtons) to High(FFilterButtons) do
      if FFilterButtons[iconIndex] <> nil then
         FFilterButtons[iconIndex].Down := True;
   for i := 0 to filters.ElementCount-1 do begin
      k := GetEnumValue(TypeInfo(TIconIndex), 'ii'+filters.Elements[i].AsString);
      if Cardinal(k) <= Cardinal(High(TIconIndex)) then begin
         iconIndex := TIconIndex(k);
         if FFilterButtons[iconIndex] <> nil then begin
            Include(FFilter, iconIndex);
            FFilterButtons[iconIndex].Down := False;
         end
      end;
   end;

   k := CBSort.Items.IndexOf(prefs.Items['Sort'].AsString);
   if k < 0 then
      k := 0;
   CBSort.ItemIndex := k;
end;

// RefreshTree
//
procedure TdwsOverviewDialog.RefreshTree;
var
   root : TTreeNode;
begin
   TreeView.Items.BeginUpdate;
   try
      if TreeView.Items.Count > 0 then
         CollectExpandedNodes(nil);
      TreeView.Items.Clear;

      root := TreeView.Items.AddFirst(nil, FScriptPos.SourceName);
      root.ImageIndex := Ord(iiSource);
      root.SelectedIndex := Ord(iiSource);
      AddSymbolsOfSourceFile(root, FScriptPos.SourceFile);
      root.Expand(False);
      if root.Count = 1 then
         root.getFirstChild.Expand(False);

      ApplyExpandedNodes(nil);
   finally
      TreeView.Items.EndUpdate;
   end;
end;

// SortSymbols
//
function CompareDeclaration(Item1, Item2: Pointer): Integer;
begin
   Result := TSymbolPositionList(Item1).FindUsage(suDeclaration).ScriptPos
            .Compare(TSymbolPositionList(Item2).FindUsage(suDeclaration).ScriptPos);
end;
function CompareAlpha(Item1, Item2: Pointer): Integer;
begin
   Result := UnicodeCompareText(TSymbolPositionList(Item1).Symbol.Name,
                                TSymbolPositionList(Item2).Symbol.Name);
end;
procedure TdwsOverviewDialog.SortSymbols(list : TList);
begin
   if CBSort.ItemIndex = 0 then
      list.Sort(CompareDeclaration)
   else list.Sort(CompareAlpha);
end;

procedure TdwsOverviewDialog.TreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
   r : TRect;
   txt : String;
   p : Integer;
   symbol : TSymbol;
begin
   PaintImages := True;
   DefaultDraw := (Stage <> cdPostPaint) or (Node.Data = nil);
   if not DefaultDraw then begin
      r := Node.DisplayRect(True);
      r.Left := r.Right + 10;
      r.Right := TreeView.Width;
      if (GetWindowLong(TreeView.Handle, GWL_STYLE) and WS_VSCROLL) <> 0 then
         r.Right := r.Right - GetSystemMetrics(SM_CXVSCROLL);
      if Node.ImageIndex >= 0 then begin
         symbol := TSymbolPositionList(Node.Data).Symbol;
         txt := symbol.Description;
         p := Pos( LowerCase(symbol.Name), LowerCase(txt) );
         if p > 0 then begin
            txt := Trim(Copy(txt, 1, p-1)) + ' ' + Trim(Copy(txt, p + Length(symbol.Name)));
            txt := Trim(StrBeforeChar(txt, #13));
         end else txt := '';
      end else begin
         txt := 'Line ' + IntToStr(Integer(Node.Data));
      end;
      if txt <> '' then begin
         TreeView.Canvas.Font.Size := 8;
         TreeView.Canvas.Font.Name := 'Segoe UI';
         TreeView.Canvas.Font.Color := $AAAAAA;
         TreeView.Canvas.TextRect(r, txt, [tfLeft, tfSingleLine, tfVerticalCenter, tfEndEllipsis]);
      end;
   end;
end;

// FilterChanged
//
procedure TdwsOverviewDialog.FilterChanged(sender : TObject);
var
   i : TIconIndex;
begin
   i := TIconIndex((sender as TToolButton).Tag);
   if (sender as TToolButton).Down then
      Exclude(FFilter, i)
   else Include(FFilter, i);
   RefreshTree;
end;

procedure TdwsOverviewDialog.TreeViewDblClick(Sender: TObject);
begin
   ValidateSelection;
end;

// ValidateSelection
//
procedure TdwsOverviewDialog.ValidateSelection;
var
   node : TTreeNode;
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   node := TreeView.Selected;
   if node = nil then Exit;

   if node.ImageIndex >= 0 then begin
      symPosList := TSymbolPositionList(node.Data);
      if symPosList = nil then Exit;
      if symPosList.Count = 0 then Exit;

      symPos := symPosList.FindUsage(suImplementation);
      if symPos = nil then begin
         symPos := symPosList.FindUsage(suDeclaration);
         if symPos = nil then
            symPos := symPosList[0];
      end;

      FScriptPos := symPos.ScriptPos;
   end else begin
      FScriptPos.Line := Integer(node.Data);
   end;

   if Assigned(FOnGoToScriptPos) then
      FOnGoToScriptPos(Self);
   Close;
end;

// CollectExpandedNodes
//
procedure TdwsOverviewDialog.CBSortChange(Sender: TObject);
begin
   RefreshTree;
end;

procedure TdwsOverviewDialog.CollectExpandedNodes(parent : TTreeNode);
var
   child : TTreeNode;
begin
   if parent = nil then begin
      FExpandedNodes.Clear;
      child := TreeView.Items.GetFirstNode;
   end else child := parent.getFirstChild;
   while child <> nil do begin
      if child.HasChildren and child.Expanded then begin
         if child.Data<>nil then
            FExpandedNodes.Add(TSymbolPositionList(child.Data).Symbol.QualifiedName);
         CollectExpandedNodes(child);
      end;
      child := child.getNextSibling;
   end;
end;

// ApplyExpandedNodes
//
procedure TdwsOverviewDialog.ApplyExpandedNodes(parent : TTreeNode);
var
   child : TTreeNode;
begin
   if parent = nil then begin
      if FExpandedNodes.Count = 0 then Exit;
      child := TreeView.Items.GetFirstNode;
   end else child := parent.getFirstChild;
   while child <> nil do begin
      if child.Data <> nil then begin
         if child.ImageIndex >= 0 then begin
            if FExpandedNodes.IndexOf(TSymbolPositionList(child.Data).Symbol.QualifiedName) >= 0 then begin
               child.Expand(False);
               ApplyExpandedNodes(child);
            end;
         end;
      end else if child.Expanded then
         ApplyExpandedNodes(child);
      child := child.getNextSibling;
   end;
end;

procedure TdwsOverviewDialog.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
   if (Node.Count = 1) and (Node.getFirstChild.Text = '') then
      AddSymbolsOfComposite(Node, FScriptPos.SourceFile);
   AllowExpansion := True;
end;

procedure TdwsOverviewDialog.TreeViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if Key = 13 then
      ValidateSelection;
end;

procedure TdwsOverviewDialog.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   node : TTreeNode;
begin
   node := TreeView.GetNodeAt(x, y);
   if (node <> nil) and (node <> TreeView.Selected) then
      TreeView.Selected := node;
end;

// AddSymbolsOfSourceFile
//
procedure TdwsOverviewDialog.AddSymbolsOfSourceFile(root : TTreeNode; const sourceFile : TSourceFile);
var
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
   node : TTreeNode;
   symbolClass : TClass;
   localSymbols : TList;
   iconIndex : TIconIndex;
   i : Integer;
begin
   localSymbols := TList.Create;
   try
      for symPosList in FProg.SymbolDictionary do begin
         if symPosList.Symbol.Name='' then continue;
         if     (symPosList.Symbol is TTypeSymbol)
            and not (symPosList.Symbol is TMethodSymbol) then begin

            for symPos in symPosList do begin
               if (symPos.ScriptPos.SourceFile = sourceFile) and (suDeclaration in symPos.SymbolUsages) then begin
                  localSymbols.Add(symPosList);
                  break;
               end;
            end;
         end;
      end;

      SortSymbols(localSymbols);

      for i := 0 to localSymbols.Count-1 do begin
         symPosList := TSymbolPositionList(localSymbols[i]);

         symbolClass := symPosList.Symbol.ClassType;

         if symbolClass.InheritsFrom(TClassSymbol) then begin
            iconIndex := iiClass;
         end else if symbolClass.InheritsFrom(TRecordSymbol) then begin
            iconIndex := iiRecord;
         end else if symbolClass.InheritsFrom(TInterfaceSymbol) then begin
            iconIndex := iiInterface;
         end else if symbolClass.InheritsFrom(THelperSymbol) then begin
            iconIndex := iiHelper;
         end else if symbolClass.InheritsFrom(TEnumerationSymbol) then begin
            iconIndex := iiEnum;
         end else if symbolClass.InheritsFrom(TFuncSymbol) and (not symPosList.Symbol.IsType) then
            iconIndex := iiFunction
         else iconIndex := iiType;

         if iconIndex in FFilter then begin
            node := TreeView.Items.AddChild(root, symPosList.Symbol.Name);
            if iconIndex in [iiClass, iiRecord, iiInterface, iiHelper, iiFunction] then
               TreeView.Items.AddChild(node, '');
            node.Data := symPosList;
            node.ImageIndex := Ord(iconIndex);
            node.SelectedIndex := Ord(iconIndex);
         end;
      end;
   finally
      localSymbols.Free;
   end;
end;

// AddSymbolsOfComposite
//
procedure TdwsOverviewDialog.AddSymbolsOfComposite(parent : TTreeNode; const sourceFile : TSourceFile);
var
   symbol : TSymbol;
   symPosList : TSymbolPositionList;
   composite : TCompositeTypeSymbol;
   iconIndex : TIconIndex;
   node : TTreeNode;
   members : TList;
   i : Integer;
begin
   parent.DeleteChildren;

   symbol := TSymbolPositionList(parent.Data).Symbol;
   if not (symbol is TCompositeTypeSymbol) then begin
      AddSymbolLocations(parent, symbol, sourceFile);
      Exit;
   end;

   composite := symbol as TCompositeTypeSymbol;

   members := TList.Create;
   try
      for symbol in composite.Members do begin
         if symbol.Name='' then continue;
         symPosList := FProg.SymbolDictionary.FindSymbolPosList(symbol);
         if symPosList = nil then continue;
         if symbol is TMethodSymbol then begin
            members.Add(symPosList);
         end;
      end;
      SortSymbols(members);
      for i := 0 to members.Count-1 do begin
         symPosList := TSymbolPositionList(members[i]);
         if symPosList.FindAnyUsageInFile([suDeclaration, suImplementation], sourceFile) = nil then continue;
         symbol := symPosList.Symbol;
         if symbol is TMethodSymbol then begin
            case TMethodSymbol(symbol).Visibility of
               cvPrivate :   iconIndex := iiMethodPrivate;
               cvProtected : iconIndex := iiMethodProtected;
            else
               iconIndex := iiMethodPublic;
            end;
         end else iconIndex := iiFunction;
         if iconIndex in FFilter then begin
            node := TreeView.Items.AddChild(parent, symbol.Name);
            node.Data := symPosList;
            node.ImageIndex := Ord(iconIndex);
            node.SelectedIndex := Ord(iconIndex);
            if symbol is TMethodSymbol then
               TreeView.Items.AddChild(node, '');
         end;
      end;
   finally
      members.Free;
   end;
end;

// AddSymbolLocations
//
procedure TdwsOverviewDialog.AddSymbolLocations(parent : TTreeNode; symbol : TSymbol; const sourceFile : TSourceFile);
var
   symPosList : TSymbolPositionList;
   prevSymPos : TSymbolPosition;
   symPos : TSymbolPosition;

   procedure AddLocation(const locationLabel : String; usage : TSymbolUsage);
   var
      node : TTreeNode;
   begin
      symPos := symPosList.FindAnyUsageInFile([usage], sourceFile);
      if (symPos <> nil) and (symPos <> prevSymPos) then begin
         node := TreeView.Items.AddChildFirst(parent, locationLabel);
         node.Data := Pointer(symPos.ScriptPos.Line);
         node.ImageIndex := -1;
         node.SelectedIndex := -1;
      end;
      prevSymPos := symPos;
   end;

begin
   symPosList := FProg.SymbolDictionary.FindSymbolPosList(symbol);
   if symPosList = nil then Exit;
   prevSymPos := nil;
   AddLocation('Implementation', suImplementation);
   AddLocation('Declaration', suDeclaration);
   AddLocation('Forward', suForward);
end;

// SavePreferencesAsJSON
//
function TdwsOverviewDialog.SavePreferencesAsJSON : String;
var
   wr : TdwsJSONWriter;
begin
   wr := TdwsJSONWriter.Create;
   try
      SavePreferences(wr);
      Result := wr.ToString;
   finally
      wr.Free;
   end;
end;

// LoadPreferencesAsJSON
//
procedure TdwsOverviewDialog.LoadPreferencesAsJSON(const j : String);
var
   v : TdwsJSONValue;
begin
   if j = '' then Exit;
   v := TdwsJSONValue.ParseString(j);
   try
      LoadPreferences(v);
   finally
      v.Free;
   end;
end;

end.
