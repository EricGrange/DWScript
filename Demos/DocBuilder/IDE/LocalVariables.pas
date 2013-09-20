unit LocalVariables;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  dwsUtils;

type
  TFrmLocalVariables = class(TForm)
    ListView: TListView;
    procedure FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    procedure Redraw;
  end;

var
  FrmLocalVariables: TFrmLocalVariables;

implementation

{$R *.dfm}

uses
  System.Variants, dwsDebugger, dwsSymbols, dwsCompiler, dwsExprs, DockingUtils,
  MainUnit;

function DebuggerEvaluate(ADebugger : TDwsDebugger; const AExpression : string) : String;
var
  expr: IdwsEvaluateExpr;
  V: variant;
begin
  try
    expr := ADebugger.Evaluate(AExpression);
    try
      Result := '(no result)';
      expr.Expression.EvalAsVariant(ADebugger.Execution.ExecutionObject, V);
      Result := VarToStr(V);
      if VarIsStr(V) then
        Result := '''' + Result + '''';
    finally
      expr := nil;
    end;
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

procedure TFrmLocalVariables.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
  FrmBasic.ShowDockPanel(False);
  FrmBasic.AcnViewLocalVariables.Checked := False;
end;

procedure TFrmLocalVariables.FormDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := (Source.Control is TFrmLocalVariables);
end;

procedure TFrmLocalVariables.FormStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  DragObject := TTransparentDragDockObject.Create(Self);
end;

procedure TFrmLocalVariables.Redraw;

  procedure AppendSymbol(const AName: string);
  var
    S: string;
    Item: TListItem;
  begin
    S := DebuggerEvaluate(FrmBasic.dwsDebugger, AName);

    Item := ListView.Items.Add;
    Item.Caption := AName;
    Item.SubItems.Add(S);
  end;

  procedure AppendSymbolsToDisplay(ATable: TSymbolTable; AExec: TdwsProgramExecution);
  var
    I: Integer;
    Sym: TSymbol;
  begin
    for I := 0 to ATable.Count - 1 do
    begin
      Sym := ATable[I];
      if Sym is TDataSymbol then
        AppendSymbol(Sym.Name);
    end
  end;

  procedure AppendParamsToDisplay(AProc: TdwsProcedure; AExec: TdwsProgramExecution);
  var
    I: Integer;
    Sym: TSymbol;
  begin
    for I := 0 to AProc.Func.Params.Count - 1 do
    begin
      Sym := AProc.Func.Params[I];
      if Sym is TDataSymbol then
        AppendSymbol(Sym.Name);
    end;

    // If it is a function, get the function result
    Sym := AProc.Func.Result;
    if Assigned(Sym) then
      AppendSymbol(Sym.Name);
  end;

var
  ProgramExecution : TdwsProgramExecution;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;

    ProgramExecution := TdwsProgramExecution(FrmBasic.dwsDebugger.Execution);

    if ProgramExecution.CurrentProg is TdwsProcedure then
      AppendParamsToDisplay(TdwsProcedure(ProgramExecution.CurrentProg), ProgramExecution);

    AppendSymbolsToDisplay(ProgramExecution.CurrentProg.Table, ProgramExecution);
  finally
    ListView.Items.EndUpdate;
  end;
end;

end.
