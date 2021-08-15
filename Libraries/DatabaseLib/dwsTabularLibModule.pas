unit dwsTabularLibModule;

interface

uses
  System.SysUtils, System.Classes, dwsComp, dwsExprs, dwsSymbols;

type
  TdwsTabularLib = class(TDataModule)
    dwsTabular: TdwsUnit;
    procedure dwsTabularClassesTabularDataConstructorsCreateFromDataSetEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTabularClassesTabularDataCleanUp(ExternalObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsEvaluateAggregateEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsEvaluateNewColumnEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsExportToSeparatedEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsDropColumnEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses dwsUtils, dwsTabular, dwsDatabaseLibModule, dwsDatabase;

{$R *.dfm}

function PrepareExprFromOpcode(const tabular : TdwsTabular; const opCodes : IScriptDynArray) : TdwsTabularExpression;
var
   buf : String;
   i : Integer;
begin
   Result := TdwsTabularExpression.Create(tabular);
   try
      for i := 0 to opCodes.ArrayLength-1 do begin
         opCodes.EvalAsString(i, buf);
         Result.Opcode(buf);
      end;
   except
      Result.Free;
      raise;
   end;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataConstructorsCreateFromDataSetEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   dsFields : array of IdwsDataField;
   buf : String;
begin
   var ds := Info.ParamAsScriptObj[0];
   var scriptDS := (ds.ExternalObject as TScriptDataSet).Intf;

   var tabular := TdwsTabular.Create;
   ExtObject := tabular;

   var nbFields := scriptDS.FieldCount;
   SetLength(dsFields, nbFields);
   for var i := 0 to nbFields-1 do begin
      dsFields[i] := scriptDS.Fields[i];
      tabular.AddColumn(dsFields[i].Name);
   end;

   while not scriptDS.Eof do begin
      for var i := 0 to nbFields-1 do begin
         dsFields[i].GetAsString(buf);
         tabular.ColumnData[i].Add(buf, dsFields[i].AsFloat);
      end;
      scriptDS.Next;
   end;

   tabular.InitializeJIT;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsDropColumnEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TdwsTabular;
   info.ResultAsBoolean := tabular.DropColumn(Info.ParamAsString[0]);
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsEvaluateAggregateEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TdwsTabular;
   var aggregateFunc := Info.ParamAsString[0];
   var opCodes := Info.ParamAsScriptDynArray[1];

   var expr := PrepareExprFromOpcode(tabular, opCodes);
   try
      expr.JITCompile;
      var stack := TdwsTabularStack.Create(expr.MaxStackDepth);
      try
         if aggregateFunc = 'sum' then begin
            var sum : Double := 0;
            var buf : TdwsTabularBatchResult;
            stack.RowIndex := 0;
            for var i := 1 to tabular.RowCount div Length(buf) do begin
               expr.EvaluateBatch(stack, buf);
               sum := sum + buf[0] + buf[1] + buf[2] + buf[3];
            end;
            for var i := stack.RowIndex to tabular.RowCount-1 do begin
               stack.RowIndex := i;
               sum := sum + expr.Evaluate(stack);
            end;
            Info.ResultAsFloat := sum;
         end else raise EdwsTabular.CreateFmt('Unsupported aggregate function "%s"', [ aggregateFunc ]);
      finally
         stack.Free;
      end;
   finally
      expr.Free;
   end;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsEvaluateNewColumnEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TdwsTabular;
   var opCodes := Info.ParamAsScriptDynArray[1];

   var expr := PrepareExprFromOpcode(tabular, opCodes);
   try
      expr.JITCompile;
      var column := tabular.AddColumn(Info.ParamAsString[0]);
      var stack := TdwsTabularStack.Create(expr.MaxStackDepth);
      try
         for var i := 0 to tabular.RowCount-1 do begin
            stack.RowIndex := i;
            column.Add(expr.Evaluate(stack));
         end;
      finally
         stack.Free;
      end;
   finally
      expr.Free;
   end;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsExportToSeparatedEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   wobs : TWriteOnlyBlockStream;
   columns : TdwsTabularColumns;
   separator, quote : String;
   separatorFirst, quoteFirst : Char;

   procedure WriteQuoted(const s : String);
   begin
      var buf := s;
      FastStringReplace(buf, quote, quote+quote);
      wobs.WriteString(quote + buf + quote);
   end;

   procedure Write(const s : String);
   var
      p : PChar;
   begin
      if s = '' then Exit;
      p := PChar(s);
      repeat
         if (p^ = separatorFirst) or (p^ = quoteFirst) then begin
            WriteQuoted(s);
            Exit;
         end;
         Inc(p);
      until p^ = #0;
      wobs.WriteString(s);
   end;

var
   buf : String;
begin
   var tabular := Info.ScriptObj.ExternalObject as TdwsTabular;
   var columnNames := Info.ParamAsScriptDynArray[0];

   quote := Info.ParamAsString[2];
   if separator <> '' then
      separatorFirst := separator[1]
   else separatorFirst := #0;

   separator := Info.ParamAsString[1];
   if quote <> '' then
      quoteFirst := quote[1]
   else quoteFirst := #0;

   wobs := TWriteOnlyBlockStream.AllocFromPool;
   try
      var nbCols := columnNames.ArrayLength;
      SetLength(columns, nbCols);
      for var i := 0 to nbCols-1 do begin
         columnNames.EvalAsString(i, buf);
         var col := tabular.ColumnByName(buf);
         if col = nil then
            raise EdwsTabular.CreateFmt('Unknown column "%s"', [ buf ]);
         columns[i] := col;
         if i > 0 then
            wobs.WriteString(separator);
         Write(buf);
      end;
      wobs.WriteCRLF;
      for var k := 0 to tabular.RowCount-1 do begin
         for var i := 0 to nbCols-1 do begin
            if i > 0 then
               wobs.WriteString(separator);
            Write(columns[i].Strings[k]);
         end;
         wobs.WriteCRLF;
      end;
      Info.ResultAsString := wobs.ToString;
   finally
      wobs.Free;
   end;
end;

end.
