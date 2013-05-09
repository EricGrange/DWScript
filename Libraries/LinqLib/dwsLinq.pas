unit dwsLinq;

interface
uses
   Classes,
   dwsComp, dwsCompiler, dwsLanguageExtension, dwsSymbols, dwsExprs, dwsUtils,
   dwsTokenizer, dwsErrors, dwsConstExprs, dwsRelExprs, dwsMethodExprs;

type
   TSqlFromExpr = class;
   TSqlList = class;
   TSqlIdentifier = class;

   TJoinType = (jtFull, jtLeft, jtRight, jtFullOuter, jtCross);

   TdwsLinqFactory = class(TdwsCustomLangageExtension)
   protected
      function CreateExtension : TdwsLanguageExtension; override;
   end;

   TdwsLinqExtension = class(TdwsLanguageExtension)
   private //utility functions
      class procedure Error(const compiler: IdwsCompiler; const msg: string); overload;
      class procedure Error(const compiler: IdwsCompiler; const msg: string; const args: array of const); overload;
      function TokenEquals(tok: TTokenizer; const value: string): boolean; inline;
   private
      FDatabaseSymbol: TClassSymbol;
      FDatasetSymbol: TClassSymbol;
      FRecursionDepth: integer;
      function EnsureDatabase(const compiler: IdwsCompiler): boolean;
      procedure ReadFromExprBody(const compiler: IdwsCompiler; tok: TTokenizer; from: TSqlFromExpr);
      function ReadFromExpression(const compiler: IdwsCompiler; tok : TTokenizer) : TSqlFromExpr;
      function DoReadFromExpr(const compiler: IdwsCompiler; tok: TTokenizer; db: TDataSymbol): TSqlFromExpr;
      procedure ReadWhereExprs(const compiler: IdwsCompiler; tok: TTokenizer; from: TSqlFromExpr);
      procedure ReadSelectExprs(const compiler: IdwsCompiler; tok: TTokenizer; from: TSqlFromExpr);
      function ReadComparisonExpr(const compiler: IdwsCompiler; tok: TTokenizer): TRelOpExpr;
      function ReadSqlIdentifier(const compiler: IdwsCompiler; tok: TTokenizer;
        acceptStar: boolean = false): TSqlIdentifier;
      procedure ReadJoinExpr(const compiler: IdwsCompiler; tok: TTokenizer; from: TSqlFromExpr);
      function ReadJoinType(const compiler: IdwsCompiler; tok: TTokenizer): TJoinType;
      procedure ReadComparisons(const compiler: IdwsCompiler; tok: TTokenizer;
        list: TSqlList);
      procedure ReadOrderExprs(const compiler: IdwsCompiler; tok: TTokenizer;
        from: TSqlFromExpr);
   public
      procedure ReadScript(compiler : TdwsCompiler; sourceFile : TSourceFile;
                           scriptType : TScriptSourceType); override;
      function ReadUnknownName(compiler: TdwsCompiler) : TTypedExpr; override;
   end;

   TSqlList = class(TObjectList<TProgramExpr>);
   TSqlJoinExpr = class;

   TSqlFromExpr = class(TTypedExpr)
   private
      FTableName: string;
      FDBSymbol: TDataSymbol;
      FWhereList: TSqlList;
      FSelectList: TSqlList;
      FJoinList: TSqlList;
      FOrderList: TSqlList;
      FDistinct: boolean;
   private
      FSQL: string;
      FParams: TArrayConstantExpr;
      FMethod: TMethodExpr;
      function NewParam: string;
      procedure BuildSelectList(list: TStringList);
      procedure BuildQuery(compiler: TdwsCompiler);
      procedure Codegen(compiler: TdwsCompiler);
      procedure BuildWhereClause(compiler: TdwsCompiler; list: TStringList);
      procedure BuildConditionElement(expr: TTypedExpr; compiler: TdwsCompiler; list: TStringList);
      procedure BuildConditional(conditional: TSqlList; compiler: TdwsCompiler;
        list: TStringList);
      procedure BuildRelOpElement(expr: TRelOpExpr; compiler: TdwsCompiler;
        list: TStringList);
      function BuildHalfRelOpElement(expr: TTypedExpr;
        compiler: TdwsCompiler): string;
      procedure BuildJoinClause(compiler: TdwsCompiler; list: TStringList);
      procedure BuildOrderClause(list: TStringList);
      procedure WriteCommaList(exprs: TSqlList; list: TStringList);
   public
      constructor Create(const tableName: string; const symbol: TDataSymbol);
      destructor Destroy; override;
      function Eval(exec : TdwsExecution) : Variant; override;
   end;

   TSqlIdentifier = class(TConstStringExpr)
   public
      constructor Create(const name: string; const compiler: IdwsCompiler);
   end;

   TSqlJoinExpr = class(TNoResultExpr)
   private
      FJoinType: TJoinType;
      FJoinExpr: TSqlIdentifier;
      FCriteria: TSqlList;
   public
      constructor Create(const pos: TScriptPos; jt: TJoinType; JoinExpr: TSqlIdentifier; list: TSqlList);
      destructor Destroy; override;
   end;

implementation
uses
   SysUtils,
   dwsDatabaseLibModule, dwsUnitSymbols, dwsCoreExprs, dwsConvExprs;

{ TdwsLinqFactory }

function TdwsLinqFactory.CreateExtension: TdwsLanguageExtension;
begin
   Result:=TdwsLinqExtension.Create;
end;

{ TdwsLinqExtension }

class procedure TdwsLinqExtension.Error(const compiler: IdwsCompiler; const msg: string);
begin
   compiler.Msgs.AddCompilerError(compiler.Tokenizer.GetToken.FScriptPos, msg);
   Abort;
end;

class procedure TdwsLinqExtension.Error(const compiler: IdwsCompiler; const msg: string;
  const args: array of const);
begin
   error(compiler, format(msg, args));
end;

function TdwsLinqExtension.EnsureDatabase(const compiler: IdwsCompiler): boolean;
var
   db: TUnitMainSymbol;
begin
   result := true;
   if FDatabaseSymbol = nil then
   begin
      db := compiler.CurrentProg.UnitMains.Find('System.Data');
      if assigned(db) then
      begin
         FDatabaseSymbol := db.Table.FindTypeLocal('Database') as TClassSymbol;
         FDatasetSymbol := db.Table.FindTypeLocal('Dataset') as TClassSymbol;
      end;
      if FDatabaseSymbol = nil then
         Error(compiler, 'Database type not found in script');
      if FDatasetSymbol = nil then
         Error(compiler, 'Dataset type not found in script');
   end;
end;

function TdwsLinqExtension.ReadComparisonExpr(const compiler: IdwsCompiler; tok: TTokenizer): TRelOpExpr;
var
   expr: TTypedExpr;
begin
   expr := compiler.readExpr;
   try
      if not(expr is TRelOpExpr) then
         Error(compiler, 'Comparison expected');
      result := TRelOpExpr(expr);
   except
      expr.Free;
      raise;
   end;
end;

procedure TdwsLinqExtension.ReadComparisons(const compiler: IdwsCompiler; tok: TTokenizer; list: TSqlList);
var
   expr: TTypedExpr;
begin
   repeat
      expr := ReadComparisonExpr(compiler, tok);
      try
         while tok.TestDelete(ttOr) do
            expr := TBoolOrExpr.Create(compiler.CurrentProg, expr, ReadComparisonExpr(compiler, tok));
         List.Add(expr);
         expr := nil;
      except
         expr.Free;
         raise;
      end;
   until not tok.TestDelete(ttAND);
end;

procedure TdwsLinqExtension.ReadWhereExprs(const compiler: IdwsCompiler; tok: TTokenizer; from: TSqlFromExpr);
begin
   from.FWhereList := TSqlList.Create;
   tok.KillToken;
   ReadComparisons(compiler, tok, from.FWhereList);
end;

procedure TdwsLinqExtension.ReadSelectExprs(const compiler: IdwsCompiler;
  tok: TTokenizer; from: TSqlFromExpr);
begin
   tok.KillToken;
   if tok.TestName and TokenEquals(tok, 'distinct') then
   begin
      from.FDistinct := true;
      tok.KillToken;
   end;

   if tok.TestDelete(ttTIMES) then
      Exit;

   from.FSelectList := TSqlList.Create;
   repeat
      from.FSelectList.Add(ReadSqlIdentifier(compiler, tok, true));
   until not tok.TestDelete(ttCOMMA);
end;

function TdwsLinqExtension.ReadJoinType(const compiler: IdwsCompiler; tok: TTokenizer): TJoinType;
begin
   result := jtFull;
   if TokenEquals(tok, 'left') then
   begin
      tok.KillToken;
      if tok.TestName and TokenEquals(tok, 'outer') then
         tok.KillToken;
      result := jtLeft;
   end
   else if TokenEquals(tok, 'right') then
   begin
      tok.KillToken;
      if tok.TestName and TokenEquals(tok, 'outer') then
         tok.KillToken;
      result := jtRight;
   end
   else if TokenEquals(tok, 'full') then
   begin
      tok.KillToken;
      if tok.TestName and TokenEquals(tok, 'outer') then
      begin
         tok.KillToken;
         result := jtFullOuter;
      end;
   end
   else if TokenEquals(tok, 'cross') then
   begin
      tok.KillToken;
      result := jtCross;
   end;
   if not (tok.TestName and TokenEquals(tok, 'join')) then
      Error(compiler, 'Invalid join type');
   tok.KillToken;
end;

procedure TdwsLinqExtension.ReadJoinExpr(const compiler: IdwsCompiler; tok: TTokenizer; from: TSqlFromExpr);
var
   jt: TJoinType;
   JoinExpr: TTypedExpr;
   list: TSqlList;
   join: TSqlJoinExpr;
   pos: TScriptPos;
begin
   pos := tok.CurrentPos;
   jt := ReadJoinType(compiler, tok);
   list := nil;
   JoinExpr := compiler.ReadExpr;
   try
      if not (joinExpr is TSqlIdentifier) then
         Error(compiler, 'Table name expected');
      if jt <> jtCross then
      begin
         if not tok.TestDelete(ttON) then
            Error(compiler, 'Join criteria expected');
         list := TSqlList.Create;
         ReadComparisons(compiler, tok, list);
      end;
      if from.FJoinList = nil then
         from.FJoinList := TSqlList.Create;
   except
      list.Free;
      joinExpr.Free;
      raise;
   end;
   join := TSqlJoinExpr.Create(pos, jt, TSqlIdentifier(joinExpr), list);
   from.FJoinList.Add(join);
end;

procedure TdwsLinqExtension.ReadOrderExprs(const compiler: IdwsCompiler; tok: TTokenizer; from: TSqlFromExpr);
var
   ident: TSqlIdentifier;
begin
   tok.KillToken;
   if not (tok.TestName and TokenEquals(tok, 'by')) then
      Error(compiler, 'Invalid ORDER BY clause');
   tok.KillToken;

   from.FOrderList := TSqlList.Create;
   repeat
      ident := ReadSqlIdentifier(compiler, tok);
      if tok.TestName and (TokenEquals(tok, 'asc') or TokenEquals(tok, 'desc')) then
      begin
         ident.Value := format('%s %s', [ident.Value, tok.GetToken.AsString]);
         tok.KillToken;
      end;
      from.FOrderList.Add(ident);
   until not tok.TestDelete(ttCOMMA);
end;

procedure TdwsLinqExtension.ReadFromExprBody(const compiler: IdwsCompiler; tok: TTokenizer;
  from: TSqlFromExpr);
begin
   while TokenEquals(tok, 'join') or TokenEquals(tok, 'left') or
      TokenEquals(tok, 'right') or TokenEquals(tok, 'full') or TokenEquals(tok, 'cross') do
      ReadJoinExpr(compiler, tok, from);
   if TokenEquals(tok, 'where') then
      ReadWhereExprs(compiler, tok, from);
   if TokenEquals(tok, 'order') then
      ReadOrderExprs(compiler, tok, from);
//   if TokenEquals(tok, 'group') then
//      ;
   if TokenEquals(tok, 'select') then
      ReadSelectExprs(compiler, tok, from);
end;

function TdwsLinqExtension.DoReadFromExpr(const compiler: IdwsCompiler; tok: TTokenizer;
  db: TDataSymbol): TSqlFromExpr;
var
   token: TToken;
begin
   token := tok.GetToken;
   result := TSqlFromExpr.Create(token.AsString, db);
   try
      tok.KillToken;
      case tok.TestAny([ttSEMI, ttNAME]) of
         ttSemi: ;
         ttName: ReadFromExprBody(compiler, tok, result);
         else Error(compiler, 'Linq keyword expected');
      end;
      result.Typ := FDatasetSymbol;
   except
      result.Free;
      raise;
   end;
end;

function TdwsLinqExtension.ReadFromExpression(const compiler: IdwsCompiler; tok : TTokenizer): TSqlFromExpr;
var
   symbol: TSymbol;
begin
   result := nil;
   if not (tok.TestName and TokenEquals(tok, 'from')) then Exit;
   if not EnsureDatabase(compiler) then
      Exit;
   try
      inc(FRecursionDepth);
      try
         tok.KillToken;
         symbol := nil;
         if tok.TestName then
         begin
            symbol := compiler.CurrentProg.Table.FindSymbol(tok.GetToken.AsString, cvMagic);
            if not ((symbol is TDataSymbol) and (symbol.Typ.IsCompatible(FDatabaseSymbol))) then
               Error(compiler, '"%s" is not a database', [tok.GetToken.AsString]);
         end
         else Error(compiler, 'Identifier expected.');
         tok.KillToken;
         if not tok.TestDelete(ttDOT) then
            Error(compiler, '"." expected.');
         if not tok.Test(ttNAME) then
            Error(compiler, '"Identifier expected.');
         result := DoReadFromExpr(compiler, tok, TDataSymbol(symbol));
      except
         on EAbort do
            compiler.Msgs.AddCompilerStop(tok.GetToken.FScriptPos, 'Invalid LINQ expression');
      end;
   finally
      dec(FRecursionDepth);
   end;
end;

function TdwsLinqExtension.ReadSqlIdentifier(const compiler: IdwsCompiler; tok : TTokenizer;
   acceptStar: boolean = false): TSqlIdentifier;
begin
   if not tok.TestName then
      Error(compiler, 'Identifier expected.');
   result := TSqlIdentifier.Create(tok.GetToken.AsString, compiler);
   try
      tok.KillToken;
      if tok.TestDelete(ttDOT) then
      begin
         if not tok.TestName then
            if not (AcceptStar and tok.Test(ttTIMES)) then
               Error(compiler, 'Identifier expected.');
         result.Value := format('%s.%s', [result.Value, tok.GetToken.AsString]);
         tok.KillToken;
      end;
   except
      result.Free;
      raise;
   end;
end;

function TdwsLinqExtension.ReadUnknownName(compiler: TdwsCompiler) : TTypedExpr;
var
   tok : TTokenizer;
begin
   tok:=compiler.Tokenizer;
   if FRecursionDepth = 0 then
   begin
      result := ReadFromExpression(compiler, tok);
      if result = nil then
         Exit;
      TSqlFromExpr(result).Codegen(compiler);
   end
   else result := ReadSqlIdentifier(compiler, tok);
end;

procedure TdwsLinqExtension.ReadScript(compiler: TdwsCompiler;
  sourceFile: TSourceFile; scriptType: TScriptSourceType);
begin
   inherited;
   FDatabaseSymbol := nil;
end;

function TdwsLinqExtension.TokenEquals(tok: TTokenizer; const value: string): boolean;
begin
   result := UnicodeSameText(tok.GetToken.AsString, value);
end;

{ TSqlFromExpr }

constructor TSqlFromExpr.Create(const tableName: string; const symbol: TDataSymbol);
begin
   inherited Create;
   FTableName := tableName;
   FDBSymbol := symbol;
end;

destructor TSqlFromExpr.Destroy;
begin
   FMethod.Free;
   FWhereList.Free;
   FSelectList.Free;
   FJoinList.Free;
   FOrderList.Free;
   inherited Destroy;
end;

procedure TSqlFromExpr.WriteCommaList(exprs: TSqlList; list: TStringList);
var
   i: integer;
   item: string;
begin
   for i := 0 to exprs.Count - 1 do
   begin
      item := (exprs[i] as TSqlIdentifier).Value;
      if i < exprs.Count - 1 then
         item := item + ',';
      list.Add(item)
   end;
end;

procedure TSqlFromExpr.BuildSelectList(list: TStringList);
begin
   if FDistinct then
      list.Add('select distinct')
   else list.Add('select');

   WriteCommaList(FSelectList, list);
end;

function GetOp(expr: TRelOpExpr): string;
begin
   if expr.ClassType = TRelEqualVariantExpr then
      result := '='
   else if expr.ClassType = TRelNotEqualVariantExpr then
      result := '<>'
   else if expr.ClassType = TRelLessVariantExpr then
      result := '<'
   else if expr.ClassType = TRelLessEqualVariantExpr then
      result := '<='
   else if expr.ClassType = TRelGreaterVariantExpr then
      result := '>'
   else if expr.ClassType = TRelGreaterEqualVariantExpr then
      result := '>='
   else raise Exception.CreateFmt('Unknown op type: %s.', [expr.ClassName]);
end;

function TSqlFromExpr.BuildHalfRelOpElement(expr: TTypedExpr; compiler: TdwsCompiler): string;
begin
   if expr.ClassType = TSqlIdentifier then
      result := TSqlIdentifier(expr).Value
   else begin
      result := NewParam;
      FParams.AddElementExpr(compiler.CurrentProg, expr);
      expr.IncRefCount;
   end;
end;

procedure TSqlFromExpr.BuildRelOpElement(expr: TRelOpExpr; compiler: TdwsCompiler; list: TStringList);
var
   l, r: string;
begin
   l := BuildHalfRelOpElement(expr.Left, compiler);
   r := BuildHalfRelOpElement(expr.Right, compiler);
   list.Add(format('%s %s %s', [l, GetOp(expr), r]));
end;

procedure TSqlFromExpr.BuildConditionElement(expr: TTypedExpr; compiler: TdwsCompiler; list: TStringList);
begin
   if expr is TBooleanBinOpExpr then
   begin
      list.Add('(');
      BuildConditionElement(TBooleanBinOpExpr(expr).Left, compiler, list);
      if expr is TBoolAndExpr then
         list.Add(') and (')
      else if expr is TBoolOrExpr then
         list.Add(') or (')
      else TdwsLinqExtension.error(compiler, 'invalid binary operator');
      BuildConditionElement(TBooleanBinOpExpr(expr).Right, compiler, list);
      list.add(')');
   end
   else if expr is TRelOpExpr then
      BuildRelOpElement(TRelOpExpr(expr), compiler, list)
   else TdwsLinqExtension.error(compiler, 'invalid WHERE expression');
end;

procedure TSqlFromExpr.BuildConditional(conditional: TSqlList; compiler: TdwsCompiler; list: TStringList);
var
   i: integer;
   expr: TTypedExpr;
begin
   for i := 0 to conditional.Count - 1 do
   begin
      expr := conditional[i] as TTypedExpr;
      BuildConditionElement(expr, compiler, list);
      if i < conditional.Count - 1 then
         list.Add('and');
   end;
end;

procedure TSqlFromExpr.BuildWhereClause(compiler: TdwsCompiler; list: TStringList);
begin
   list.Add('where');
   BuildConditional(FWhereList, compiler, list);
end;

procedure TSqlFromExpr.BuildJoinClause(compiler: TdwsCompiler; list: TStringList);
const JOINS: array[TJoinType] of string = ('', 'left ', 'right ', 'full outer ', 'cross ');
var
   i: integer;
   join: TSqlJoinExpr;
   joinLine: string;
begin
   for i := 0 to FJoinList.Count - 1 do
   begin
      join := FJoinList[i] as TSqlJoinExpr;
      joinLine := JOINS[join.FJoinType] + 'join ' + join.FJoinExpr.Value;
      if assigned(join.FCriteria) then
         joinLine := joinLine + ' on';
      list.Add(joinLine);
      if assigned(join.FCriteria) then
         BuildConditional(join.FCriteria, compiler, list);
   end;
end;

procedure TSqlFromExpr.BuildOrderClause(list: TStringList);
begin
   list.Add('order by');
   WriteCommaList(FOrderList, list);
end;

procedure TSqlFromExpr.BuildQuery(compiler: TdwsCompiler);
var
   list: TStringList;
begin
   list := TStringList.Create;
   try
      if FSelectList = nil then
      begin
         if FDistinct then
            list.Add('select distinct *')
         else list.Add('select *')
      end
      else BuildSelectList(list);
      list.Add('from ' + FTableName);
      if assigned(FJoinList) then
         BuildJoinClause(compiler, list);
      if assigned(FWhereList) then
         BuildWhereClause(compiler, list);
      if assigned(FOrderList) then
         BuildOrderClause(list);
      FSql := list.Text;
   finally
      list.Free;
   end;
end;

procedure TSqlFromExpr.Codegen(compiler: TdwsCompiler);
var
   query: TMethodSymbol;
   prog: TdwsProgram;
   base: TVarExpr;
   pos: TScriptPos;
   arr: TTypedExpr;
begin
   query := (FDBSymbol.Typ as TClassSymbol).Members.FindSymbol('query', cvMagic) as TMethodSymbol;
   prog := compiler.CurrentProg;
   pos := compiler.Tokenizer.CurrentPos;
   base := TVarExpr.CreateTyped(prog, FDBSymbol);

   FParams := TArrayConstantExpr.Create(prog, pos);
   BuildQuery(compiler);

   FMethod := TMethodStaticExpr.Create(prog, pos, query, base);
   FMethod.AddArg(TConstStringExpr.Create(prog, nil, FSql));
   arr := TConvStaticArrayToDynamicExpr.Create(prog, FParams, TDynamicArraySymbol(query.Params.Symbols[1].Typ));
   FMethod.AddArg(arr);
   FMethod.Initialize(prog);
end;

function TSqlFromExpr.Eval(exec: TdwsExecution): Variant;
begin
   FMethod.EvalAsVariant(exec, result);
end;

function TSqlFromExpr.NewParam: string;
begin
   result := ':p' + IntToStr(FParams.Size + 1);
end;

{ TSqlIdentifier }

constructor TSqlIdentifier.Create(const name: string; const compiler: IdwsCompiler);
begin
   inherited Create(compiler.CurrentProg, compiler.CurrentProg.TypVariant, name);
end;

{ TSqlJoinExpr }

constructor TSqlJoinExpr.Create(const pos: TScriptPos; jt: TJoinType; JoinExpr: TSqlIdentifier; list: TSqlList);
begin
   inherited Create(pos);
   FJoinType := jt;
   FJoinExpr := JoinExpr;
   FCriteria := list;
end;

destructor TSqlJoinExpr.Destroy;
begin
   FCriteria.Free;
   FJoinExpr.Free;
   inherited Destroy;
end;

end.
