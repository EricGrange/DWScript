unit dwsLinq;

interface
uses
   Classes,
   dwsComp, dwsCompiler, dwsLanguageExtension, dwsSymbols, dwsExprs, dwsUtils,
   dwsXPlatform, dwsScriptSource, dwsTokenTypes, dwsTokenizer, dwsErrors,
   dwsConstExprs, dwsRelExprs, dwsMethodExprs, dwsCoreExprs;

type
   TSqlList = class;
   TSqlIdentifier = class;
   TSqlFunction = class;
   TSqlJoinExpr = class;

   ILinqQueryBuilder = interface
      function From(value: TTypedExpr; base: TDataSymbol): TTypedExpr;
      function Join(from: TTypedExpr; value: TSqlJoinExpr): TTypedExpr;
      function Where(from: TTypedExpr; list: TSqlList): TTypedExpr;
      function Group(from: TTypedExpr; list: TSqlList): TTypedExpr;
      function Order(from: TTypedExpr; list: TSqlList): TTypedExpr;
      function Select(from: TTypedExpr; list: TSqlList): TTypedExpr;
      function Into(from: TTypedExpr; value: TFuncPtrExpr; aPos: TScriptPos): TTypedExpr;
      function Distinct(from: TTypedExpr): TTypedExpr;
      procedure Finalize(From: TTypedExpr);
      function NeedsDot: boolean;
   end;

   TLinqQueryBuilderFactory = function(compiler: IdwsCompiler; symbol: TTypeSymbol): ILinqQueryBuilder;

   TJoinType = (jtFull, jtLeft, jtRight, jtFullOuter, jtCross);

   TdwsLinqFactory = class(TdwsCustomLangageExtension)
   private
      FQueryBuilders: TSimpleList<TLinqQueryBuilderFactory>;
   protected
      function CreateExtension : TdwsLanguageExtension; override;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure RegisterSource(factory: TLinqQueryBuilderFactory);
   end;

   TdwsLinqExtension = class(TdwsLanguageExtension)
   public //utility functions
      class procedure Error(const compiler: IdwsCompiler; const msg: string); overload;
      class procedure Error(const compiler: IdwsCompiler; const msg: string; const args: array of const); overload;
   private //utility functions
      function TokenEquals(tok: TTokenizer; const value: string): boolean; inline;
   private
      FRecursionDepth: integer;
      FQueryBuilder: ILinqQueryBuilder;
      FQueryBuilders: TSimpleList<TLinqQueryBuilderFactory>;
      function findQueryBuilder(const compiler: IdwsCompiler; typ: TTypeSymbol;
        out builder: ILinqQueryBuilder): boolean;
      function ReadExpression(const compiler: IdwsCompiler; tok: TTokenizer): TTypedExpr;
      procedure ReadFromExprBody(const compiler: IdwsCompiler; tok: TTokenizer; var from: TTypedExpr);
      function ReadFromExpression(const compiler: IdwsCompiler; tok : TTokenizer) : TTypedExpr;
      function DoReadFromExpr(const compiler: IdwsCompiler; tok: TTokenizer; db: TDataSymbol): TTypedExpr;
      procedure ReadWhereExprs(const compiler: IdwsCompiler; tok: TTokenizer; var from: TTypedExpr);
      procedure ReadSelectExprs(const compiler: IdwsCompiler; tok: TTokenizer; var from: TTypedExpr);
      function ReadComparisonExpr(const compiler: IdwsCompiler; tok: TTokenizer): TRelOpExpr;
      function ReadSqlIdentifier(const compiler: IdwsCompiler; tok: TTokenizer;
        acceptStar: boolean = false): TSqlIdentifier;
      function ReadRenamableSqlIdentifier(const compiler: IdwsCompiler; tok: TTokenizer;
        acceptStar: boolean = false): TSqlIdentifier;
      procedure ReadJoinExpr(const compiler: IdwsCompiler; tok: TTokenizer; var from: TTypedExpr);
      function ReadJoinType(const compiler: IdwsCompiler; tok: TTokenizer): TJoinType;
      procedure ReadComparisons(const compiler: IdwsCompiler; tok: TTokenizer;
        list: TSqlList);
      procedure ReadOrderExprs(const compiler: IdwsCompiler; tok: TTokenizer;
        var from: TTypedExpr);
      procedure ReadGroupByExprs(const compiler: IdwsCompiler; tok: TTokenizer;
        var from: TTypedExpr);
      function ReadSqlFunction(const compiler: IdwsCompiler; tok: TTokenizer;
        base: TSqlIdentifier): TSqlFunction;
      function ReadIntoExpression(const compiler: IdwsCompiler; tok: TTokenizer;
        base: TTypedExpr): TTypedExpr;
      function ValidIntoExpr(from, target: TTypedExpr): boolean;
      function BuildInExpr(const compiler: IdwsCompiler; expr: TRelGreaterEqualIntExpr): TRelOpExpr;
      function IsLinqKeyword(tok: TTokenizer): boolean;
   public
      function StaticSymbols : Boolean; override;
      procedure ReadScript(compiler : TdwsCompiler; sourceFile : TSourceFile;
                           scriptType : TScriptSourceType); override;
      function ReadUnknownName(compiler: TdwsCompiler) : TTypedExpr; override;
   end;

   TSqlList = class(TObjectList<TProgramExpr>);

   TNewParamFunc = function: string of object;

   TSqlIdentifier = class(TConstStringExpr)
   private
      FRename: string;
   public
      constructor Create(const name: string; const compiler: IdwsCompiler);
      function GetValue(params: TArrayConstantExpr; prog: TdwsProgram;
        newParam: TNewParamFunc): string; virtual;
      property rename: string read FRename;
   end;

   TSqlFunction = class(TSqlIdentifier)
   private
      FFunction: TFuncExprBase;
   public
      constructor Create(base: TsqlIdentifier; const compiler: IdwsCompiler);
      destructor Destroy; override;
      function GetValue(params: TArrayConstantExpr; prog: TdwsProgram;
        newParam: TNewParamFunc): string; override;
   end;

   TSqlJoinExpr = class(TNoResultExpr)
   private
      FJoinType: TJoinType;
      FJoinExpr: TSqlIdentifier;
      FCriteria: TSqlList;
   public
      constructor Create(const pos: TScriptPos; jt: TJoinType; JoinExpr: TSqlIdentifier; list: TSqlList);
      destructor Destroy; override;
      property JoinType: TJoinType read FJoinType;
      property JoinExpr: TSqlIdentifier read FJoinExpr;
      property Criteria: TSqlList read FCriteria;
   end;

   TSqlInExpr = class(TRelOpExpr)
   public
      procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString); override;
   end;

   type TRelOp = (roEq, roNeq, roGt, roLt, roGte, roLte, roIn, roNin);

   function GetOp(expr: TRelOpExpr): TRelOp;

implementation
uses
   SysUtils,
   dwsExprList, dwsDatabaseLibModule, dwsUnitSymbols, dwsConvExprs,
   dwsArrayExprs, dwsArrayIndexOfExprs;

{ TdwsLinqFactory }

constructor TdwsLinqFactory.Create(AOwner: TComponent);
begin
   FQueryBuilders := TSimpleList<TLinqQueryBuilderFactory>.Create;
   inherited Create(AOwner);
end;

destructor TdwsLinqFactory.Destroy;
begin
   FQueryBuilders.Free;
   inherited Destroy;
end;

procedure TdwsLinqFactory.RegisterSource(factory: TLinqQueryBuilderFactory);
begin
   FQueryBuilders.Add(factory);
end;

function TdwsLinqFactory.CreateExtension: TdwsLanguageExtension;
begin
   Result:=TdwsLinqExtension.Create;
   TdwsLinqExtension(result).FQueryBuilders := self.FQueryBuilders;
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

function TdwsLinqExtension.StaticSymbols: Boolean;
begin
   result := true;
end;

function TdwsLinqExtension.BuildInExpr(const compiler: IdwsCompiler; expr: TRelGreaterEqualIntExpr): TRelOpExpr;
var
   idx: TArrayIndexOfExpr;
   base: TTypedExpr;
   ident: TSqlIdentifier;
begin
   idx := expr.Left as TArrayIndexOfExpr;
   if not ((idx.ItemExpr is TConvVarToIntegerExpr)
           and (TConvVarToIntegerExpr(idx.ItemExpr).expr is TSqlIdentifier)) then
      exit(expr);
   base := idx.baseExpr;
   assert(base.typ.classtype = TDynamicArraySymbol);
   ident := TSqlIdentifier(TConvVarToIntegerExpr(idx.ItemExpr).expr);
   result := TSqlInExpr.Create(compiler.CompilerContext, compiler.Tokenizer.HotPos, ttIN, ident, base);
   base.IncRefCount;
   ident.IncRefCount;
   expr.Free;
end;

function TdwsLinqExtension.ReadComparisonExpr(const compiler: IdwsCompiler; tok: TTokenizer): TRelOpExpr;
var
   expr: TTypedExpr;
begin
   expr := ReadExpression(compiler, tok);
   try
      if not(expr is TRelOpExpr) then
         Error(compiler, 'Comparison expected');
      if (expr.classtype = TRelGreaterEqualIntExpr) and (TRelOpExpr(expr).left is TArrayIndexOfExpr) then
         result := BuildInExpr(compiler, TRelGreaterEqualIntExpr(expr))
      else result := TRelOpExpr(expr);
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
            expr := TBoolOrExpr.Create(compiler.CompilerContext, cNullPos, ttOR, expr, ReadComparisonExpr(compiler, tok));
         List.Add(expr);
         expr := nil;
      except
         expr.Free;
         raise;
      end;
   until not tok.TestDelete(ttAND);
end;

function TdwsLinqExtension.ReadExpression(const compiler: IdwsCompiler; tok: TTokenizer): TTypedExpr;
begin
   if tok.TestDelete(ttAMP) then
      result := self.ReadRenamableSqlIdentifier(compiler, tok, true)
   else result := compiler.ReadExpr();
end;

procedure TdwsLinqExtension.ReadWhereExprs(const compiler: IdwsCompiler; tok: TTokenizer; var from: TTypedExpr);
var
   list: TSqlList;
begin
   list := TSqlList.Create;
   tok.KillToken;
   try
      ReadComparisons(compiler, tok, list);
      from := FQueryBuilder.Where(from, list);
   except
      list.Free;
      raise;
   end;
end;

procedure TdwsLinqExtension.ReadSelectExprs(const compiler: IdwsCompiler;
  tok: TTokenizer; var from: TTypedExpr);
var
   list: TSqlList;
begin
   tok.KillToken;
   if tok.TestName and TokenEquals(tok, 'distinct') then
   begin
      from := FQueryBuilder.Distinct(from);
      tok.KillToken;
   end;

   if tok.TestDelete(ttTIMES) then
      Exit;

   list := TSqlList.Create;
   repeat
      list.Add(ReadRenamableSqlIdentifier(compiler, tok, true));
   until not tok.TestDelete(ttCOMMA);
   from := FQueryBuilder.Select(from, list);
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

procedure TdwsLinqExtension.ReadJoinExpr(const compiler: IdwsCompiler; tok: TTokenizer; var from: TTypedExpr);
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
   JoinExpr := ReadExpression(compiler, tok);
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
   except
      list.Free;
      joinExpr.Free;
      raise;
   end;
   join := TSqlJoinExpr.Create(pos, jt, TSqlIdentifier(joinExpr), list);
   from := FQueryBuilder.Join(from, join)
end;


procedure TdwsLinqExtension.ReadOrderExprs(const compiler: IdwsCompiler; tok: TTokenizer; var from: TTypedExpr);
var
   ident: TSqlIdentifier;
   list: TSqlList;
begin
   tok.KillToken;
   if not (tok.TestName and TokenEquals(tok, 'by')) then
      Error(compiler, 'Invalid ORDER BY clause');
   tok.KillToken;

   list := TSqlList.Create;
   repeat
      ident := ReadSqlIdentifier(compiler, tok);
      if tok.TestName and (TokenEquals(tok, 'asc') or TokenEquals(tok, 'desc')) then
      begin
         ident.Value := format('%s %s', [ident.Value, tok.GetToken.AsString]);
         tok.KillToken;
      end;
      list.Add(ident);
   until not tok.TestDelete(ttCOMMA);
   from := FQueryBuilder.Order(from, list);
end;

function TdwsLinqExtension.IsLinqKeyword(tok: TTokenizer): boolean;
const WORDS: array [1..11] of string = ('from', 'on', 'join', 'left', 'right', 'cross', 'where', 'order', 'select', 'into', 'group');
var
   keyword: string;
begin
   result := false;
   for keyword in WORDS do
      result := result or TokenEquals(tok, keyword)
end;

function TdwsLinqExtension.ReadRenamableSqlIdentifier(
  const compiler: IdwsCompiler; tok: TTokenizer;
  acceptStar: boolean): TSqlIdentifier;
begin
   result := ReadSqlIdentifier(compiler, tok, acceptStar);
   if tok.TestDelete(ttAs) or (tok.TestName and not IsLinqKeyword(tok)) then
   begin
      if not tok.TestName then
         Error(compiler, 'Identifier expected');
      result.FRename := tok.GetToken.AsString;
      tok.KillToken;
   end;
end;

procedure TdwsLinqExtension.ReadGroupByExprs(const compiler: IdwsCompiler; tok: TTokenizer; var from: TTypedExpr);
var
   list: TSqlList;
begin
   tok.KillToken;
   if not (tok.TestName and TokenEquals(tok, 'by')) then
      Error(compiler, 'Invalid GROUP BY clause');
   tok.KillToken;

   list := TSqlList.Create;
   repeat
      list.Add(ReadSqlIdentifier(compiler, tok));
   until not tok.TestDelete(ttCOMMA);
   from := FQueryBuilder.Group(from, list);
end;

procedure TdwsLinqExtension.ReadFromExprBody(const compiler: IdwsCompiler; tok: TTokenizer;
  var from: TTypedExpr);
begin
   while TokenEquals(tok, 'join') or TokenEquals(tok, 'left') or
      TokenEquals(tok, 'right') or TokenEquals(tok, 'full') or TokenEquals(tok, 'cross') do
      ReadJoinExpr(compiler, tok, from);
   if TokenEquals(tok, 'where') then
      ReadWhereExprs(compiler, tok, from);
   if TokenEquals(tok, 'group') then
      ReadGroupByExprs(compiler, tok, from);
   if TokenEquals(tok, 'order') then
      ReadOrderExprs(compiler, tok, from);
   if TokenEquals(tok, 'select') then
      ReadSelectExprs(compiler, tok, from);
end;

function TdwsLinqExtension.DoReadFromExpr(const compiler: IdwsCompiler; tok: TTokenizer;
  db: TDataSymbol): TTypedExpr;
var
   id: TTypedExpr;
begin
   id := self.ReadExpression(compiler, tok);
   if FQueryBuilder.NeedsDot and not (id is TSqlIdentifier) then
      Error(compiler, 'SQL identifier expected');
   result := FQueryBuilder.From(TSqlIdentifier(id), db);
   try
      case tok.TestAny([ttSEMI, ttNAME]) of
         ttSemi: ;
         ttName: ReadFromExprBody(compiler, tok, result);
         else Error(compiler, 'Linq keyword expected');
      end;
   except
      result.Free;
      raise;
   end;
end;

function TdwsLinqExtension.findQueryBuilder(const compiler: IdwsCompiler; typ: TTypeSymbol;
  out builder: ILinqQueryBuilder): boolean;
var
   factory: TLinqQueryBuilderFactory;
   i: integer;
begin
   builder := nil;
   for i := 0 to self.FQueryBuilders.Count - 1 do
   begin
      factory := FQueryBuilders[i];
      builder := factory(compiler.Compiler, typ);
      if assigned(builder) then
         Exit(true);
   end;
   result := false;
end;

function TdwsLinqExtension.ReadFromExpression(const compiler: IdwsCompiler; tok : TTokenizer): TTypedExpr;
var
   symbol: TSymbol;
begin
   result := nil;
   if not (tok.TestName and TokenEquals(tok, 'from')) then Exit;
   try
      inc(FRecursionDepth);
      try
         tok.KillToken;
         symbol := nil;
         if tok.TestName then
         begin
            symbol := compiler.CurrentProg.Table.FindSymbol(tok.GetToken.AsString, cvMagic);
            if not ((symbol is TDataSymbol) and findQueryBuilder(compiler, symbol.typ, FQueryBuilder)) then
               Error(compiler, '"%s" is not a valid dwsLinq data source', [tok.GetToken.AsString]);
         end
         else Error(compiler, 'Identifier expected.');
         if FQueryBuilder.NeedsDot then
         begin
            tok.KillToken;
            if not tok.TestDelete(ttDOT) then
               Error(compiler, '"." expected.');
            if not tok.Test(ttNAME) then
               Error(compiler, '"Identifier expected.');
         end;
         result := DoReadFromExpr(compiler, tok, TDataSymbol(symbol));
      except
         on EAbort do
            compiler.Msgs.AddCompilerStop(tok.GetToken.FScriptPos, 'Invalid LINQ expression');
      end;
   finally
      dec(FRecursionDepth);
   end;
end;

function TdwsLinqExtension.ReadSqlFunction(const compiler: IdwsCompiler; tok : TTokenizer;
   base: TSqlIdentifier): TSqlFunction;
var
   arg: TTypedExpr;
begin
   result := TSqlFunction.Create(base, compiler);
   base.Free;
   if tok.TestDelete(ttBRIGHT) then
      Exit;
   repeat
      arg := ReadExpression(compiler, tok);
      result.FFunction.AddArg(arg);
      if not (arg is TSqlIdentifier) then
         arg.IncRefCount;
   until not tok.TestDelete(ttCOMMA);
   if not tok.TestDelete(ttBRIGHT) then
      Error(compiler, 'Close parenthesis expected.');
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
      end
      else if tok.TestDelete(ttBLEFT) then
         result := ReadSqlFunction(compiler, tok, result);
   except
      result.Free;
      raise;
   end;
end;

function TdwsLinqExtension.ValidIntoExpr(from, target: TTypedExpr): boolean;
var
   base: TFuncSymbol;
begin
   result := false;
   if not (target is TFuncRefExpr) then
      Exit;
   base := TFuncRefExpr(target).FuncExpr.FuncSym;
   if base.Params.Count <> 1 then
      Exit;
   if base.GetParamType(0) <> from.Typ then
      Exit;
   result := true;
end;

function TypeSymbol(const compiler: IdwsCompiler; base: TTypedExpr): TFuncSymbol;
begin
   result := TFuncSymbol.Create('', fkMethod, 0);
   result.Typ := compiler.CompilerContext.TypAnyType;
   result.AddParam(TParamSymbol.Create('', base.Typ));
end;

function TdwsLinqExtension.ReadIntoExpression(const compiler: IdwsCompiler;
  tok : TTokenizer; base: TTypedExpr): TTypedExpr;
var
   target: TTypedExpr;
   targetSymbol: TFuncSymbol;
   targetFunc: TFuncPtrExpr;
   aPos: TScriptPos;
begin
   try
      try
         aPos := tok.CurrentPos;
         tok.KillToken;
         targetSymbol := TypeSymbol(compiler, base);
         try
            target := compiler.ReadExpr(targetSymbol);
         finally
            targetSymbol.Free;
         end;
         if not ValidIntoExpr(base, target) then
         begin
            target.Free;
            Error(compiler, 'Into expression must be a valid function reference.');
         end;
         targetFunc := TFuncPtrExpr.Create(compiler.CompilerContext, aPos, target);
         result := FQueryBuilder.Into(base, targetFunc, aPos);
      except
         base.Free;
         raise;
      end;
   except
      on EAbort do
      begin
         compiler.Msgs.AddCompilerStop(tok.GetToken.FScriptPos, 'Invalid LINQ expression');
         result := nil;
      end;
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
      FQueryBuilder.Finalize(result);
      if TokenEquals(tok, 'into') then
         result := ReadIntoExpression(compiler, tok, result);
   end
   else result := ReadRenamableSqlIdentifier(compiler, tok);
end;

procedure TdwsLinqExtension.ReadScript(compiler: TdwsCompiler;
  sourceFile: TSourceFile; scriptType: TScriptSourceType);
begin
   inherited;
end;

function TdwsLinqExtension.TokenEquals(tok: TTokenizer; const value: string): boolean;
begin
   result := UnicodeSameText(tok.GetToken.AsString, value);
end;

{ TSqlIdentifier }

constructor TSqlIdentifier.Create(const name: string; const compiler: IdwsCompiler);
begin
   inherited Create(compiler.CompilerContext.TypVariant, name);
end;


function TSqlIdentifier.GetValue(params: TArrayConstantExpr; prog: TdwsProgram; newParam: TNewParamFunc): string;
begin
   result := self.Value;
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

{ TSqlFunction }

constructor TSqlFunction.Create(base: TsqlIdentifier; const compiler: IdwsCompiler);
begin
   inherited Create(base.Value, compiler);
   FFunction := TFuncExpr.Create(compiler.CompilerContext, compiler.Tokenizer.CurrentPos, nil);
end;

destructor TSqlFunction.Destroy;
begin
  FFunction.Free;
  inherited Destroy;
end;

function TSqlFunction.GetValue(params: TArrayConstantExpr; prog: TdwsProgram; newParam: TNewParamFunc): string;
var
   sl: TStringList;
   i: integer;
   arg: TExprBase;
begin
   sl := TStringList.Create;
   try
      for i := 0 to FFunction.Args.Count - 1 do
      begin
         arg := FFunction.Args[i];
         if arg is TsqlIdentifier then
            sl.Add(TsqlIdentifier(arg).GetValue(params, prog, newParam))
         else begin
            sl.Add(newParam());
            params.AddElementExpr(cNullPos, prog.Root.CompilerContext, arg as TTypedExpr);
         end;
      end;
      result := self.Value + '(' + sl.CommaText + ')';
   finally
      sl.Free;
   end;
end;

function GetOp(expr: TRelOpExpr): TRelOp;
begin
   if (expr.ClassType = TRelEqualVariantExpr) or (expr.ClassType = TRelEqualStringExpr) then
      result := roEq
   else if expr.ClassType = TRelNotEqualVariantExpr then
      result := roNeq
   else if expr.ClassType = TRelLessVariantExpr then
      result := roLt
   else if expr.ClassType = TRelLessEqualVariantExpr then
      result := roLte
   else if expr.ClassType = TRelGreaterVariantExpr then
      result := roGt
   else if expr.ClassType = TRelGreaterEqualVariantExpr then
      result := roGte
   else if expr.classType = TRelGreaterEqualIntExpr then
      result := roGte
   else if expr.classType = TSqlInExpr then
      result := roIn
   else raise Exception.CreateFmt('Unknown op type: %s.', [expr.ClassName]);
end;

{ TSqlInExpr }

procedure TSqlInExpr.EvalAsString(exec: TdwsExecution; var Result: UnicodeString);
begin
//
end;

end.
