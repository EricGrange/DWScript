unit dwsLinqSql;

interface
uses
   Classes,
   dwsLinq, dwsExprs, dwsXPlatform, dwsSymbols, dwsDataContext,
   dwsConstExprs, dwsMethodExprs,
   dwsCompiler, dwsCoreExprs, dwsErrors, dwsRelExprs;

type
   TLinqSqlExtension = class(TComponent)
   private
      FLinqFactory: TdwsLinqFactory;
      procedure SetLinqFactory(const Value: TdwsLinqFactory);
   public
      property LinqFactory: TdwsLinqFactory read FLinqFactory write SetLinqFactory;
   end;

   TLinqSqlFactory = class(TInterfacedObject, ILinqQueryBuilder)
   private
      FCompiler: TdwsCompiler;
      FDatabaseSymbol: TClassSymbol;
      FDatasetSymbol: TClassSymbol;

      function From(value: TTypedExpr; base: TDataSymbol): TTypedExpr;
      function Join(base: TTypedExpr; value: TSqlJoinExpr): TTypedExpr;
      function Where(from: TTypedExpr; list: TSqlList): TTypedExpr;
      function Group(from: TTypedExpr; list: TSqlList): TTypedExpr;
      function Order(from: TTypedExpr; list: TSqlList): TTypedExpr;
      function Select(from: TTypedExpr; list: TSqlList): TTypedExpr;
      function Into(base: TTypedExpr; targetFunc: TFuncPtrExpr; aPos: TScriptPos): TTypedExpr;
      function Distinct(from: TTypedExpr): TTypedExpr;
      procedure Finalize(From: TTypedExpr);
      function NeedsDot: boolean;
   public
      constructor Create(compiler: TdwsCompiler);
   end;

   TSqlFromExpr = class(TTypedExpr)
   private
      FTableName: TSqlIdentifier;
      FDBSymbol: TDataSymbol;
      FWhereList: TSqlList;
      FSelectList: TSqlList;
      FJoinList: TSqlList;
      FOrderList: TSqlList;
      FGroupList: TSqlList;
      FDistinct: boolean;
   private
      FSQL: string;
      FParams: TArrayConstantExpr;
      FMethod: TMethodExpr;
      function NewParam: string;
      procedure BuildSelectList(list: TStringList; prog: TdwsProgram);
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
      procedure BuildOrderClause(list: TStringList; prog: TdwsProgram);
      procedure WriteCommaList(exprs: TSqlList; list: TStringList; prog: TdwsProgram);
      procedure BuildGroupClause(list: TStringList; prog: TdwsProgram);
   public
      constructor Create(tableName: TSqlIdentifier; const symbol: TDataSymbol);
      destructor Destroy; override;
      function Eval(exec : TdwsExecution) : Variant; override;
   end;

   TLinqIntoExpr = class(TTypedExpr)
   private
      FBase: TSqlFromExpr;
      FInto: TFuncPtrExpr;
      FAssign: TAssignExpr;
      FData: TDataSymbol;
      FFree: TMethodStaticExpr;
   public
      constructor Create(base: TSqlFromExpr; into: TFuncPtrExpr;
        const compiler: IdwsCompiler; const aPos: TScriptPos);
      destructor Destroy; override;
   end;

   TLinqIntoSingleExpr = class(TLinqIntoExpr)
   private
      FStep: TMethodStaticExpr;
   public
      constructor Create(base: TSqlFromExpr; into: TFuncPtrExpr;
        const compiler: IdwsCompiler; const aPos: TScriptPos; dsSymbol: TClassSymbol);
      destructor Destroy; override;
      function Eval(exec : TdwsExecution) : Variant; override;
   end;

   TLinqIntoSetExpr = class(TLinqIntoExpr)
   public
      constructor Create(base: TSqlFromExpr; into: TFuncPtrExpr;
        const compiler: IdwsCompiler; const aPos: TScriptPos);
      function  Eval(exec : TdwsExecution) : Variant; override;
   end;

implementation
uses
   Sysutils,
   dwsUtils, dwsExprList, dwsConvExprs, dwsUnitSymbols;

{ TSqlFromExpr }

constructor TSqlFromExpr.Create(tableName: TSqlIdentifier; const symbol: TDataSymbol);
begin
   inherited Create;
   FTableName := tableName;
   FDBSymbol := symbol;
end;

destructor TSqlFromExpr.Destroy;
begin
   FTableName.Free;
   FMethod.Free;
   FWhereList.Free;
   FSelectList.Free;
   FJoinList.Free;
   FOrderList.Free;
   FGroupList.Free;
   inherited Destroy;
end;

procedure TSqlFromExpr.WriteCommaList(exprs: TSqlList; list: TStringList; prog: TdwsProgram);
var
   i: integer;
   item: string;
begin
   for i := 0 to exprs.Count - 1 do
   begin
      item := (exprs[i] as TSqlIdentifier).GetValue(FParams, prog, self.NewParam);
      if i < exprs.Count - 1 then
         item := item + ',';
      list.Add(item)
   end;
end;

procedure TSqlFromExpr.BuildSelectList(list: TStringList; prog: TdwsProgram);
begin
   if FDistinct then
      list.Add('select distinct')
   else list.Add('select');

   WriteCommaList(FSelectList, list, prog);
end;

function GetOpText(expr: TRelOpExpr): string;
begin
   case GetOp(expr) of
      roEq: result := '=';
      roNeq: result := '<>';
      roLt: result := '<';
      roLte: result := '<=';
      roGt: result := '>';
      roGte: result := '>=';
      roIn: result := 'in';
      roNin: result := 'not in';
   end;
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
   list.Add(format('%s %s %s', [l, GetOpText(expr), r]));
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
      joinLine := JOINS[join.JoinType] + 'join ' + join.JoinExpr.Value;
      if assigned(join.Criteria) then
         joinLine := joinLine + ' on';
      list.Add(joinLine);
      if assigned(join.Criteria) then
         BuildConditional(join.Criteria, compiler, list);
   end;
end;

procedure TSqlFromExpr.BuildOrderClause(list: TStringList; prog: TdwsProgram);
begin
   list.Add('order by');
   WriteCommaList(FOrderList, list, prog);
end;

procedure TSqlFromExpr.BuildGroupClause(list: TStringList; prog: TdwsProgram);
begin
   list.Add('group by');
   WriteCommaList(FGroupList, list, prog);
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
      else BuildSelectList(list, compiler.CurrentProg);
      list.Add('from ' + FTableName.GetValue(FParams, compiler.CurrentProg, self.NewParam));
      if assigned(FJoinList) then
         BuildJoinClause(compiler, list);
      if assigned(FWhereList) then
         BuildWhereClause(compiler, list);
      if assigned(FGroupList) then
         BuildGroupClause(list, compiler.CurrentProg);
      if assigned(FOrderList) then
         BuildOrderClause(list, compiler.CurrentProg);
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

{ TLinqIntoExpr }

constructor TLinqIntoExpr.Create(base: TSqlFromExpr; into: TFuncPtrExpr;
  const compiler: IdwsCompiler; const aPos: TScriptPos);
var
   dsVar: TObjectVarExpr;
   prog: TdwsProgram;
   freeSym: TMethodSymbol;
begin
   inherited Create;
   FBase := base;
   FInto := into;
   prog := compiler.CurrentProg;
   FData := TDataSymbol.Create('', FBase.Typ);
   FData.AllocateStackAddr(prog.Table.AddrGenerator);
   dsVar := TObjectVarExpr.Create(prog, FData);
   FBase.FMethod.IncRefCount;
   FAssign := TAssignExpr.Create(prog, aPos, dsVar, FBase.FMethod);
   dsVar.IncRefCount;
   FInto.AddArg(dsVar);
   FInto.Initialize(prog);
   freeSym := (prog.Table.FindTypeSymbol('TObject', cvMagic) as TClassSymbol).Members.FindSymbol('Free', cvMagic) as TMethodSymbol;
   dsVar.IncRefCount;
   FFree := TMethodStaticExpr.Create(prog, aPos, freeSym, dsVar);
end;

destructor TLinqIntoExpr.Destroy;
begin
   FBase.Free;
   FInto.Free;
   FTyp.Free;
   FAssign.Free;
   FData.Free;
   FFree.Free;
   inherited;
end;

{ TLinqIntoSingleExpr }

constructor TLinqIntoSingleExpr.Create(base: TSqlFromExpr; into: TFuncPtrExpr;
  const compiler: IdwsCompiler; const aPos: TScriptPos; dsSymbol: TClassSymbol);
begin
   inherited Create(base, into, compiler, aPos);
   self.Typ := TDynamicArraySymbol.Create('', into.FuncSym.Result.Typ, compiler.CurrentProg.TypInteger);
   FAssign.Left.IncRefCount;
   FStep := TMethodStaticExpr.Create(compiler.CurrentProg, aPos,
     dsSymbol.Members.FindSymbol('Step', cvMagic) as TMethodSymbol, FAssign.Left);
end;

destructor TLinqIntoSingleExpr.Destroy;
begin
   FStep.Free;
   inherited Destroy;
end;

function TLinqIntoSingleExpr.Eval(exec: TdwsExecution): Variant;
var
   dyn: TScriptDynamicArray;
   n: integer;
begin
   FAssign.EvalNoResult(exec);
   dyn := TScriptDynamicArray.CreateNew(FTyp);
   n := 0;

   while FStep.EvalAsBoolean(exec) do
   begin
      dyn.ArrayLength := n + 1;
      dyn.AsPVariant(n)^ := FInto.Eval(exec);
      inc(n);
   end;
   result := IScriptObj(dyn);
   FFree.Eval(exec);
end;

{ TLinqIntoSetExpr }

constructor TLinqIntoSetExpr.Create(base: TSqlFromExpr; into: TFuncPtrExpr;
  const compiler: IdwsCompiler; const aPos: TScriptPos);
begin
   inherited Create(base, into, compiler, aPos);
   self.Typ := into.FuncSym.Result.Typ;
   self.Typ.IncRefCount;
end;

function TLinqIntoSetExpr.Eval(exec: TdwsExecution): Variant;
begin
   FAssign.EvalNoResult(exec);
   FInto.EvalAsVariant(exec, result);
   FFree.Eval(exec);
end;

{ TLinqSqlFactory }

constructor TLinqSqlFactory.Create(compiler: TdwsCompiler);
var
   db: TUnitMainSymbol;
begin
   inherited create;
   FCompiler := compiler;

   db := compiler.CurrentProg.UnitMains.Find('System.Data');
   if assigned(db) then
   begin
      FDatabaseSymbol := db.Table.FindTypeLocal('Database') as TClassSymbol;
      FDatasetSymbol := db.Table.FindTypeLocal('Dataset') as TClassSymbol;
   end;
   if FDatabaseSymbol = nil then
      TdwsLinqExtension.Error(compiler, 'Database type not found in script');
   if FDatasetSymbol = nil then
      TdwsLinqExtension.Error(compiler, 'Dataset type not found in script');
end;

function TLinqSqlFactory.From(value: TTypedExpr; base: TDataSymbol): TTypedExpr;
begin
   result := TSqlFromExpr.Create(TSqlIdentifier(value), base);
   result.Typ := FDatasetSymbol;
end;

function TLinqSqlFactory.Group(from: TTypedExpr; list: TSqlList): TTypedExpr;
begin
   assert((from as TSqlFromExpr).FGroupList = nil);
   TSqlFromExpr(from).FGroupList := list;
   result := from;
end;

function TLinqSqlFactory.Into(base: TTypedExpr; targetFunc: TFuncPtrExpr; aPos: TScriptPos): TTypedExpr;
var
   from: TSqlFromExpr;
begin
   from := base as TSqlFromExpr;
   if targetFunc.FuncSym.Typ is TArraySymbol then
      result := TLinqIntoSetExpr.Create(from, targetFunc, FCompiler, aPos)
   else result := TLinqIntoSingleExpr.Create(from, targetFunc, FCompiler, aPos, from.Typ as TClassSymbol);
end;

function TLinqSqlFactory.Join(base: TTypedExpr; value: TSqlJoinExpr): TTypedExpr;
var
   from: TSqlFromExpr;
begin
   from := base as TSqlFromExpr;
   if from.FJoinList = nil then
      from.FJoinList := TSqlList.Create;
   from.FJoinList.Add(value);
   result := from;
end;

function TLinqSqlFactory.NeedsDot: boolean;
begin
   result := true;
end;

function TLinqSqlFactory.Distinct(from: TTypedExpr): TTypedExpr;
begin
   (from as TSqlFromExpr).FDistinct := true;
   result := from;
end;

function TLinqSqlFactory.Order(from: TTypedExpr; list: TSqlList): TTypedExpr;
begin
   assert((from as TSqlFromExpr).FOrderList = nil);
   TSqlFromExpr(from).FOrderList := list;
   result := from;
end;

function TLinqSqlFactory.Select(from: TTypedExpr; list: TSqlList): TTypedExpr;
begin
   assert((from as TSqlFromExpr).FSelectList = nil);
   TSqlFromExpr(from).FSelectList := list;
   result := from;
end;

function TLinqSqlFactory.Where(from: TTypedExpr; list: TSqlList): TTypedExpr;
begin
   assert((from as TSqlFromExpr).FWhereList = nil);
   TSqlFromExpr(from).FWhereList := list;
   result := from;
end;

procedure TLinqSqlFactory.Finalize(From: TTypedExpr);
begin
   TSqlFromExpr(from).Codegen(FCompiler);
end;

function LinqSqlFactory(compiler: TdwsCompiler; symbol: TTypeSymbol): ILinqQueryBuilder;
var
   factory: TLinqSqlFactory;
begin
   factory := TLinqSqlFactory.Create(compiler);
   if symbol.IsCompatible(factory.FDatabaseSymbol) then
      result := factory
   else begin
      result := nil;
      factory.Free;
   end;
end;

{ TLinqSqlExtension }

procedure TLinqSqlExtension.SetLinqFactory(const Value: TdwsLinqFactory);
begin
   FLinqFactory := Value;
   if assigned(FLinqFactory) then
      FLinqFactory.RegisterSource(@LinqSqlFactory);
end;

end.
