unit dwsLinqSql;

interface
uses
   Classes,
   dwsLinq, dwsExprs, dwsXPlatform, dwsSymbols, dwsDataContext, dwsUtils,
   dwsConstExprs, dwsMethodExprs, dwsScriptSource,
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
      FListParams: TStringList;
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
      function GetIdentifierName(ident: TSqlIdentifier;
        prog: TdwsProgram): string;

      function EvalDynamic(exec : TdwsExecution) : Variant;
      function Interpolate(exec: TdwsExecution; list: TObjectVarExpr; params: TArrayConstantexpr;
        const param, query: string): string;
      procedure BuildDynamicQuery(prog: TdwsProgram; const query: string;
        params: TArrayConstantExpr);
   public
      constructor Create(tableName: TSqlIdentifier; const symbol: TDataSymbol);
      destructor Destroy; override;
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
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
   protected
      FStep: TMethodStaticExpr;
   public
      constructor Create(base: TSqlFromExpr; into: TFuncPtrExpr;
        const compiler: IdwsCompiler; const aPos: TScriptPos; dsSymbol: TClassSymbol);
      destructor Destroy; override;
   end;

   TLinqIntoSingleValExpr = class(TLinqIntoSingleExpr)
   public
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   TLinqIntoSingleProcExpr = class(TLinqIntoSingleExpr)
   public
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   TLinqIntoSetExpr = class(TLinqIntoExpr)
   public
      constructor Create(base: TSqlFromExpr; into: TFuncPtrExpr;
        const compiler: IdwsCompiler; const aPos: TScriptPos);
   end;

   TLinqIntoSetValExpr = class(TLinqIntoSetExpr)
   public
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

implementation
uses
   Sysutils,
   dwsExprList, dwsConvExprs, dwsUnitSymbols;

{ TSqlFromExpr }

constructor TSqlFromExpr.Create(tableName: TSqlIdentifier; const symbol: TDataSymbol);
begin
   inherited Create;
   FTableName := tableName;
   FDBSymbol := symbol;
   FListParams := TStringList.Create;
end;

destructor TSqlFromExpr.Destroy;
begin
   FListParams.Free;
   FTableName.Free;
   FMethod.Free;
   FWhereList.Free;
   FSelectList.Free;
   FJoinList.Free;
   FOrderList.Free;
   FGroupList.Free;
   FParams.Free;
   inherited Destroy;
end;

function TSqlFromExpr.GetIdentifierName(ident: TSqlIdentifier; prog: TdwsProgram): string;
begin
   result := ident.GetValue(FParams, prog, self.NewParam);
   if ident.rename <> '' then
      result := format('%s as %s', [result, ident.rename]);
end;

procedure TSqlFromExpr.WriteCommaList(exprs: TSqlList; list: TStringList; prog: TdwsProgram);
var
   i: integer;
   item: string;
begin
   for i := 0 to exprs.Count - 1 do
   begin
      item := GetIdentifierName(exprs[i] as TSqlIdentifier, prog);
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
      FParams.AddElementExpr(cNullPos, compiler.CompilerContext, expr);
      expr.IncRefCount;
      if expr.typ.classtype = TDynamicArraySymbol then
         FListParams.Add(result)
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
      joinLine := JOINS[join.JoinType] + 'join ' + GetIdentifierName(join.JoinExpr, compiler.CurrentProg);
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
   base: TVarExpr;
   pos: TScriptPos;
   arr: TTypedExpr;
begin
   query := (FDBSymbol.Typ as TClassSymbol).Members.FindSymbol('query', cvMagic) as TMethodSymbol;
   pos := compiler.Tokenizer.CurrentPos;
   base := TVarExpr.CreateTyped(compiler.CompilerContext, FDBSymbol);

   FParams := TArrayConstantExpr.Create(compiler.CompilerContext, pos);
   FParams.IncRefCount;
   BuildQuery(compiler);

   FMethod := TMethodStaticExpr.Create(compiler.CompilerContext, pos, query, base);
   FMethod.AddArg(TConstStringExpr.Create(compiler.CompilerContext.TypString, FSql));
   arr := TConvStaticArrayToDynamicExpr.Create(compiler.CompilerContext, FParams, TDynamicArraySymbol(query.Params.Symbols[1].Typ));
   FMethod.AddArg(arr);
   FMethod.Initialize(compiler.CompilerContext);
end;

function TSqlFromExpr.Interpolate(exec: TdwsExecution; list: TObjectVarExpr; params: TArrayConstantexpr;
  const param, query: string): string;
var
   i, high: integer;
   paramList: TStringList;
   obj: IScriptDynArray;
   prog: TdwsProgram;
begin
   list.EvalAsScriptDynArray(exec, obj);
   high := obj.ArrayLength - 1;
   prog := TdwsProgramExecution(exec).Prog;
   paramList := TStringList.Create;
   try
      for i := 0 to High do
      begin
         paramList.Add(':a' + intToStr(i));
         params.AddElementExpr(cNullPos, prog.Root.CompilerContext,
                               TConstExpr.Create(prog.Root.CompilerContext.TypVariant, obj.AsVariant[i]));
      end;
      result := StringReplace(query, param, format('(%s)', [paramList.CommaText]), []);
   finally
      paramList.Free;
   end;
end;

procedure TSqlFromExpr.BuildDynamicQuery(prog: TdwsProgram; const query: string; params: TArrayConstantExpr);
var
   method: TMethodStaticExpr;
   arr: TConvStaticArrayToDynamicExpr;
begin
   method := TMethodStaticExpr.Create(prog.Root.CompilerContext, FMethod.ScriptPos, FMethod.FuncSym as TMethodSymbol, FMethod.BaseExpr);
   method.BaseExpr.IncRefCount;
   method.AddArg(TConstStringExpr.Create(prog.Root.CompilerContext.TypString, query));
   arr := TConvStaticArrayToDynamicExpr.Create(prog.Root.CompilerContext, params,
     TDynamicArraySymbol(method.FuncSym.Params.Symbols[1].Typ));
   method.AddArg(arr);
   method.Initialize(prog.Root.CompilerContext);
   FMethod.free;
   FMethod := method;
end;

function TSqlFromExpr.EvalDynamic(exec: TdwsExecution): Variant;
var
   param, query: string;
   counter, index: integer;
   params: TArrayConstantExpr;
   prog: TdwsProgram;
   sub: TTypedExpr;
begin
   prog := (exec as TdwsProgramExecution).Prog;
   query := FSql;
   params := TArrayConstantExpr.Create(prog.Root.CompilerContext, FParams.ScriptPos);
   try
      counter := 0;
      for param in FListParams do
      begin
         index := StrToint(copy(param, 3)) - 1;
         while counter < index do
         begin
            sub := FParams.SubExpr[counter] as TTypedExpr;
            params.AddElementExpr(cNullPos, prog.Root.CompilerContext, sub);
            sub.IncRefCount;
            inc(counter);
         end;
         query := interpolate(exec, FParams.SubExpr[index] as TObjectVarExpr, params, param, query);
      end;
      BuildDynamicQuery(prog, query, params);
   except
      params.Free;
      raise;
   end;
   FMethod.EvalAsVariant(exec, result);
end;

// EvalAsVariant
//
procedure TSqlFromExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   if FListParams.Count > 0 then
      result := EvalDynamic(exec)
   else FMethod.EvalAsVariant(exec, result);
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
   dsVar := TObjectVarExpr.Create(FData);
   FBase.IncRefCount;
   FAssign := TAssignExpr.Create(prog.Root.CompilerContext, aPos, dsVar, FBase);
   dsVar.IncRefCount;
   FInto.AddArg(dsVar);
   FInto.Initialize(prog.Root.CompilerContext);
   freeSym := (prog.Table.FindTypeSymbol('TObject', cvMagic) as TClassSymbol).Members.FindSymbol('Free', cvMagic) as TMethodSymbol;
   dsVar.IncRefCount;
   FFree := TMethodStaticExpr.Create(prog.Root.CompilerContext, aPos, freeSym, dsVar);
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
   if assigned(into.FuncSym.Result) then
      self.Typ := TDynamicArraySymbol.Create('', into.FuncSym.Result.Typ, compiler.CompilerContext.TypInteger);
   FAssign.Left.IncRefCount;
   FStep := TMethodStaticExpr.Create(compiler.CompilerContext, aPos,
     dsSymbol.Members.FindSymbol('Step', cvMagic) as TMethodSymbol, FAssign.Left);
end;

destructor TLinqIntoSingleExpr.Destroy;
begin
   FStep.Free;
   inherited Destroy;
end;

{ TLinqIntoSingleValExpr }

// EvalAsVariant
//
procedure TLinqIntoSingleValExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
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
      FInto.EvalAsVariant(exec, dyn.AsPVariant(n)^);
      inc(n);
   end;
   result := IScriptDynArray(dyn);
   FFree.EvalNoResult(exec);
end;

{ TLinqIntoSingleProcExpr }

// EvalAsVariant
//
procedure TLinqIntoSingleProcExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   FAssign.EvalNoResult(exec);
   while FStep.EvalAsBoolean(exec) do
      FInto.EvalNoResult(exec);
   FFree.EvalNoResult(exec);
end;

{ TLinqIntoSetExpr }

constructor TLinqIntoSetExpr.Create(base: TSqlFromExpr; into: TFuncPtrExpr;
  const compiler: IdwsCompiler; const aPos: TScriptPos);
begin
   inherited Create(base, into, compiler, aPos);
   self.Typ := into.FuncSym.Result.Typ;
   self.Typ.IncRefCount;
end;

{ TLinqIntoSetValExpr }

// EvalAsVariant
//
procedure TLinqIntoSetValExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   FAssign.EvalNoResult(exec);
   FInto.EvalAsVariant(exec, result);
   FFree.EvalNoResult(exec);
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
   if targetFunc.FuncSym.Typ = nil then
      result := TLinqIntoSingleProcExpr.Create(from, targetFunc, FCompiler,
                aPos, from.Typ as TClassSymbol)
   else if targetFunc.FuncSym.Typ is TArraySymbol then
      result := TLinqIntoSetValExpr.Create(from, targetFunc, FCompiler, aPos)
   else result := TLinqIntoSingleValExpr.Create(from, targetFunc, FCompiler, aPos, from.Typ as TClassSymbol);
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
