unit UDebuggerTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsComConnector, Variants, ActiveX, ComObj, dwsXPlatform, dwsUtils,
   dwsSymbols, dwsDebugger;

type

   TDebuggerTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FUnits : TdwsUnit;
         FDebugger : TdwsDebugger;

         FDebugEvalAtLine : Integer;
         FDebugEvalExpr : String;
         FDebugLastEvalResult : String;

         procedure DoCreateExternal(Info: TProgramInfo; var ExtObject: TObject);
         procedure DoCleanupExternal(externalObject : TObject);
         procedure DoGetValue(Info: TProgramInfo; ExtObject: TObject);

         procedure DoDebugEval(exec: TdwsExecution; expr: TExprBase);

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure EvaluateSimpleTest;
         procedure EvaluateOutsideOfExec;
         procedure EvaluateContextTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TTestObject = class
      FField : String;
      constructor Create(const value : String);
   end;

// Create
//
constructor TTestObject.Create(const value : String);
begin
   FField:=value;
end;

// ------------------
// ------------------ TDebuggerTests ------------------
// ------------------

// SetUp
//
procedure TDebuggerTests.SetUp;
var
   cls : TdwsClass;
   cst : TdwsConstructor;
   meth : TdwsMethod;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
   FUnits:=TdwsUnit.Create(nil);
   FUnits.UnitName:='TestUnit';
   FUnits.Script:=FCompiler;
   FDebugger:=TdwsDebugger.Create(nil);
   FDebugger.OnDebug:=DoDebugEval;

   cls:=FUnits.Classes.Add;
   cls.Name:='TTestClass';
   cls.OnCleanUp:=DoCleanupExternal;
   cst:=cls.Constructors.Add as TdwsConstructor;
   cst.Name:='Create';
   cst.OnEval:=DoCreateExternal;

   meth:=cls.Methods.Add as TdwsMethod;
   meth.ResultType:='String';
   meth.Name:='GetValue';
   meth.OnEval:=DoGetValue;
end;

// TearDown
//
procedure TDebuggerTests.TearDown;
begin
   FDebugger.Free;
   FUnits.Free;
   FCompiler.Free;
end;

// DoCreateExternal
//
procedure TDebuggerTests.DoCreateExternal(Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=TTestObject.Create('Hello');
end;

// DoCleanupExternal
//
procedure TDebuggerTests.DoCleanupExternal(externalObject : TObject);
begin
   externalObject.Free;
end;

// DoGetValue
//
procedure TDebuggerTests.DoGetValue(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TTestObject).FField;
end;

// DoDebugEval
//
procedure TDebuggerTests.DoDebugEval(exec: TdwsExecution; expr: TExprBase);
begin
   if expr.ScriptPos.Line=FDebugEvalAtLine then
      FDebugLastEvalResult:=FDebugger.EvaluateAsString(FDebugEvalExpr);
end;

// EvaluateSimpleTest
//
procedure TDebuggerTests.EvaluateSimpleTest;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   expr : IdwsEvaluateExpr;
   buf : String;
begin
   prog:=FCompiler.Compile('var i := 10;');
   try
      exec:=prog.BeginNewExecution;
      try
         exec.RunProgram(0);

         CheckEquals(10, exec.Info.ValueAsInteger['i'], 'value of i');

         expr:=TdwsCompiler.Evaluate(exec, 'i+i*10');
         try
            CheckEquals(110, expr.Expression.EvalAsInteger(exec.ExecutionObject), 'i+i*10');
         finally
            expr:=nil;
         end;

         expr:=TdwsCompiler.Evaluate(exec, 'StrToInt(''123'')');
         try
            CheckEquals(123, expr.Expression.EvalAsInteger(exec.ExecutionObject), 'StrToInt(''123'')');
         finally
            expr:=nil;
         end;

         expr:=TdwsCompiler.Evaluate(exec, 'i +* i');
         try
            expr.Expression.EvalAsString(exec.ExecutionObject, buf);
            CheckEquals('Syntax Error: Expression expected [line: 1, column: 4]'#13#10, buf, 'i +* i');
         finally
            expr:=nil;
         end;

      finally
         exec.EndProgram;
         exec:=nil;
      end;
   finally
      prog:=nil;
   end;
end;

// EvaluateOutsideOfExec
//
procedure TDebuggerTests.EvaluateOutsideOfExec;
var
   expr : IdwsEvaluateExpr;
begin
   expr:=TdwsCompiler.Evaluate(nil, 'StrToInt(''113'')+10');
   try
      CheckEquals(123, expr.Expression.EvalAsInteger(expr.Execution.ExecutionObject), 'StrToInt(''113'')+10');
   finally
      expr:=nil;
   end;
end;

// EvaluateContextTest
//
procedure TDebuggerTests.EvaluateContextTest;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var i := 1;'#13#10
                           +'procedure Test;'#13#10
                           +'var i := 2;'#13#10
                           +'begin'#13#10
                              +'PrintLn(i);'#13#10    // line 5
                           +'end;'#13#10
                           +'Test;');                 // line 7

   try
      exec:=prog.CreateNewExecution;
      try
         FDebugEvalExpr:='i';

         FDebugEvalAtLine:=5;
         FDebugLastEvalResult:='';
         FDebugger.BeginDebug(exec);
         try
            CheckEquals('2', FDebugLastEvalResult, 'i at line 5');
         finally
            FDebugger.EndDebug;
         end;

         FDebugEvalAtLine:=7;
         FDebugLastEvalResult:='';
         FDebugger.BeginDebug(exec);
         try
            CheckEquals('1', FDebugLastEvalResult, 'i at line 7');
         finally
            FDebugger.EndDebug;
         end;
      finally
         exec:=nil;
      end;
   finally
      prog:=nil;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('DebuggerTests', TDebuggerTests.Suite);

end.
