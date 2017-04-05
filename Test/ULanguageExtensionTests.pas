unit ULanguageExtensionTests;

interface

uses
   Windows, Classes, SysUtils,
   dwsStack,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsDataContext,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils, Variants,
   dwsSymbols, dwsPascalTokenizer, dwsStrings, dwsJSON, dwsLanguageExtension;

type

   TLanguageExtensionTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure AutoExternalValues;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TAutoExternalValues = class (TdwsLanguageExtension)
      public
         function FindUnknownName(compiler : TdwsCompiler; const name : UnicodeString) : TSymbol; override;
   end;

   TCallableAutoExternal = class(TdwsCallable)
      protected
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
   end;

   TAutoExternalValuesExtension = class (TdwsCustomLangageExtension)
      protected
         function CreateExtension : TdwsLanguageExtension; override;
   end;

// CreateExtension
//
function TAutoExternalValuesExtension.CreateExtension : TdwsLanguageExtension;
begin
   Result:=TAutoExternalValues.Create;
end;

// Call
//
procedure TCallableAutoExternal.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
begin
   exec.Stack.WriteStrValue_BaseRelative(func.Result.StackAddr, func.Name);
end;

// FindUnknownName
//
function TAutoExternalValues.FindUnknownName(compiler : TdwsCompiler; const name : UnicodeString) : TSymbol;
var
   externalSym : TExternalVarSymbol;
   readFunc : TFuncSymbol;
   call : TCallableAutoExternal;
begin
   externalSym:=TExternalVarSymbol.Create(name, compiler.CompilerContext.TypString);
   compiler.CurrentProg.Table.AddSymbol(externalSym);

   call:=TCallableAutoExternal.Create(nil);

   readFunc:=TFuncSymbol.Create('_get_'+name, fkFunction, 0);
   readFunc.Executable:=call;
   readFunc.Typ:=compiler.CompilerContext.TypString;

   call._Release;

   externalSym.ReadFunc:=readFunc;

   Result:=externalSym;
end;

// ------------------
// ------------------ TLanguageExtensionTests ------------------
// ------------------

// SetUp
//
procedure TLanguageExtensionTests.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TLanguageExtensionTests.TearDown;
begin
   FCompiler.Free;
end;

// AutoExternalValues
//
procedure TLanguageExtensionTests.AutoExternalValues;
var
   custom : TAutoExternalValuesExtension;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   custom:=TAutoExternalValuesExtension.Create(nil);
   try
      custom.Script:=FCompiler;
      prog:=FCompiler.Compile( 'PrintLn(Hello);'#13#10
                              +'PrintLn(World);'#13#10);
      CheckEquals('', prog.Msgs.AsInfo);
      exec:=prog.Execute;
      CheckEquals('_get_Hello'#13#10'_get_World'#13#10, exec.Result.ToUnicodeString);
   finally
      custom.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('LanguageExtensionTests', TLanguageExtensionTests);

end.
