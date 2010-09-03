unit UCornerCasesTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer;

type

   TCornerCasesTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure EmptyTokenBuffer;
         procedure IgnoreDecimalSeparator;
         procedure TimeOutTestFinite;
         procedure TimeOutTestInfinite;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   // TTokenBufferWrapper
   //
   TTokenBufferWrapper = class
      Buffer : TTokenBuffer;
   end;

// ------------------
// ------------------ TCornerCasesTests ------------------
// ------------------

// SetUp
//
procedure TCornerCasesTests.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TCornerCasesTests.TearDown;
begin
   FCompiler.Free;
end;

// EmptyTokenBuffer
//
procedure TCornerCasesTests.EmptyTokenBuffer;
var
   w : TTokenBufferWrapper;
   s : String;
begin
   w:=TTokenBufferWrapper.Create;
   try
      CheckEquals('', w.Buffer.ToStr, 'ToStr function');
      s:='dummy';
      w.Buffer.ToStr(s);
      CheckEquals('', s, 'ToStr procedure');
      s:='dummy';
      w.Buffer.ToUpperStr(s);
      CheckEquals('', s, 'ToUpperStr');
      CheckEquals(#0, w.Buffer.LastChar, 'LastChar');
   finally
      w.Free;
   end;
end;

// IgnoreDecimalSeparator
//
procedure TCornerCasesTests.IgnoreDecimalSeparator;
var
   w : TTokenBufferWrapper;
   dc : Char;
begin
   w:=TTokenBufferWrapper.Create;
   dc:=DecimalSeparator;
   try
      w.Buffer.AppendChar('1');
      w.Buffer.AppendChar('.');
      w.Buffer.AppendChar('5');

      DecimalSeparator:='.';
      CheckEquals(1.5, w.Buffer.ToFloat, 'With dot');
      DecimalSeparator:=',';
      CheckEquals(1.5, w.Buffer.ToFloat, 'With comma');
      DecimalSeparator:='P';
      CheckEquals(1.5, w.Buffer.ToFloat, 'With P');

   finally
      DecimalSeparator:=dc;
      w.Free;
   end;
end;

// TimeOutTestFinite
//
procedure TCornerCasesTests.TimeOutTestFinite;
var
   prog : TdwsProgram;
begin
   prog:=FCompiler.Compile('while false do;');
   try
      prog.TimeoutMilliseconds:=1000;
      prog.Execute;
   finally
      prog.Free;
   end;
end;

// TimeOutTestInfinite
//
procedure TCornerCasesTests.TimeOutTestInfinite;
var
   prog : TdwsProgram;
begin
   prog:=FCompiler.Compile('while true do;');
   try
      prog.TimeoutMilliseconds:=100;
      prog.Execute;
   finally
      prog.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('CornerCasesTests', TCornerCasesTests.Suite);

end.
