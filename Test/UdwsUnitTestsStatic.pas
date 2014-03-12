unit UdwsUnitTestsStatic;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsSymbols, dwsUtils, dwsStack, UdwsUnitTests;

type

   TdwsUnitTestsStatic = class (TdwsUnitTests)
      private
         class var vContext : TdwsUnitTestsContext;

         procedure DoAfterInitUnitTable(Sender : TObject);

      public
         procedure SetUp; override;
         procedure TearDown; override;
   end;

implementation

// ------------------
// ------------------ TdwsUnitTestsStatic ------------------
// ------------------

// SetUp
//
procedure TdwsUnitTestsStatic.SetUp;
begin
   FContext:=vContext;

   inherited SetUp;

   vContext:=FContext;

   FUnit.StaticSymbols:=True;

   FUnit.OnAfterInitUnitTable:=DoAfterInitUnitTable;
end;

// TearDown
//
procedure TdwsUnitTestsStatic.TearDown;
begin
   FContext:=nil;

   inherited TearDown;
end;

// DoAfterInitUnitTable
//
procedure TdwsUnitTestsStatic.DoAfterInitUnitTable(Sender : TObject);
begin
   FUnit.Tag:=FUnit.Tag+1;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('dwsUnit', TdwsUnitTestsStatic);

finalization

   TdwsUnitTestsStatic.vContext.Free;

end.
