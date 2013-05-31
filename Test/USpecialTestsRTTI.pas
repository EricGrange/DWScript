unit USpecialTestsRTTI;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsUtils,
   dwsXPlatform, dwsSymbols, dwsErrors, dwsRTTIExposer;

type

   TSpecialTestsRTTI = class (TTestCase)
      private

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure ExposeThreadTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TEnumeratorEmptyCallBack = class
      procedure EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
   end;

procedure TEnumeratorEmptyCallBack.EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
begin
   // just used for detecting crashes in subexpr tree navigation
end;

// ------------------
// ------------------ TSpecialTestsRTTI ------------------
// ------------------

// SetUp
//
procedure TSpecialTestsRTTI.SetUp;
begin
end;

// TearDown
//
procedure TSpecialTestsRTTI.TearDown;
begin
end;

// Expose Test
// http://stackoverflow.com/questions/16774354/dwscript-issue-updating-to-current-development-version

type

   TScriptApplication = class(TPersistent)
   end;

   TExposeTestClass = class(TThread)
      private
         FScript                  : IdwsProgram;
         FDelphiWebScript         : TDelphiWebScript;
         FUnit                    : TdwsUnit;
         FScriptApplication       : TScriptApplication;
         FSuccess                 : Boolean;
         FSuccess1                : Boolean;
         FSuccess2                : Boolean;
         procedure ExposeInstancesAfterInitTable(Sender: TObject);

      public
         constructor Create;
         destructor Destroy; override;
         procedure Execute; override;
   end;

constructor TExposeTestClass.Create;
begin
    inherited Create(TRUE);
    FScriptApplication              := TScriptApplication.Create;
    FDelphiWebScript                := TDelphiWebScript.Create(nil);
    FUnit                           := TdwsUnit.Create(nil);
    FUnit.UnitName                  := 'Test';
    FUnit.Script                    := FDelphiWebScript;
    FUnit.ExposeRTTI(TypeInfo(TScriptApplication), [eoNoFreeOnCleanup]);
    FUnit.OnAfterInitUnitTable      := ExposeInstancesAfterInitTable;
end;

destructor TExposeTestClass.Destroy;
begin
    FreeAndNil(FScriptApplication);
    FreeAndNil(FUnit);
    FreeAndNil(FDelphiWebScript);
    inherited;
end;

procedure TExposeTestClass.Execute;
begin
    FSuccess  := FALSE;
    FScript   := FDelphiWebScript.Compile('Unit Test; var I: Integer; I := 0;');
    FSuccess1 := FSuccess;
    FSuccess  := FALSE;
    FScript   := FDelphiWebScript.Compile('var I: Integer; I := 0;');
    FSuccess2 := FSuccess;
end;


procedure TExposeTestClass.ExposeInstancesAfterInitTable(Sender: TObject);
begin
    FUnit.ExposeInstanceToUnit('Application', 'TScriptApplication', FScriptApplication);
    FSuccess := TRUE;
end;

// ExposeThreadTest
//
procedure TSpecialTestsRTTI.ExposeThreadTest;
var
   Test : TExposeTestClass;
begin
   Test := TExposeTestClass.Create;
   try
      Test.Start;
      Test.WaitFor;
      CheckFalse(Test.FSuccess1, 'Test1');
      CheckTrue(Test.FSuccess2, 'Test2');
   finally
      Test.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('SpecialTests', TSpecialTestsRTTI);

end.
