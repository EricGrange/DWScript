unit UInstantiateTests;

interface

uses
   Windows, Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsDataContext,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils, Variants,
   dwsSymbols, dwsPascalTokenizer, dwsStrings, dwsJSON;

type

   TInstantiateTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FUnit : TdwsUnit;
         FFlag : Integer;
         FFlagHigh : Integer;
         FFlagNb : Integer;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoOnInstantiate(info: TProgramInfo; var ExtObject: TObject);
         procedure DoOnInstantiateNil(info: TProgramInfo; var ExtObject: TObject);

      published
         procedure Basic;
         procedure TwoExecs;
         procedure NilTest;
         procedure InfoAccess;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TTestObj = class
      FTests : TInstantiateTests;
      FValue : Integer;
      constructor Create(aTests : TInstantiateTests; aValue : Integer);
      destructor Destroy; override;
   end;

// Create
//
constructor TTestObj.Create(aTests : TInstantiateTests; aValue : Integer);
begin
   FTests:=aTests;
   FValue:=aValue;
end;

// Destroy
//
destructor TTestObj.Destroy;
begin
   inherited;
   Dec(FTests.FFlag);
end;

// ------------------
// ------------------ TInstantiateTests ------------------
// ------------------

// SetUp
//
procedure TInstantiateTests.SetUp;
var
   inst : TdwsInstance;
begin
   FCompiler:=TDelphiWebScript.Create(nil);

   FUnit:=TdwsUnit.Create(nil);
   FUnit.UnitName:='Instantiate';
   FUnit.Script:=FCompiler;

   inst:=FUnit.Instances.Add;
   inst.DataType:='TObject';
   inst.Name:='test';
   inst.OnInstantiate:=DoOnInstantiate;
   inst.AutoDestroyExternalObject:=True;

   inst:=FUnit.Instances.Add;
   inst.DataType:='TObject';
   inst.Name:='testNil';
   inst.OnInstantiate:=DoOnInstantiateNil;
end;

// TearDown
//
procedure TInstantiateTests.TearDown;
begin
   FUnit.Free;
   FCompiler.Free;
end;

// DoOnInstantiate
//
procedure TInstantiateTests.DoOnInstantiate(info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=TTestObj.Create(Self, FFlag);
   Inc(FFlag);
   Inc(FFlagNb);
   if FFlag>FFlagHigh then
      FFlagHigh:=FFlag;
end;

// DoOnInstantiateNil
//
procedure TInstantiateTests.DoOnInstantiateNil(info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=nil;
end;

// Basic
//
procedure TInstantiateTests.Basic;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FFlag:=0;
   FFlagHigh:=0;
   FFlagNb:=0;

   prog:=FCompiler.Compile('Print(test.ClassName); Print(test.ClassName);');

   CheckEquals('', prog.Msgs.AsInfo, 'compile');

   exec:=prog.CreateNewExecution;

   exec.Execute(0);

   CheckEquals('', exec.Msgs.AsInfo, 'exec');

   CheckEquals('TObjectTObject', exec.Result.ToString, 'Result');

   exec:=nil;

   CheckEquals(0, FFlag, 'Flag');
   CheckEquals(1, FFlagHigh, 'FlagHigh');
   CheckEquals(1, FFlagNb, 'FlagNb');
end;

// TwoExecs
//
procedure TInstantiateTests.TwoExecs;
var
   prog : IdwsProgram;
   exec1, exec2 : IdwsProgramExecution;
begin
   FFlag:=0;
   FFlagHigh:=0;
   FFlagNb:=0;

   prog:=FCompiler.Compile('Print(test.ClassName);Print(test.ClassName);');

   CheckEquals('', prog.Msgs.AsInfo, 'compile');

   exec1:=prog.CreateNewExecution;
   exec2:=prog.CreateNewExecution;

   exec1.BeginProgram;
   exec2.BeginProgram;

   exec1.RunProgram(0);
   exec2.RunProgram(0);

   exec1.EndProgram;
   exec2.EndProgram;

   CheckEquals(0, FFlag, 'Flag');
   CheckEquals(2, FFlagHigh, 'FlagHigh');
   CheckEquals(2, FFlagNb, 'FlagNb');
end;

// NilTest
//
procedure TInstantiateTests.NilTest;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile('if testNil=nil then Print("nil");');
   CheckEquals('', prog.Msgs.AsInfo, 'compile');

   exec:=prog.Execute(0);
   CheckEquals('', exec.Msgs.AsInfo, 'exec');

   CheckEquals('nil', exec.Result.ToString, 'Result');
end;

// InfoAccess
//
procedure TInstantiateTests.InfoAccess;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile('');

   exec:=prog.CreateNewExecution;

   exec.BeginProgram;

   exec.RunProgram(0);

   CheckEquals('TTestObj', exec.Info.Vars['test'].ExternalObject.ClassName);

   exec.EndProgram;
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('CornerCasesTests', TInstantiateTests);

end.
