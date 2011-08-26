unit URTTIExposeTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsRTTIExposer, TypInfo;

type

   TRTTIExposeTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FUnit : TdwsUnit;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure ExposeInstancesAfterInitTable(Sender : TObject);

      published
         procedure SimpleClass;
         procedure SimpleEnumeration;
         procedure ExposeInstances;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type

   {$M+}
   {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}
   TSimpleClass = class
      private
         FValue : Integer;

      public
         [dwsPublished]
         procedure DecValue;

      published
         [dwsPublished('CreateValued')]
         constructor Create(val : Integer);

         procedure IncValue;

         [dwsNotPublished]
         procedure NullValue;

         procedure Multiply(m : Integer);

         property Value : Integer read FValue write FValue;
   end;

   TSimpleEnumeration = (seZero, seOne, seTwo);

   TTestInstance = class
      private
         FValue : String;
      published
         property Value : String read FValue;
   end;

// Create
//
constructor TSimpleClass.Create(val : Integer);
begin
   FValue:=val;
end;

// IncValue
//
procedure TSimpleClass.IncValue;
begin
   Inc(FValue);
end;

// NullValue
//
procedure TSimpleClass.NullValue;
begin
   FValue:=0;
end;

// Multiply
//
procedure TSimpleClass.Multiply(m : Integer);
begin
   FValue:=FValue*m;
end;

// DecValue
//
procedure TSimpleClass.DecValue;
begin
   Dec(FValue);
end;

// ------------------
// ------------------ TRTTIExposeTests ------------------
// ------------------

// SetUp
//
procedure TRTTIExposeTests.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
   FUnit:=TdwsUnit.Create(nil);
   FUnit.UnitName:='Test';
   FUnit.Script:=FCompiler;
end;

// TearDown
//
procedure TRTTIExposeTests.TearDown;
begin
   FUnit.Free;
   FCompiler.Free;
end;

// SimpleClass
//
procedure TRTTIExposeTests.SimpleClass;
const
   cSimpleClassTest =
       'var c : TSimpleClass = TSimpleClass.Create;'#13#10
      +'Print(IntToStr(c.Value));'#13#10
      +'c.Value:=1;'#13#10
      +'Print(IntToStr(c.Value));'#13#10
      +'c.IncValue;'#13#10
      +'Print(IntToStr(c.Value));'#13#10
      +'c.Multiply(3);'#13#10
      +'Print(IntToStr(c.Value));'#13#10
      +'c.DecValue;'#13#10
      +'Print(IntToStr(c.Value));'#13#10
      +'c := TSimpleClass.CreateValued(3);'#13#10
      +'Print(IntToStr(c.Value));'#13#10
      ;
   cSimpleClassFailNullValueTest =
       'var c : TSimpleClass = TSimpleClass.Create;'#13#10
      +'c.NullValue;'#13#10
      ;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FUnit.ExposeRTTI(TypeInfo(TSimpleClass));

   prog:=FCompiler.Compile(cSimpleClassTest);

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');
   exec:=prog.Execute;
   CheckEquals('', prog.Msgs.AsInfo, 'Exec Msgs');
   CheckEquals('012653', exec.Result.ToString, 'Exec Result');

   prog:=FCompiler.Compile(cSimpleClassFailNullValueTest);

   CheckNotEquals('', prog.Msgs.AsInfo, 'Compile');
end;

// SimpleEnumeration
//
procedure TRTTIExposeTests.SimpleEnumeration;
const
   cSimpleEnumeration =
       'Print(IntToStr(Ord(seZero)));'#13#10
      +'Print(IntToStr(Ord(seOne)));'#13#10
      +'Print(IntToStr(Ord(seTwo)));'#13#10
      ;

var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FUnit.ExposeRTTI(TypeInfo(TSimpleEnumeration));

   prog:=FCompiler.Compile(cSimpleEnumeration);

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.Execute;

   CheckEquals('', prog.Msgs.AsInfo, 'Exec Msgs');
   CheckEquals('012', exec.Result.ToString, 'Exec Result');
end;

var
   i1, i2 : TTestInstance;

// ExposeInstancesAfterInitTable
//
procedure TRTTIExposeTests.ExposeInstancesAfterInitTable(Sender : TObject);
begin
   FUnit.ExposeInstanceToUnit('instanceOne', 'TTestInstance', i1);
   FUnit.ExposeInstanceToUnit('instanceTwo', 'TTestInstance', i2);
end;

// ExposeInstances
//
procedure TRTTIExposeTests.ExposeInstances;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FUnit.ExposeRTTI(TypeInfo(TTestInstance), [eoNoFreeOnCleanup]);

   FUnit.OnAfterInitUnitTable:=ExposeInstancesAfterInitTable;

   i1:=TTestInstance.Create;
   i2:=TTestInstance.Create;

   try
      i1.FValue:='Hello ';
      i2.FValue:='World!';

      prog:=FCompiler.Compile( 'Print(instanceOne.Value);'
                              +'Print(instanceTwo.Value);');

      CheckEquals('', prog.Msgs.AsInfo, 'Compile');

      exec:=prog.Execute;

      CheckEquals('', prog.Msgs.AsInfo, 'Exec Msgs');
      CheckEquals('Hello World!', exec.Result.ToString, 'Exec Result');

   finally

      FUnit.OnAfterInitUnitTable:=nil;

      i1.Free;
      i2.Free;

   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('RTTIExposeTests', TRTTIExposeTests.Suite);

end.
