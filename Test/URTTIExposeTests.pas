unit URTTIExposeTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsRTTIExposer, dwsRTTIConnector, TypInfo, Types, RTTI,
   Forms, StdCtrls;

type

   TRTTIExposeTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FRTTIConnector : TdwsRTTIConnector;
         FUnit : TdwsUnit;

      protected
         procedure DoGetSimpleClassInstance(Info: TProgramInfo);

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure ExposeInstancesAfterInitTable(Sender : TObject);

      published
         procedure SimpleClass;
         procedure SimpleEnumeration;
         procedure SimpleRecord;
         procedure SimpleInterface;
         procedure ExposeInstances;

         procedure ConnectSimpleClass;
         procedure ConnectSimpleClassTyped;
         procedure ConnectTypeCheckFail;
         procedure ConnectFormCreateComponent;
         procedure ConnectClassMethod;

         procedure EnvironmentTest;
         procedure EnvironmentTest2;

         procedure ExposeGeneric;
   end;

type
   {$M+}
   {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])  FIELDS([vcPublic, vcPublished])}
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

   [dwsPublished('TRect')]
   TMyRect = record
      Left, Top, Right, Bottom : Integer;
   end;

   TRectFuncs = class
      class function RectWidth(const r : TMyRect) : Integer; static;
      class function UnitRect : TMyRect; static;
      class procedure InflateRect(var r : TMyRect); static;
   end;

   TGenericWrapper<T> = class
      private
         FField : T;
      public
         property Field : T read FField write FField;
   end;

   TWrappedObject = class(TGenericWrapper<TObject>)
      procedure Stuff(obj : TGenericWrapper<Integer>);
   end;

   TTestEnvironment = class
      public
         FieldOne : TSimpleClass;
         FieldTwo : String;
         FieldBool : Boolean;
         FieldFloat : Double;
         FieldInteger : Integer;
   end;

   ISimpleInterface = interface
      ['{4AF21B43-EE4E-4B24-836F-06DF97685315}']
      function GetHello : String;
      property Hello : String read GetHello;
   end;

   TSimpleInterface = class(TInterfacedObject, ISimpleInterface)
      public
         function GetHello : String;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RectWidth
//
class function TRectFuncs.RectWidth(const r : TMyRect) : Integer;
begin
   Result:=r.Right-r.Left;
end;

// UnitRect
//
class function TRectFuncs.UnitRect : TMyRect;
begin
   Result.Left:=0;
   Result.Top:=0;
   Result.Right:=1;
   Result.Bottom:=1;
end;

// InflateRect
//
class procedure TRectFuncs.InflateRect(var r : TMyRect);
begin
   Dec(r.Left); Dec(r.Top);
   Inc(r.Right); Inc(r.Bottom);
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

// Stuff
//
procedure TWrappedObject.Stuff(obj : TGenericWrapper<Integer>);
begin
   //
end;

// ------------------
// ------------------ TSimpleInterface ------------------
// ------------------

// GetHello
//
function TSimpleInterface.GetHello : String;
begin
   Result:='Hello World';
end;

// ------------------
// ------------------ TRTTIExposeTests ------------------
// ------------------

// SetUp
//
procedure TRTTIExposeTests.SetUp;
var
   func : TdwsFunction;
begin
   FCompiler:=TDelphiWebScript.Create(nil);

   FRTTIConnector:=TdwsRTTIConnector.Create(nil);
   FRTTIConnector.Script:=FCompiler;

   FUnit:=TdwsUnit.Create(nil);
   FUnit.UnitName:='Test';
   FUnit.Script:=FCompiler;
   FUnit.Dependencies.Add(RTTI_UnitName);

   func:=FUnit.Functions.Add;
   func.Name:='GetSimpleInstance';
   func.ResultType:='RTTIVariant';
   func.OnEval:=DoGetSimpleClassInstance;
end;

// TearDown
//
procedure TRTTIExposeTests.TearDown;
begin
   FRTTIConnector.Free;
   FUnit.Free;
   FCompiler.Free;
end;

// DoGetSimpleClassInstance
//
procedure TRTTIExposeTests.DoGetSimpleClassInstance(Info: TProgramInfo);
begin
   Info.ResultAsVariant:=TdwsRTTIVariant.FromObject(TSimpleClass.Create(123));
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

// SimpleRecord
//
procedure TRTTIExposeTests.SimpleRecord;
const
   cSimpleRecord =
       'var r : TRect := (Left: 1; Top: 2; Right: 3; Bottom: 4);'#13#10
      +'Print(Format("%d, %d, %d, %d", [r.Left, r.Top, r.Right, r.Bottom]));'#13#10
      +'Print(", "+IntToStr(TRectFuncs.RectWidth(r)));'#13#10
      +'r := TRectFuncs.UnitRect;'#13#10
      +'Print(Format(", %d, %d, %d, %d", [r.Left, r.Top, r.Right, r.Bottom]));'#13#10
      +'TRectFuncs.InflateRect(r);'#13#10
      +'Print(Format(", %d, %d, %d, %d", [r.Left, r.Top, r.Right, r.Bottom]));'#13#10
      ;

var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FUnit.ExposeRTTI(TypeInfo(TMyRect));
   FUnit.ExposeRTTI(TypeInfo(TRectFuncs));

   prog:=FCompiler.Compile(cSimpleRecord);

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.Execute;

   CheckEquals('', exec.Msgs.AsInfo, 'Exec Msgs');
   CheckEquals('1, 2, 3, 4, 2, 0, 0, 1, 1, -1, -1, 2, 2', exec.Result.ToString, 'Exec Result');
end;

// SimpleInterface
//
procedure TRTTIExposeTests.SimpleInterface;
//var
//   prog : IdwsProgram;
//   exec : IdwsProgramExecution;
begin
{   FUnit.ExposeRTTI(TypeInfo(ISimpleInterface));

   prog:=FCompiler.Compile( 'var i : ISimpleInterface'#13#10
                           +'PrintLn(i.Hello);'#13#10
                           +'Print(i.GetHello);');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.Info.Vars['i'].Value:=IUnknown(nil);

      exec.RunProgram(0);

      CheckEquals('', exec.Msgs.AsInfo, 'Exec Msgs');
      CheckEquals('1, 2, 3, 4, 2, 0, 0, 1, 1, -1, -1, 2, 2', exec.Result.ToString, 'Exec Result');
   finally
      exec.EndProgram;
   end; }
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

// ConnectSimpleClass
//
procedure TRTTIExposeTests.ConnectSimpleClass;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile('var v : RTTIVariant = GetSimpleInstance;'#13#10
                           +'PrintLn(v.Value);'#13#10
                           +'v.IncValue();'#13#10
                           +'PrintLn(v.Value);'#13#10
                           +'v.Value:=456;'#13#10
                           +'PrintLn(v.Value);'#13#10
                           +'v.Free();'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.Execute;

   CheckEquals('', prog.Msgs.AsInfo, 'Exec Msgs');
   CheckEquals('123'#13#10'124'#13#10'456'#13#10, exec.Result.ToString, 'Exec Result');
end;

// ConnectSimpleClassTyped
//
procedure TRTTIExposeTests.ConnectSimpleClassTyped;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile('var v : RTTIVariant<URTTIExposeTests.TSimpleClass> = GetSimpleInstance;'#13#10
                           +'PrintLn(v.Value);'#13#10
                           +'v.IncValue();'#13#10
                           +'PrintLn(v.Value);'#13#10
                           +'v.Value:=456;'#13#10
                           +'PrintLn(v.Value);'#13#10
                           +'v.Free();'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.Execute;

   CheckEquals('', prog.Msgs.AsInfo, 'Exec Msgs');
   CheckEquals('123'#13#10'124'#13#10'456'#13#10, exec.Result.ToString, 'Exec Result');
end;

// ConnectTypeCheckFail
//
procedure TRTTIExposeTests.ConnectTypeCheckFail;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile('var v : RTTIVariant<URTTIExposeTests.TBug>;');

   CheckEquals('Syntax Error: Connector "RttiVariant" specialization to "URTTIExposeTests.TBug" failed [line: 1, column: 42]'#13#10,
               prog.Msgs.AsInfo, 'Specialize');

   prog:=FCompiler.Compile('var v : RTTIVariant<URTTIExposeTests.TSimpleClass>;'#13#10
                           +'PrintLn(v.Bug);'#13#10);

   CheckEquals('Syntax Error: Member "Bug" readonly or not found in connector "RttiVariant <URTTIExposeTests.TSimpleClass>" [line: 2, column: 14]'#13#10,
               prog.Msgs.AsInfo, 'Member');
end;

// ConnectFormCreateComponent
//
procedure TRTTIExposeTests.ConnectFormCreateComponent;
var
   form : TForm;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   RegisterClass(TLabel);

   Application.CreateForm(TForm, form);
   try
      form.Name:='ConnectorTestForm';

      prog:=FCompiler.Compile( 'var f := ConnectForm("ConnectorTestForm");'#13#10
                              +'Print(f.Name);'#13#10
                              +'f.Name := "Hello";'#13#10
                              +'var lbl := CreateComponent(f, "StdCtrls.TLabel");'#13#10
                              +'lbl.Caption := "World";'#13#10
                              +'CreateComponent(f, "TLabel").Caption := "Label2";'#13#10
                              );

      CheckEquals('', prog.Msgs.AsInfo, 'compile');

      exec:=prog.Execute(0);

      CheckEquals('ConnectorTestForm', exec.Result.ToString, 'result');

      CheckEquals('Hello', form.Name, 'form renamed');

      CheckEquals(2, form.ComponentCount, 'form component count');

      CheckEquals('TLabel', form.Components[0].ClassName, 'form component[0]');
      CheckEquals('TLabel', form.Components[1].ClassName, 'form component[1]');

      CheckEquals('World', TLabel(form.Components[0]).Caption, 'label 1 caption');
      CheckEquals('Label2', TLabel(form.Components[1]).Caption, 'label 2 caption');
   finally
      form.Free;
   end;
end;

// ConnectClassMethod
//
procedure TRTTIExposeTests.ConnectClassMethod;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   obj : TTestInstance;
begin
   prog:=FCompiler.Compile('var obj: RttiVariant; Print(obj.ClassName());');

   obj:=TTestInstance.Create;
   try
      exec:=prog.BeginNewExecution;
      try
         exec.Info.ValueAsVariant['obj']:=TdwsRTTIVariant.FromObject(obj);
         exec.RunProgram(0);
         CheckEquals(obj.ClassName, exec.Result.ToString, 'exec');
         CheckFalse(exec.Msgs.HasErrors, exec.Msgs.AsInfo);
      finally
         exec.EndProgram;
         exec:=nil;
      end;
   finally
      obj.Free;
   end;
end;

// EnvironmentTest
//
procedure TRTTIExposeTests.EnvironmentTest;
var
   obj : TTestEnvironment;
   enviro : TRTTIEnvironment;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   obj:=TTestEnvironment.Create;
   obj.FieldOne:=TSimpleClass.Create(123);
   obj.FieldTwo:='Hello';
   obj.FieldBool:=True;
   obj.FieldFloat:=3.14;
   obj.FieldInteger:=314;
   try
      enviro:=TRTTIEnvironment.Create;
      enviro.DefaultEnvironment:=obj;
      FCompiler.Extensions.Add(enviro);
      try
         prog:=FCompiler.Compile( 'PrintLn(FieldOne.Value);'#13#10
                                 +'PrintLn(FieldTwo);'#13#10
                                 +'PrintLn(FieldTwo[1]);'#13#10
                                 +'PrintLn(FieldOne.ClassName());'#13#10
                                 +'PrintLn(FieldBool);'#13#10
                                 +'PrintLn(FieldFloat);'#13#10
                                 +'PrintLn(FieldInteger*2);'#13#10);
         try
            CheckEquals('', prog.Msgs.AsInfo, 'compile');

            exec:=prog.Execute(0);

            CheckEquals('', exec.Msgs.AsInfo, 'exec');

            CheckEquals( '123'#13#10'Hello'#13#10'H'#13#10'TSimpleClass'#13#10
                        +'True'#13#10
                        +'3.14'#13#10
                        +'628'#13#10
                        , exec.Result.ToString, 'result');
         finally
            prog:=nil;
         end;
      finally
         FCompiler.Extensions.Remove(enviro);
         enviro.Free;
      end;
   finally
      obj.FieldOne.Free;
      obj.Free;
   end;
end;

// EnvironmentTest2
//
procedure TRTTIExposeTests.EnvironmentTest2;
var
   obj1, obj2 : TTestEnvironment;
   enviro : TRTTIEnvironment;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   obj1:=TTestEnvironment.Create;
   obj1.FieldOne:=TSimpleClass.Create(123);
   obj1.FieldTwo:='Hello';
   obj2:=TTestEnvironment.Create;
   obj2.FieldOne:=TSimpleClass.Create(456);
   obj2.FieldTwo:='World';
   try
      enviro:=TRTTIEnvironment.Create;
      enviro.DefaultEnvironment:=obj1;
      FCompiler.Extensions.Add(enviro);
      try
         prog:=FCompiler.Compile( 'PrintLn(FieldOne.Value);'#13#10
                                 +'PrintLn(FieldTwo);'#13#10
                                 +'PrintLn(FieldOne.ClassName);');
         try
            CheckEquals('', prog.Msgs.AsInfo, 'compile');

            exec:=prog.BeginNewExecution;
            exec.Environment:=TRTTIRuntimeEnvironment.Create(obj2);

            exec.RunProgram(0);
            exec.EndProgram;

            CheckEquals('', exec.Msgs.AsInfo, 'exec 2');
            CheckEquals( '456'#13#10'World'#13#10'TSimpleClass'#13#10, exec.Result.ToString, 'result 2');

            exec:=prog.BeginNewExecution;
            exec.Environment:=TRTTIRuntimeEnvironment.Create(obj1);

            exec.RunProgram(0);
            exec.EndProgram;

            CheckEquals('', exec.Msgs.AsInfo, 'exec 1');
            CheckEquals( '123'#13#10'Hello'#13#10'TSimpleClass'#13#10, exec.Result.ToString, 'result 1');
         finally
            prog:=nil;
         end;
      finally
         FCompiler.Extensions.Remove(enviro);
         enviro.Free;
      end;
   finally
      obj1.FieldOne.Free;
      obj1.Free;
      obj2.FieldOne.Free;
      obj2.Free;
   end;
end;

// ExposeGeneric
//
procedure TRTTIExposeTests.ExposeGeneric;
begin
   FUnit.ExposeRTTI(TypeInfo(TWrappedObject), [eoExposePublic]);
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
