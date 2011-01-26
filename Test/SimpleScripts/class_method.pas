type
   TMyClass = class
      class procedure PrintMyName;
      class function GetMyName : String; virtual;
   end;

type
   TMyOtherClass = class (TMyClass)
      class procedure PrintMyName;
      class function GetMyName : String; override;
   end;

class procedure TMyClass.PrintMyName;
begin
   PrintLn('TMyClass');
end;

class function TMyClass.GetMyName : String;
begin
   Result:='This is TMyClass';
end;

class procedure TMyOtherClass.PrintMyName;
begin
   PrintLn('TMyOtherClass');
end;

class function TMyOtherClass.GetMyName : String;
begin
   Result:='I''m TMyOtherClass';
end;

TMyClass.PrintMyName;
TMyOtherClass.PrintMyName;
PrintLn(TMyClass.GetMyName);
PrintLn(TMyOtherClass.GetMyName);

var my : TMyClass;

my:=TMyClass.Create;
my.PrintMyName;
PrintLn(my.GetMyName);

my:=TMyOtherClass.Create;
my.PrintMyName;
PrintLn(my.GetMyName);

var myo : TMyOtherClass;

myo:=TMyOtherClass.Create;
myo.PrintMyName;
PrintLn(myo.GetMyName);

myo:=nil;
try
   myo.PrintMyName;
except
   on E: Exception do
      PrintLn(e.Message);
end;


