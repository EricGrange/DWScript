type
   TMyClass = class
      procedure PrintMyName; virtual;
   end;
type
   TChild = class (TMyClass)
      procedure PrintMyName; override;
   end;

procedure TMyClass.PrintMyName;
begin
   PrintLn('Ancestor: '+ClassName);
end;

procedure TChild.PrintMyName;
begin
   PrintLn('I''m '+ClassName);
end;

var o : TObject;
var c : TClass;

o:=TObject.ClassType.Create;
c:=o.ClassType;
PrintLn(TObject.ClassName);
PrintLn(o.ClassName);
PrintLn(c.ClassName);
o:=c.Create;
PrintLn(o.ClassName);

var mc : TMyClass;
var cc : TClass;

mc:=TMyClass.Create;
cc:=mc.ClassType;
mc.PrintMyName;
PrintLn(TMyClass.ClassType.ClassName);
PrintLn(cc.ClassName);

mc:=TChild.Create;
cc:=mc.ClassType;
mc.PrintMyName;
PrintLn(TChild.ClassType.ClassName);
PrintLn(cc.ClassName);
