type
   TMyClass = class
      procedure PrintMyName; virtual;
      procedure PrintClassName;
   end;
type
   TChild = class (TMyClass)
      procedure PrintMyName; override;
   end;

procedure TMyClass.PrintMyName;
begin
   PrintLn('Ancestor: '+ClassName);
end;

procedure TMyClass.PrintClassName;
begin
   PrintLn(ClassName);
end;

procedure TChild.PrintMyName;
begin
   PrintLn('I''m '+ClassName);
end;

var o : TObject;
var c : TClass;

o:=TObject.Create;
c:=TObject;
PrintLn(TObject.ClassName);
PrintLn(o.ClassName);
PrintLn(c.ClassName);

o:=TMyClass.Create;
c:=TMyClass;
TMyClass(o).PrintMyName;
TMyClass(o).PrintClassName;
PrintLn(TMyClass.ClassName);
PrintLn(o.ClassName);
PrintLn(c.ClassName);

o:=TChild.Create;
c:=TChild;
TMyClass(o).PrintMyName;
TMyClass(o).PrintClassName;
PrintLn(TChild.ClassName);
PrintLn(o.ClassName);
PrintLn(c.ClassName);
