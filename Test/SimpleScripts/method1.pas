type
   TMyClass = class
      method Meth1;
      method Meth2 : Integer;
      class method Meth3;
      class method Meth4 : String;
   end;

method TMyClass.Meth1;
begin
   PrintLn('Meth1');
end;

method TMyClass.Meth2 : Integer;
begin
   Result:=123;
end;

class method TMyClass.Meth3;
begin
   PrintLn('Meth3');
end;

class method TMyClass.Meth4 : String;
begin
   Result:='Meth4';
end;

var obj = TMyClass.Create;
obj.Meth1;
PrintLn(IntToStr(obj.Meth2));
obj.Meth3;
TMyClass.Meth3;
PrintLn(obj.Meth4);
PrintLn(TMyClass.Meth4);
