type
   TMyClass = class
      constructor Build(i : Integer); virtual; default;
   end;

type
   TSubClass = class(TMyClass)
      constructor Build(i : Integer); override;
   end;

constructor TMyClass.Build(i : Integer);
begin
   Print('Root class ');
   PrintLn(i);
end;

constructor TSubClass.Build(i : Integer);
begin
   Print('Sub class ');
   PrintLn(i);
end;

var o1 := new TMyClass(10);
var o2 := new TSubClass(20);

PrintLn(o1.ClassName);
PrintLn(o2.ClassName);