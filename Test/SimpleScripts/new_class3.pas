type
   TMyClass = class
      constructor Build(i : Integer = -1); default;
   end;

type
   TSubClass = class(TMyClass)
   end;
   
type TClassRef = class of TMyClass;

constructor TMyClass.Build(i : Integer = -1);
begin
   Print(ClassName);
   PrintLn(i);
end;

function GetAClass(r : TClassRef) : TClassRef;
begin
   Result:=r;
end;

var o1 := new TMyClass;
var o2 := new (TMyClass)(1);

var r := TSubClass;

new r;
new r(1);
new (r)(2);

o1:=new (GetAClass(TMyClass));
o1:=new (GetAClass(r))();
o1:=new (GetAClass(TSubClass))(3);

