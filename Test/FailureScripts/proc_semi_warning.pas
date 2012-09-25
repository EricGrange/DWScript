procedure TestProc
begin
end;

function TestFunc : Integer
var
   i : Integer;
begin
   i:=1;
   Result:=i;
end;

type
   TMyClass = class
      method Meth1 : String;
      method Meth2;
   end;
   
method TMyClass.Meth1 : String
begin
   Result:='';
end;

method TMyClass.Meth2
const
   c = 2;
begin
end;