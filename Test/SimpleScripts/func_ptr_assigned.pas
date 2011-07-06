type
   TMyFunc = function (i : Integer) : String;

type
   TMyClass = class
      FEvent : TMyFunc;
      function DoConv(i : Integer) : String;
   end;

function TMyClass.DoConv(i : Integer) : String;
begin
   Result:='['+IntToStr(i)+']';
end;

var o := TMyClass.Create;

if Assigned(o.FEvent) then
   PrintLn(o.FEvent(1))
else PrintLn('Unassigned');
o.FEvent:=o.DoConv;
if Assigned(o.FEvent) then
   PrintLn(o.FEvent(2));
   
var e : TMyFunc;

if Assigned(e) then
   PrintLn(e(3))
else PrintLn('Unassigned');
e:=o.DoConv;
if Assigned(e) then
   PrintLn(e(4));
   
e:=nil;
o.FEvent:=e;

if Assigned(e) then
   PrintLn(e(5))
else PrintLn('Unassigned');
