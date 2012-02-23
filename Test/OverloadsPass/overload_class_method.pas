type tobj = class

 function classname(a : Integer) : String; overload;
 begin
   Result:=IntToStr(a);
 end;

 class function classname(s : String) : String; overload;
 begin
   Result:=s+ClassName;
 end;
 
end;

var o := TObj.Create;

println(o.classname);
println(o.classname(123));
println(o.classname('hello '));