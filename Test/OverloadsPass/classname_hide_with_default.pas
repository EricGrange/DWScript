type tobj = class

 function ClassName(a : Integer = 5) : String; overload;
 begin
    Result:=IntToStr(a);
 end;

end;

type TSub = class (Tobj)
 function ClassName(a : Integer = 2) : String; overload;
 begin
    Result:=IntToStr(a);
 end;
end;

var o := TObj.Create;

println(o.ClassName);
println(o.ClassName(1));
println(TObject(o).ClassName);

println(TSub.Create.Classname);
