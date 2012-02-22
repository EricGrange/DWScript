type tobj = class
 field : Integer;

 constructor create(a : Integer); overload;
 begin
   field:=a;
 end;

end;

var o := TObj.Create;
PrintLn(o.field);
o := TObj.Create(1);
PrintLn(o.field);
o := new TObj;
PrintLn(o.field);
o := new TObj(2);
PrintLn(o.field);
