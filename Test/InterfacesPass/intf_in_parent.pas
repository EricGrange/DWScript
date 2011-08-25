type
   IMy = interface 
      function A(s : String) : String;
   end;
type
   TBase = class(TObject, IMy)
      Field : String;
      function A(s : String) : String;
   end;
type 
   TChild = class(TBase)
   end;

function TBase.A(s : String) : String;
begin
   Result:='Base '+s+Field;
end;


var i : IMy;
var b := TBase.Create;

b.Field:='!';
i:=b;

PrintLn(i.A('world'));

b:=new TChild;
b.Field:='?';
i:=b;

PrintLn(i.A('World'));

