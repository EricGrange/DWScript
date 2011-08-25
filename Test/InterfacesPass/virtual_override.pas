type
   IMy = interface 
      function A(s : String) : String;
   end;
type
   TBase = class(TObject, IMy)
      Field : String;
      function A(s : String) : String; virtual;
   end;
type 
   TChildHello = class(TBase, IMy)
      function A(s : String) : String; override;
   end;
type 
   TChildBye = class(TBase)
      FieldBye : String;
      function A(s : String) : String; override;
   end;

   
function TBase.A(s : String) : String;
begin
   Result:='Base '+s+Field;
end;

function TChildHello.A(s : String) : String;
begin
   Result:='Hello '+s+Field;
end;

function TChildBye.A(s : String) : String;
begin
   Result:=FieldBye+' '+s+Field;
end;

var i : IMy;
var b := TBase.Create;

b.Field:='!';
i:=b;

PrintLn(i.A('world'));

b:=new TChildHello;
b.Field:='?';
i:=b;

PrintLn(i.A('World'));

b:=new TChildBye;
b.Field:='.';
TChildBye(b).FieldBye:='Bye';
i:=b;

PrintLn(i.A('WORLD'));



