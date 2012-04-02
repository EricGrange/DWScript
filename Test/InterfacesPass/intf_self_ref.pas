type
   IMy = interface 
      function Test : IMy;
      procedure Print;
   end;
  
type
   TMy = class(TObject, IMy)
      Field : Integer;
      constructor Create(v : Integer);
      begin
         Field:=v;
      end;
      function Test : IMy;
      procedure Print;
   end;

function TMy.Test : IMy;
begin
   Result:=new TMy(Field+1);
end;

procedure TMy.Print;
begin
   PrintLn(Field);
end;

var i, j : IMy;
i:=new TMy(0);
i.Print;
j:=i.Test;
j.Print;
j:=j.Test;
j.Print;
i.Print; 
