type
   TMy = class
      x, y : Integer;
      procedure IncY; begin Inc(y, 2); end;
   end;

type
   TMYHelper = class helper for TMy
      procedure IncX; begin Inc(x); end;
      procedure IncXY; begin IncX; IncY; end;
      class procedure PrintClassName;
   end;
   
class procedure TMYHelper.PrintClassName;
begin
   PrintLn(ClassName);
end;

TMy.PrintClassName;

var m := TMy.Create;

m.IncX;
PrintLn(m.x);
m.IncY;
PrintLn(m.y);
m.IncXY;
PrintLn(m.x);
PrintLn(m.y);

m.PrintClassName;

