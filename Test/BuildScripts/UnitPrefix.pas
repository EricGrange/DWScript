unit UnitPrefix;

interface

type
   TTest = class
      procedure Hello;
   end;

procedure Hello;

implementation

procedure Hello;
begin
   PrintLn('Hello');
end;

procedure TTest.Hello;
begin
   Print(ClassName);
   PrintLn(' Hello');
end;

end.