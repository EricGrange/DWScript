unit Hello;

interface

type
   TTest = class
      Field : String;
      constructor Create(v : String);
      procedure Print;
   end;
   
procedure HelloFunc(v : String);

implementation

procedure HelloFunc(v : String);
begin
   new TTest('Hello '+v+'!').Print;
end;

constructor TTest.Create(v : String);
begin
   Field:=v;
end;

procedure TTest.Print;
begin
   PrintLn(Field);
end;

end.