// comment
unit ScopePrint;

// comment

interface

// comment

procedure Print(s : String);

implementation

procedure Print(s : String);
begin
   Default.Print('Scoped<');
   Default.Print(s);
   Default.Print('>');
end;

procedure PrintLn(s : String);
begin
   Default.PrintLn('invisible<');
   Default.PrintLn(s);
   Default.PrintLn('>');
end;

function IntToHex(i : Integer) : String;
begin
   Result:='bug';
end;

end.
