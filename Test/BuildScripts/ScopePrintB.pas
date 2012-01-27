// comment
unit ScopePrintB;

interface

procedure PrintLn(i : Integer);

implementation

uses ScopePrint;

procedure PrintLn(i : Integer);
begin
   Default.Print('ScopedB<');
   Print(IntToStr(i));
   Default.PrintLn('>');
end;

procedure Print(s : String);
begin
   Default.Print('invisibleB<');
   Default.Print(s);
   Default.Print('>');
end;

end.