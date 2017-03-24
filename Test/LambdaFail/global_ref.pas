var a, b, c : Integer;

var f := lambda => Inc(b);

procedure Test;
begin
    PrintLn(f());
end;

PrintLn(f());
Test;

Print(a);
Print(b);
PrintLn(c);

f := lambda => Inc(a);

PrintLn(f());
Test;

Print(a);
Print(b);
PrintLn(c);
