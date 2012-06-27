type TMy = class end;

function Equal(m1, m2 : TMy) : String;
begin
   Result := 'Hello';
end;

function Diff(m1, m2 : TMy) : String;
begin
   Result := 'World';
end;

operator = (TMy, TMy): String uses Equal;
operator <> (TMy, TMy): String uses Diff;

var c : TMy;

PrintLn(c=c);
PrintLn(c<>c);