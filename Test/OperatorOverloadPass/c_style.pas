function EqualWithPrint(a, b  : Integer) : Boolean;
begin
   Result := a = b;
   PrintLn('hello');
end;

function DiffWithPrint(a, b  : Integer) : Boolean;
begin
   Result := a <> b;
   PrintLn('world');
end;

operator == (Integer, Integer) : Boolean uses EqualWithPrint;
operator != (Integer, Integer) : Boolean uses DiffWithPrint;

var a := 1;
var b := 1;

PrintLn(a = b);
PrintLn(a == b);
PrintLn(a <> b);
PrintLn(a != b);
PrintLn(a == a);
PrintLn(a != a);