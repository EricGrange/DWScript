
procedure TestVar(var a : array [String] of String);
begin
   PrintLn('v ' + a['hello']);
   var b : array [String] of String;
   b['world'] := 'beta';
   a := b;
end;

procedure TestConst(const a : array [String] of String);
begin
   PrintLn('c ' + a['hello']);
   a['world'] := 'beta';
end;

var a1, a2 : array [String] of String;
a1['hello'] := 'alpha';
a2 := a1;

TestVar(a1);

PrintLn(a1['hello']);
PrintLn(a1['world']);
PrintLn(a2['hello']);
PrintLn(a2['world']);

TestConst(a1);

PrintLn(a1['hello']);
PrintLn(a1['world']);
