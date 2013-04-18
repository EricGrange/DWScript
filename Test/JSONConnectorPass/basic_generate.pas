var v := JSON.Parse('{}');

v.Hello := 'World';
v['123'] := 456;
v.Seven := 7.0;

PrintLn(JSON.Stringify(v));

v.Flags := JSON.NewObject;
v.Flags.Bool := True;

v.List := JSON.NewArray;
v.List[0] := 'zero';
v.List[2] := 2;
v.List[4] := True;
v.List[3] := 2.5;

PrintLn(JSON.Stringify(v));
