function MyBool(b : Boolean) : Boolean;
begin
   PrintLn('MyBool called with ' + if b then 'True' else 'False');
   Result := b;
end;

var b : Boolean;

PrintLn('-- with b False');

PrintLn(b ?? False);
PrintLn(b ?? True);
PrintLn(b ?? MyBool(True));
PrintLn(b ?? MyBool(False));

b := True;
PrintLn('-- with b True');

PrintLn(b ?? False);
PrintLn(b ?? True);
PrintLn(b ?? MyBool(True));
PrintLn(b ?? MyBool(False));
