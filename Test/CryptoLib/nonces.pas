var n1 := Nonces.Generate(99999);
var n2 := Nonces.Generate(99999);

if n1=n2 then PrintLn('bug');

PrintLn(Nonces.CheckAndKeep(n1));
PrintLn(Nonces.CheckAndRemove(n2));

PrintLn(Nonces.CheckAndKeep(n1));
PrintLn(Nonces.CheckAndRemove(n2));

Nonces.Clear;

PrintLn(Nonces.CheckAndKeep(n1));
