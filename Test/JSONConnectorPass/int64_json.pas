var i := 4611686018427387905;
PrintLn(i);

var js := JSON.Stringify(i);
Print('Stringified     ');
PrintLn(js);

var v := JSON.Parse(js);
Print('Parsed          ');
PrintLn(v);
PrintLn(v.TypeName());

Print('Parsed to Int   ');
i := v;
PrintLn(i);

Print('Parsed to Float ');
var f : Float := v;
PrintLn(f);
Print('Rounded         ');
PrintLn(Round(f));
