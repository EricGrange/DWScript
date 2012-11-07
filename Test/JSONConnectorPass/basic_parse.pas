var v := JSON.Parse('{"hello":"world","one":1,"half":0.5,"yes":true,"no":false}');

PrintLn(v.TypeName());
PrintLn(v.hello.TypeName());
PrintLn(v.hello);
PrintLn(v.one.TypeName());
PrintLn(v.one);
PrintLn(v.half.TypeName());
PrintLn(v.half);
PrintLn(v.yes.TypeName());
PrintLn(v.yes);
PrintLn(v.no);

var i : Integer;
for i:=v.Low() to v.High() do
   PrintLn(v.ElementName(i));
