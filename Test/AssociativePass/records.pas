type TMyRec = record
   Value1 : String;
   Value2 : String;
end;

function Test : array[String] of TMyRec;
begin
   var b := Result['toto'];
   b.Value1 := 'tata';
   b.Value2 := 'tutu';
   Result['toto'] := b;
end;
PrintLn(Test.Keys.Join(','));
var r1 := Test['toto'];
PrintLn(r1.Value1 + r1.Value2);

function Test2(s: String) : array[String] of TMyRec;
begin
   var b := Result['a'];
   b.Value1 := 'tata' + s;
   b.Value2 := s + 'tutu';
   Result['a'] := b;
   b.Value2 := 'tutu' + s;
   Result['b'] := b;
end;
var r2 := Test2('-');
PrintLn(r2.Keys.Join(','));
for var k in ['a','b'] do begin
   PrintLn(k);
   PrintLn(r2[k].Value1);
   PrintLn(r2[k].Value2);
end;