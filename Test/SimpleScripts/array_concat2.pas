procedure PrintArray(const A: array of Variant);
begin
  var LFirst:= True;
  for var V in A do begin
    if not LFirst then
      Print(',')
    else
      LFirst:= False;
    Print(V);
  end;
  PrintLn('');
end;

var a: array of Variant;

a:= ['Hello', 1];

PrintArray(a);

a:= a + ['World'] + [4];

PrintArray(a);

a:= ['Say'] + a;
a.Insert(2, 'to');

PrintArray(a);

a:= [7] + [1.5] + a;

PrintArray(a);

a := [2.5] + [1] + a;

PrintArray(a);
