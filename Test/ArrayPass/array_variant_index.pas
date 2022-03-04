var b : Array [0 .. 5] Of String;
var v : Variant := '5';

b[v] := '56';

v:='';
try
   b[v] := '56';
except
   on e: Exception do 
      PrintLn(e.Message.Before(' ('));
end;

v:=False;
b[v]:='123';
PrintLn(b[0]);