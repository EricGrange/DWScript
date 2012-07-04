unit InitUnit2;

interface

implementation

initialization

var v := 'Hello InitUnit2';
PrintLn(v);
v:= 'Byebye InitUnit2';

finalization

PrintLn(v);

end.
