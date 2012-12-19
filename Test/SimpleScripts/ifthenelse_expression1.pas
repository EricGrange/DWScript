var b := True;

PrintLn(if b then 't' else 'f');
PrintLn(if not b then 'f' else 't');

b:=if b then not b else b;

PrintLn(if b then 't' else 'f');
PrintLn(if not b then 'f' else 't');

PrintLn(if 1+1=2 then 'good' else 'bad');
PrintLn(if 1+2=2 then 'oops!' else 'good');
