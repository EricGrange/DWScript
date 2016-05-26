function Test : String; begin Result:=''; end;
function Test2 : String; begin Result:='toto'; end;

PrintLn('' in ['']);

var a:='';
PrintLn(''=a);
PrintLn('' in [a]);

PrintLn(''=Test());
PrintLn('' in [Test()]); 
PrintLn('' in [Test2()]); 

PrintLn('toto' in ['toto']);

var b:='toto';
PrintLn('toto'=b);
PrintLn('toto' in [b]); 


PrintLn('toto'=Test2());
PrintLn('toto' in [Test()]);