procedure PrintFormat(fmt : String; const args : array of const);
begin
   PrintLn(Format(fmt, args));
end;

PrintFormat('hello %s', ['World']);
PrintFormat('%d %d %d', [1, 2, 3]);
