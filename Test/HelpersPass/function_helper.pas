procedure Hello(s : String); helper SayHello;
begin
   Print('Hello ');
   PrintLn(s);
end;

function World(s : String) : String; helper;
begin
   Result:=s+' world';
end;

var s := 'bob';

('World').SayHello;
s.SayHello;

PrintLn(('Hello').World);
PrintLn(s.World);

