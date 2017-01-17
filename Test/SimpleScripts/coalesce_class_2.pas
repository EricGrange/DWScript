type TTest = class end;
type TSub1 = class(TTest) end;
type TSub2 = class(TTest) end;

var t : TTest;
var s1 := TSub1.Create;
var s2 : TSub2;

t := s1 ?? s2;
PrintLn(t.ClassName);
t := s2 ?? s1;
PrintLn(t.ClassName);

s2 := TSub2.Create;

t := s1 ?? s2;
PrintLn(t.ClassName);
t := s2 ?? s1;
PrintLn(t.ClassName);

s1 := nil;

t := s1 ?? s2;
PrintLn(t.ClassName);
t := s2 ?? s1;
PrintLn(t.ClassName);

