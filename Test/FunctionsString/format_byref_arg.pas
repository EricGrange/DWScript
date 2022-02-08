procedure TestVars(var s : String; var i : Integer; var f : Float);
begin
   PrintLn(Format('"%s" %d %.3f', [ s, i, f ]));
end;

var vs := 'hello';
var vi := 123;
var vf := 4.567;
TestVars(vs, vi, vf);

procedure TestArgs(const args : array of const);
begin
   PrintLn(Format('"%s" %d %.3f', args));
end;

procedure TestArgsFunc(var s : String; var i : Integer; var f : Float);
begin
   TestArgs([ s, i, f ]);
end;

vs := 'world';
vi := 789;
vf := 1.234;
TestArgsFunc(vs, vi, vf);