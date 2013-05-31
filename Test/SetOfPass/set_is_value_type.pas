type TMy = set of (A, B, C);

var v1 : TMy := [A, B, C];
var v2 := v1;

if B in v1 then PrintLn('ok1');
if B in v2 then PrintLn('ok2');

v1.Exclude(B);

if B not in v1 then PrintLn('ok3');
if B in v2 then PrintLn('ok4');

procedure AddIt(v : TMy);
begin
	if B in v then PrintLn('bug');
	
	Include(v, B);
	
	if B not in v then PrintLn('rebug');
end;

AddIt(v1);

if B in v1 then PrintLn('rerebug');



