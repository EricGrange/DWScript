Type TProc = Procedure;

var a : Array Of TProc;
var proc : TProc;

If @proc In a Then PrintLn('bug');

procedure Dummy;
begin
end;

if Dummy in a then PrintLn('bug1');

proc:=Dummy;

A.Add(proc);

if proc not in a then PrintLn('bug2');
if Dummy in a then PrintLn('ok');