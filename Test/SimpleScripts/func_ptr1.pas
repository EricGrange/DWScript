type
   TMyProc = procedure;

procedure Proc1;
begin
	PrintLn('Proc1');
end;   

procedure Proc2;
begin
	PrintLn('Proc2');
end;   

var p, pp : TMyProc;

p:=Proc1;
pp:=p;
p;
p:=Proc2;
p();

pp;
pp:=p;
pp();