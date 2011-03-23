type
   TMyProc = procedure;

procedure Proc1;
begin
   PrintLn('Proc1');
end;   

procedure Proc2(i : Integer);
begin
	PrintLn('Proc2');
end;   

var p, pp : TMyProc;

p:=Proc1;
pp:=Proc1();
p:=pp;
pp:=p();

p('hello');

p:=Proc2;
p:=Proc2();
