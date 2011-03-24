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

procedure Proc3(i : Float);
begin
   PrintLn('Proc3');
end;

function Proc4 : String;
begin
   Result:='';
end;

var p, pp : TMyProc;

p:=Proc1;
pp:=Proc1();
p:=pp;
pp:=p();

p('hello');

p:=Proc2;
p:=Proc2();

p:=Proc3;
p:=Proc3();

p:=Proc4;
p:=Proc4();
