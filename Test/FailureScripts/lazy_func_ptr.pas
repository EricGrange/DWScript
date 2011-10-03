Type TProc = Procedure;

Procedure test(lazy Proc : TProc);
Begin
 Proc;  
 Proc();
End;

Var proc : TProc;

var msg : String;

procedure Hello;
begin
   PrintLn('Hello '+msg);
   msg:=msg+'bis';
end;

proc:=Hello;

msg:='var';
Test(proc);
msg:='proc';
Test(Hello);
msg:='@proc';
Test(@Hello);