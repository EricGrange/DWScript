Type TProc = Procedure;

procedure SayHello;
begin
   PrintLn('Hello');
end;

procedure SayBye;
begin
   PrintLn('Bye');
end;

Procedure Test(Var AProc : TProc);
Begin
   if Assigned(AProc) then
      AProc;
   AProc:=SayBye;
End;

Var p : TProc;

Test(p);
Test(p);
p:=SayHello;
Test(p);
Test(p);
