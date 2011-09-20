Type TProc = Procedure;

Procedure Test(Var AProc : TProc);
Begin
End;

Var p : TProc;

Test(p);
Test(p());