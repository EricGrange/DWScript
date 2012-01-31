// Ordinary parameter after default parameter is not allowed
Procedure Proc(A : Integer = 1024; B : String);
Begin
End;

type TTest = class
   Procedure Proc(A : Integer = 1024; B : String);
   Begin
   End;
end;

type IIntf = interface
   Procedure Proc(A : Integer = 1024; B : String);
end;

type TProc = Procedure (A : Integer = 1024; B : String);