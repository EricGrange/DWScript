Type TProc = Procedure;

procedure Hello;
begin
   PrintLn('Hello');
end;

Var
 P : TProc := Hello;
 
P; // normal call

Type

  TObj = Class
  
   FProc : TProc;
   
   Procedure Proc;
   Begin
    FProc; // error. FProc() - normal 
   End;
   
  End;
  
var o := TObj.Create;
o.FProc := Hello;

o.Proc;