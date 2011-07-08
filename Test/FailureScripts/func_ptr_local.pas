type
   TMyProc = procedure;

procedure Proc1;
begin
 
   procedure Proc2;
   begin
      PrintLn('Proc2');
   end;

   var p : TMyProc := Proc2;
   
end;   

var p : TMyProc := Proc1;
