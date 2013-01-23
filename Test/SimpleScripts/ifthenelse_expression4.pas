procedure Proc1;
begin 
   PrintLn('Proc1'); 
end;

procedure Proc2;
begin 
   PrintLn('Proc2');
end;

var b : Boolean;

var p := if b then @Proc1 else @Proc2;
p();
p := if not b then @Proc1 else @Proc2;
p();

p := if True then @Proc1 else @Proc2;
p();
p := if False then @Proc1 else @Proc2;
p();
