type 
   TTest = class
      function Test : Integer; empty;
      function Virt : String; virtual; empty;
      method Proc; empty;
   end;
   
type
   TSub = class (TTest)
      function Virt : String; override;
      begin
         PrintLn('here');
         Result:='['+inherited Virt+']';
      end;
   end;
   
var t := new TTest;

PrintLn(t.Test);
PrintLn('-'+t.Virt+'-');
t.Proc;

t := new TSub;

PrintLn(t.Test);
PrintLn('-'+t.Virt+'-');
t.Proc;
