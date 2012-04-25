type
   TStatic = class
      class procedure Print; static;
      begin
         Default.Print('Hello ');
         Default.PrintLn(ClassName);
      end;
   end;

type   
   TSubStatic = class (TStatic); 

var c := TStatic;
var o := TStatic.Create;
var cs := TSubStatic;
var os := TSubStatic.Create;
var p : Procedure;

p:=TStatic.Print;
p;
p:=TSubStatic.Print;
p;
p:=c.Print;
p;
p:=cs.Print;
p;
p:=o.Print;
p;
p:=os.Print;
p;