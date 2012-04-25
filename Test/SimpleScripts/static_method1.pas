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

TStatic.Print;

var c := TStatic;
c.Print;

var o := TStatic.Create;
o.Print;

TSubStatic.Print;

var cs := TSubStatic;
cs.Print;

var os := TSubStatic.Create;
os.Print;
