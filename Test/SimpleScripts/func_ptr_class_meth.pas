type 
   TProc = procedure;

type
   TMyClass = class
      class procedure SayHello; virtual;
      begin
         PrintLn('Hello '+ClassName);
      end;
      
      class procedure Hello;
      begin
         SayHello;
      end;
   end;
   
type
   TMyChild = class (TMyClass)
      class procedure SayHello; override;
      begin
         PrintLn('Bye bye '+ClassName);
      end;
   end;
   
var p : TProc;

p := TMyClass.Hello;
p();
p := TMyChild.Hello;
p();

var o1 := TMyClass.Create;
p := o1.Hello;
p();
var o2 := TMyChild.Create;
p := o2.Hello;
p();
      