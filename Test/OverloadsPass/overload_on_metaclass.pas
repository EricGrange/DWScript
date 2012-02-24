type 
   TObj = class
      function ClassName : String; overload;
      begin
         Result:='My '+inherited ClassName;
      end;
   end;
type
   TSub = class(TObj);

PrintLn(TObj.ClassName);

var c := TObj;

PrintLn(c.ClassName);

c := TSub;

PrintLn(c.ClassName);
