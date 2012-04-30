type
   IMy = interface
      procedure SayIt(s : String);
   end;
   
type
   TObj = class (IMy)
      procedure SayIt(s : String); virtual;
      begin
         PrintLn(ClassName+' says '+s);
      end;
   end;
   
type
   TSubObj = class(TObj);

type
   THelper = helper for IMy
      procedure SayItWithQuotes(s : String);
      begin
         SayIt('"'+s+'"');
      end;
      class procedure SayHello;
      begin
         PrintLn('Hello');
      end;
   end;
   
IMy.SayHello;

var i : IMy := TObj.Create;

i.SayIt('hello');
i.SayItWithQuotes('hello');

i := TSubObj.Create;

i.SayIt('world');
i.SayItWithQuotes('world');
