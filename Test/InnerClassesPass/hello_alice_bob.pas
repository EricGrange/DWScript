type
   TTest = class
      type TSub = class
         FName : String;
         procedure Hello; begin PrintLn('Hello ' + FName) end;
      end;

      FSubs : array of TSub;

      procedure Add(n : String);
      begin
         var sub := new TSub;
         sub.FName := n;
         FSubs.Add(sub);
      end;

      procedure Print;
      begin
         for var sub in FSubs do
            sub.Hello;
      end;
   end;
	
type
	TSub = class
		class procedure Byebye; begin PrintLn('ByeBye') end;
	end;

var t := new TTest;
t.Add('Alice');
t.Add('Bob');
t.Print;

TSub.Byebye;