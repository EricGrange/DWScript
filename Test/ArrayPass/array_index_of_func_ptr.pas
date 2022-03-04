Type TProc = Procedure;

procedure test;
begin
end; 

type IMY = interface
      procedure Dummy;
   end;

type TMy = class(IMy)
      procedure Dummy; begin end;
      class procedure CDummy; begin end;       
   end;
   
var m := new TMy;
var i := m;

var a : Array of TProc;

a.add(test); 
a.Add(TMy.CDummy);
a.Add(m.Dummy);
a.Add(i.Dummy);
 
PrintLn(a.IndexOf(test));
PrintLn(a.IndexOf(nil));
PrintLn(a.IndexOf(@test));

PrintLn(a.IndexOf(TMy.CDummy));
PrintLn(a.IndexOf(m.Dummy));
PrintLn(a.IndexOf(i.Dummy));
PrintLn(a.IndexOf(i.Dummy, 3));

