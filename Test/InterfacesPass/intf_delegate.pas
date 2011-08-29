type IMy = interface
      procedure Hello;
   end;

type
   TTest = class (TObject, IMY)
      Field : String;
      procedure Hello; begin PrintLn('Hello '+Field+' from '+ClassName); end;
   end;

var t := TTest.Create;
t.Field:='World';

var i : IMy := t;

var h : procedure := i.Hello;

h();