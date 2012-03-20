type IMy = interface
      procedure Hello;
   end;

type
   TTest = class (TObject, IMY)
      private 
         class var vHello = 'hello';
         procedure Hello; begin PrintLn(vHello+' '+ClassName); end;
         procedure Unused; begin end;
   end;

var i : IMy = new TTest;
i.Hello;