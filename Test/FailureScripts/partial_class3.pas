type
   TBase = class
   end;
   
type   
   TTest = partial class (TBase)
      Field : Integer;
   end;

type
   TTest = class partial (TObject)
      Field : Integer;
   end;

type   
   TTest = partial
   end;

   type   
   TTest = partial class 
   end;
      