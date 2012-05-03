type
   TTest = partial class
      Field : Integer;
      procedure PrintMe; begin PrintLn(Field); end;
   end;
   
type   
   TTest = partial class
      procedure PrintMe; begin PrintLn(Field); end;
   end;

type   
   TTest = class partial (TTest)
      Field2 : Integer;
      property Field : Integer read Field2;
   end;

type   
   TTest = partial class
      Field : Integer;
   end;
