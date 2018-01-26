type
   TRec = record
      Field : Integer;
      procedure Increment; begin Field += 1; end;
      function Func1 : Integer; begin Increment; Result := Field; end;
      function Func2 : Integer; begin Self.Increment; Result := Field; end;
   end;

var r1, r2 : TRec;

PrintLn(r1.Func1);
PrintLn(r1.Func1);
PrintLn(r2.Func1);
PrintLn(r1.Func1);

PrintLn(r1.Func2);
PrintLn(r1.Func2);
PrintLn(r2.Func2);
PrintLn(r1.Func2);