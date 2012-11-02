unit PartialA;

interface

type
   TTest = partial class
      FieldA : String = 'hello';
      procedure PrintA;
      procedure AdjustA;
   end;

implementation

procedure TTest.PrintA;
begin
   PrintLn('A: '+FieldA);
end;

procedure TTest.AdjustA;
begin
   FieldA:='world';
end;

end.
