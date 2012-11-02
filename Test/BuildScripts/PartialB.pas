unit PartialB;

interface

uses PartialA;

type
   TTest = partial class
      procedure PrintB;
   end;

implementation

procedure TTest.PrintB;
begin
   PrintLn('B: '+FieldA);
   FieldA:='changed in B';
   PrintA;
end;

end.
