type
   TAbstract = class
   public
      procedure DoSomething; virtual; abstract;
      class procedure Test;
   end;

class procedure TAbstract.Test;
begin
   PrintLn('Success');
end;

TAbstract.Test;




