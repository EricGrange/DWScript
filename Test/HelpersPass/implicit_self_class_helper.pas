type
  TClass1 = class
    Field : String;
    procedure Test;
  end;
  
type
  TClassHelper1 = helper for TClass1
    procedure ABC;
  end;

procedure TClassHelper1.ABC;
begin
   PrintLn('ABC:'+Field);
end;

procedure TClass1.Test;
begin
  Self.ABC;
  ABC;     
end;

var o := new TClass1;
o.Field:='hello';
o.Test;