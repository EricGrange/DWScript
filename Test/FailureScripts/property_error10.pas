type
   TMyClass = class
      function GetVal(i : Integer) : Integer;
      property Val[i : Integer] : Integer read GetVal;
      procedure Test;
   end;

procedure TMyClass.Test;
begin
   Val.Bug;
end;

