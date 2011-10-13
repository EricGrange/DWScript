type
   TMyClass = class
      Field : Integer;
      property Val : Integer read Field;
      procedure Test;
   end;

procedure TMyClass.Test;
begin
   Val.Bug;
end;

