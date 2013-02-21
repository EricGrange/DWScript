type
   TTest = class
      procedure Test; overload;
      procedure Test(i : Integer); 
   end;

procedure Test; overload;
begin
end;

procedure Test(i : Integer);
begin
end;

procedure TTest.Test(a : String);
begin
end;

class procedure TTest.Test;
begin
end;
