var i : Integer;

procedure Test;
begin
   for var i:=1 to 10 do break;
end;

procedure Test2;
begin
   var i : Integer;
   
   begin
      for var i:=1 to 10 do break;
   end;
end;

for var i:=1 to 10 do break;