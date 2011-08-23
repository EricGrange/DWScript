var level : Integer;

procedure Recursive;
begin
   Inc(level);
   try
      Recursive;
   except
   end;
end;

Recursive;

if level>1000 then
   Println('Recursion Level is above 1000')
else Println('Recursion Level is under 1000');