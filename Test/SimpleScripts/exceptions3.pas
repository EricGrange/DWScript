type E1 = class(Exception) end;
type E2 = class(Exception) end;

try
   try
      raise E1.Create('');
   except
      on e : E1 do PrintLn('Caught E1');
   end;
except
   on e : E2 do PrintLn('Caught E2');
end;

try
   try
      raise E2.Create('');
   except
      on e : E1 do PrintLn('Caught E1');
   end;
except
   on e : E2 do PrintLn('Caught E2');
end;
