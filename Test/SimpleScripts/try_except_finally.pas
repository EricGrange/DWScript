try
   Print('Hello');
except
   Print('BUG');
finally
   PrintLn(' World');
end;

try
   raise new Exception('DOH');
except
   Print('Exception ');
finally
   PrintLn('World');
end;

type EMyExcept = class(Exception) end;

try
   raise new EMyExcept('Bye');
except
   on E : EMyExcept do
      Print(E.Message)
   else 
      PrintLn('Bug');
finally
   PrintLn(' World');
end;
   