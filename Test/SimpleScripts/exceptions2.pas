procedure Trigger;
begin
   raise Exception.Create('');
end;

try
   try
      Trigger;
      PrintLn('Never here!');
   except
      PrintLn('Caught once');
      raise;
      PrintLn('Missed!');
   end;
except
   PrintLn('Caught again');
end;
PrintLn('Ended');

