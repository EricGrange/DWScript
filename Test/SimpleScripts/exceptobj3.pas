procedure Test;
begin
   try
      raise Exception.Create('hello');
   except
      PrintLn(ExceptObject.StackTrace);
   end;
end;

PrintLn(ExceptObject = nil);
PrintLn(ExceptObject.StackTrace);
Test;
PrintLn(ExceptObject = nil);