type
   EMyExcept = class (Exception) end;

if ExceptObject<>nil then PrintLn('bug');

try
   raise Exception.Create('hello');
except
   PrintLn(ExceptObject.ClassName+': '+ExceptObject.Message);
   try
      raise EMyExcept.Create('world');
   except
      on e: EMyExcept do begin
         PrintLn(e.ClassName+': '+e.Message);
         PrintLn(ExceptObject.ClassName+': '+ExceptObject.Message);
         if e<>ExceptObject then PrintLn('bug');
      end;
      else
         PrintLn('bug');
   end;
   PrintLn(ExceptObject.ClassName+': '+ExceptObject.Message);
end;

if ExceptObject<>nil then PrintLn('bug');

try
   try
      raise EMyExcept.Create('hello world');
   finally
      PrintLn(ExceptObject.ClassName+': '+ExceptObject.Message);
   end;
except
   PrintLn(ExceptObject.ClassName+': '+ExceptObject.Message);
end;

if ExceptObject<>nil then PrintLn('bug');