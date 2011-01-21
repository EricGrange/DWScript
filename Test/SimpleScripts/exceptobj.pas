type
   EMyExcept = class (Exception) end;

if ExceptObj<>nil then PrintLn('bug');

try
   raise Exception.Create('hello');
except
   PrintLn(ExceptObj.ClassName+': '+ExceptObj.Message);
   try
      raise EMyExcept.Create('world');
   except
      on e: EMyExcept do begin
         PrintLn(e.ClassName+': '+e.Message);
         PrintLn(ExceptObj.ClassName+': '+ExceptObj.Message);
         if e<>ExceptObj then PrintLn('bug');
      end;
      else
         PrintLn('bug');
   end;
   PrintLn(ExceptObj.ClassName+': '+ExceptObj.Message);
end;

if ExceptObj<>nil then PrintLn('bug');
