try
   new DataBase('bug');
except
   on E: Exception do
      PrintLn(E.Message);
end;
