try
except
   on e : Exception do begin
      try
         PrintLn(e);
      except
         try
            PrintLn(e);
         except
            on e : Exception do 
               bug();
         end;
      end;
   end;
end;