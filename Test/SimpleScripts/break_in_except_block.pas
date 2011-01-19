while True do begin
   try
      raise Exception.Create('hello');
   except
      on e:Exception do begin
         PrintLn(e.Message);
         Break;
      end;
   end;
end;

repeat
   try
      raise Exception.Create('world');
   except
      on e:Exception do begin
         PrintLn(e.Message);
         Break;
      end;
   end;
until False;

var i : Integer;
for i:=1 to 10 do begin
   try
      try
         raise Exception.Create('duh');
      except
         on e: Exception do begin
            PrintLn(e.Message);
            Break;
         end;
      end;
   finally
      PrintLn('finally');
   end;
   PrintLn(i);
end;
