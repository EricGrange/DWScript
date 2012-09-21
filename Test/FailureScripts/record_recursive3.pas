type 
   TRec = record
      A : Integer;
      function Test : TRec;
      begin
         Result.A:=1;
      end;
      B : Integer;
   end;
