unit Helper1;

type 
   TMyHelper1 = helper for Integer
      function ToString : String;
      begin
         Result:='Helper1('+IntToStr(Self)+')';
      end;
      function By2 : Integer;
      begin
         Result:=Self*2;
      end;
   end;