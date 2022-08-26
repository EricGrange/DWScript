procedure Access(i : Integer);
begin
   var a : array [0..2] of Integer;
   a[0] := 789;
   a[1] := 123;
   a[2] := 456;
   
   var k := a[i];
   PrintLn(k);
end;

procedure CheckExceeded(i : Integer; which : String);
begin
   try
      Access(i);
      if which <> '' then
         PrintLn(i.ToString + ' ' + which + ' detection failed');
   except
      on E : Exception do
         if which not in E.Message then
            PrintLn(i.ToString + ' ' + which + ' qualification failed')
         else PrintLn(i.ToString + ' ' + which + ' exceeded');
   end;
end;

CheckExceeded(-1, 'Lower');
CheckExceeded(0, '');
CheckExceeded(1, '');
CheckExceeded(2, '');
CheckExceeded(3, 'Upper');

