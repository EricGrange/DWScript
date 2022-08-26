var a : array of Integer;

procedure Access(a : array of Integer; i : Integer);
begin
   var k := a[i];
   PrintLn(k);
end;

procedure CheckExceeded(i : Integer; which : String);
begin
   try
      Access(a, i);
      if which <> '' then
         PrintLn(i.ToString + ' ' + which + ' detection failed');
   except
      on E : Exception do
         if which not in E.Message then
            PrintLn(i.ToString + ' ' + which + ' qualification failed')
         else PrintLn(i.ToString + ' ' + which + ' exceeded');
   end;
end;

CheckExceeded(0, 'Upper');

a.Add(123);
CheckExceeded(-1, 'Lower');
CheckExceeded(0, '');
CheckExceeded(1, 'Upper');

a.Add(456);
CheckExceeded(-1, 'Lower');
CheckExceeded(0, '');
CheckExceeded(1, '');
CheckExceeded(2, 'Upper');

