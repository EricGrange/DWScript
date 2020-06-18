procedure Test(y,m,d,h,n,s,ms : Integer);
begin
   var a:=EncodeDateTime(y,m,d,h,n,s,ms);
   var b:=IncMonth(a,2);
   PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss', a)+' => '+FormatDateTime('yyyy-mm-dd hh:nn:ss', b));
end;

FormatSettings.Zone := DateTimeZone.Local;
Test(2019,8,26,3,30,0,0);
Test(2019,8,27,3,30,0,0);
Test(2019,8,28,3,30,0,0);
FormatSettings.Zone := DateTimeZone.UTC;
Test(2019,8,26,3,30,0,0);
Test(2019,8,27,3,30,0,0);
Test(2019,8,28,3,30,0,0);
FormatSettings.Zone := DateTimeZone.Local;
Test(2020,1,28,2,30,0,0);
Test(2020,1,29,1,30,0,0);
Test(2020,1,29,2,30,0,0);
Test(2020,1,30,2,30,0,0);
FormatSettings.Zone := DateTimeZone.UTC;
Test(2020,1,28,2,30,0,0);
Test(2020,1,29,1,30,0,0);
Test(2020,1,29,2,30,0,0);
Test(2020,1,30,2,30,0,0);
