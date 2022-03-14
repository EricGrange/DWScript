var ut := UnixTime;
var t := UTCDateTime;

PrintLn((t-25569)*86400-ut <= 1);

PrintLn(UnixTimeToDateTime(ut)-t <= 1);

PrintLn(DateTimeToUnixTime(UnixTimeToDateTime(1489755600))= 1489755600);

PrintLn(DateTimeToUnixTime(t)-ut <= 1);

var i := 0;
repeat 
	var tMsec := UnixTimeMSec;
	t := UnixTime*1000; 
	if t <> tMsec then begin
		if Abs(t-tMsec) > 1500 then
			PrintLn('range too high ' + t.ToString + ' and ' + tMsec.ToString);
		break;
	end;
	Sleep(10);
until i > 10;
if i > 10 then
    PrintLn('failed')
else PrintLn('ok');
