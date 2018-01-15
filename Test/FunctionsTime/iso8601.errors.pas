procedure TryParse(s : String);
begin
	try
		PrintLn(ISO8601ToDateTime(s));
   except
      on E : Exception do
         PrintLn(E.Message + ' for "' + s + '"');
   end;
end;

TryParse('');

TryParse('2017-01-0y');
TryParse('20y7-01-01');
TryParse('2017y01-01');
TryParse('2017-01y01');

TryParse('2017-01-99');
TryParse('2017-99-01');

TryParse('2017-01-01y01:02:03');
TryParse('2017-01-01 y1:02:03');
TryParse('2017-01-01 01:y2:03');
TryParse('2017-01-01 01:02:y3');

TryParse('20170101T0102:03');
TryParse('20170101T01:0203');
TryParse('20170101T0102y');
TryParse('20170101T0102+05:00');
TryParse('2017-01-01T01:02+0500');
