uses System.Data.TimeSeries;

var ts := TimeSeries.Create;

ts.AddSequence('test', 1);

var t := 0;
for var i := 1 to 20 do begin
   ts.StoreSample('test', t, i);
   t += 1;
   for var k := 1 to i do begin
      ts.StoreSample('test', t, 0/0);
      t += 1;
   end;
end;
var times : array of Integer;
var values : array of Float;

PrintLn(ts.ExtractSamples('test', 0, 10000, times, values, [ tseoIgnoreNulls ]));
PrintLn(JSON.Stringify(times));
PrintLn(JSON.Stringify(values));

times.Clear;
values.Clear;

PrintLn(ts.ExtractSamples('test', 0, 10000, times, values, [ ]));
PrintLn(JSON.Stringify(times));
PrintLn(JSON.Stringify(values));

ts.Optimize;

times.Clear;
values.Clear;

PrintLn(ts.ExtractSamples('test', 0, 10000, times, values, [ tseoIgnoreNulls ]));
PrintLn(JSON.Stringify(times));
PrintLn(JSON.Stringify(values));

times.Clear;
values.Clear;

PrintLn(ts.ExtractSamples('test', 0, 10000, times, values, [ ]));
PrintLn(JSON.Stringify(times));
PrintLn(JSON.Stringify(values));
