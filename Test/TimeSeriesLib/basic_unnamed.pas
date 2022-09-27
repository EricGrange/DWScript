uses System.Data.TimeSeries;

var ts := TimeSeries.Create;

ts.AddSequence('test', 3);

PrintLn(ts.SequenceCount);
ts.StoreSample('test', 1, 1.5);
ts.StoreSample('test', 10, 3.5);

var times : array of Integer;
var values : array of Float;

PrintLn(ts.ExtractSamples('test', 0, 10, times, values, [ tseoIgnoreNulls ]));
PrintLn(JSON.Stringify(times) + ' ' + JSON.Stringify(values));

ts.StoreSample('test', 5, 2.5);

PrintLn(ts.ExtractSamples('test', 0, 10, times, values, [ tseoIgnoreNulls ]));
PrintLn(JSON.Stringify(times) + ' ' + JSON.Stringify(values));

times.Clear;
values.Clear;

PrintLn(ts.ExtractSamples('test', 5, 12, times, values, [ tseoIgnoreNulls ]));
PrintLn(JSON.Stringify(times) + ' ' + JSON.Stringify(values));