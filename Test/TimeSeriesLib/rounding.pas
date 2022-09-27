uses System.Data.TimeSeries;

var ts := TimeSeries.Create;

ts.AddSequence('test1', 1);
ts.AddSequence('test2', 2);
ts.AddSequence('test3', 3);

ts.StoreSample('test3', 0, 1.2345);
ts.StoreSample('test2', 0, 1.2345);
ts.StoreSample('test1', 0, 1.2345);

var times : array of Integer;
var values : array of Float; 
PrintLn(ts.ExtractSamples('test1', 0, 10, times, values, [ tseoIgnoreNulls ]));
PrintLn(ts.ExtractSamples('test2', 0, 10, times, values, [ tseoIgnoreNulls ]));
PrintLn(ts.ExtractSamples('test3', 0, 10, times, values, [ tseoIgnoreNulls ]));
PrintLn(JSON.Stringify(times) + ' / ' + JSON.Stringify(values));

ts.Optimize;

times.Clear;
values.Clear;

PrintLn(ts.ExtractSamples('test1', 0, 10, times, values, [ tseoIgnoreNulls ]));
PrintLn(ts.ExtractSamples('test2', 0, 10, times, values, [ tseoIgnoreNulls ]));
PrintLn(ts.ExtractSamples('test3', 0, 10, times, values, [ tseoIgnoreNulls ]));
PrintLn(JSON.Stringify(times) + ' / ' + JSON.Stringify(values));