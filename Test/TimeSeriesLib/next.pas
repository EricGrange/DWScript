uses System.Data.TimeSeries;

var ts := TimeSeries.Create;

var t := -1;

PrintLn(ts.NextTimeStamp(t));
PrintLn(t);

ts.AddSequence('test', 0);

ts.StoreSample('test', 0, 10);
ts.StoreSample('test', 1, 11);
ts.StoreSample('test', 3, 13);

PrintLn('Iterate');
while ts.NextTimeStamp(t) do
   PrintLn(t.ToString + ' = ' + ts.GetSample('test', t).ToString);

t := 2;
PrintLn(ts.NextTimeStamp(t));
PrintLn(t);

t := 200;
PrintLn(ts.NextTimeStamp(t));
PrintLn(t);
