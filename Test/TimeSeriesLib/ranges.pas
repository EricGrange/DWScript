uses System.Data.TimeSeries;

procedure TestRange(mini, maxi, decimals : Integer);
begin
   PrintLn('Testing range ' + mini.ToString + ' to ' + maxi.ToString + ' with ' + decimals.ToString + ' decimals');
   
   var ts := TimeSeries.Create;

   var scale := IntPower(10, decimals);
   var ref : array of Float;
   SetRandSeed(0);
   for var i := 1 to 1000 do
      ref.Add((mini+RandomInt(maxi-mini+1))/scale);

   ts.AddSequence('test', decimals);

   for var i := 0 to ref.High do
      ts.StoreSample('test', i, ref[i]);
   
   var times : array of Integer;
   var values : array of Float;

   var nb := ts.ExtractSamples('test', 0, ref.High, times, values, [ tseoIgnoreNulls ]);
   Assert(nb = ref.Length, 'before optimize');

   for var i := 0 to ref.High do begin
      if times[i] <> i then
         PrintLn('Before, invalid time at ' + i.ToString);
      if Round((values[i] - ref[i])*scale) <> 0 then
         PrintLn('Before, invalid value at ' + i.ToString);
   end;

   ts.Optimize;

   times.Clear;
   values.Clear;

   nb := ts.ExtractSamples('test', 0, ref.High, times, values, [ tseoIgnoreNulls ]);
   Assert(nb = ref.Length, 'after optimize');

   for var i := 0 to ref.High do begin
      if times[i] <> i then
         PrintLn('After, invalid time at ' + i.ToString);
      if Round((values[i] - ref[i])*scale) <> 0 then
         PrintLn('After, invalid value at ' + i.ToString + ': ' + values[i].ToString + ' vs ' + ref[i].ToString);
   end;
end;

TestRange(0, 10, 0);
TestRange(0, 10, 1);
TestRange(0, 1000, 2);
TestRange(-10000, 10000, 3);
TestRange(-100000000, 100000000, 3);
TestRange(0, 1000000000000000, 1);
