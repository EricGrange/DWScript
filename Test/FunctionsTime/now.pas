var n : Float = Now;
var d = Date;
var t = Time;

if Abs(Frac(d))>1/86400000 then PrintLn('Date incorrect');
if Int(t)<>0 then PrintLn('Time incorrect');

if Int(Now)<>d then Print('Now incoherent with Date');
if Abs(n-d-t)>1/24/3600 then PrintLn('Now incoherent with Date+Time');

var nUTC := UTCDateTime;

if Abs(nUTC-n)>1 then PrintLn('UTCDateTime incoherent with Now');