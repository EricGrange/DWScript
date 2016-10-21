var y:=2014;
var m:=9;
var d:=5;
var h:=9;
var n:=15;

var dt_loc := EncodeDate(y,m,d)+EncodeTime(h,n,0,0);
var dt_utc := EncodeDate(y,m,d,DateTimeZone.UTC)+EncodeTime(h,n,0,0);
var delta := dt_utc-dt_loc;

if delta=0 then PrintLn('Cannot perform test for GMT+0');

var loc_loc := FormatDateTime('dd.mm.yyyy hh:nn', dt_loc, DateTimeZone.Local);
var loc_utc := FormatDateTime('dd.mm.yyyy hh:nn', dt_loc, DateTimeZone.UTC);

var utc_loc := FormatDateTime('dd.mm.yyyy hh:nn', dt_utc, DateTimeZone.Local);
var utc_utc := FormatDateTime('dd.mm.yyyy hh:nn', dt_utc, DateTimeZone.UTC);

if loc_loc=loc_utc then PrintLn('Failed different referencial loc');
if utc_loc=utc_utc then PrintLn('Failed different referencial utc');

if loc_loc<>utc_utc then PrintLn('Failed same referencial "'+loc_loc+'" vs "'+utc_utc+'"');

var dt_loc_loc := ParseDateTime('dd.mm.yyyy hh:nn', loc_loc);
var dt_loc_utc := ParseDateTime('dd.mm.yyyy hh:nn', loc_utc);

if dt_loc_loc=0 then PrintLn('Failed loc_loc');
if dt_loc_utc=0 then PrintLn('Failed loc_utc');

PrintLn( Round( (dt_loc_loc-dt_loc_utc-delta)*86400 ) );

var dt_utc_loc := ParseDateTime('dd.mm.yyyy hh:nn', utc_loc);
var dt_utc_utc := ParseDateTime('dd.mm.yyyy hh:nn', utc_utc);

PrintLn( Round( (dt_utc_loc-dt_utc_utc-delta)*86400 ) );

