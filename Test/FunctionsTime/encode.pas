var y:=2014;
var m:=9;
var d:=5;
var h:=9;
var n:=15;

var dt:=EncodeDate(y,m,d)+EncodeTime(h,n,0,0);

PrintLn(FormatDateTime('dd.mm.yyyy hh:nn',dt));