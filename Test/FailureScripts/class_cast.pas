type TMyObject = class end;
var o : TObject;

raise Exception('message');
raise Exception(123);
o:=Exception(True);

var e : Exception;
var m : TMyObject;

o:=Exception(m);  // !!
o:=TObject(m);
o:=TMyObject(m);
o:=Exception(e);
o:=TObject(e);

m:=TMyObject(m);
m:=TMyObject(o);
m:=TMyObject(e);  // !!

m:=TMyObject(m;