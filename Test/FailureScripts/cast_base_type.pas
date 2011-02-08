var f : Float;
var i : Integer;
var b : Boolean;
var s : String;
var v : Variant;

i:=Integer(b);
i:=Integer(s);
b:=Boolean(i);
b:=Boolean(s);
f:=Float(i);
f:=Float(b);
f:=Float(s);
v:=Variant(s);

type TRec = record
 end;
var r : TRec;
r:=TRec(v);