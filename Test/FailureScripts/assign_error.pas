type TTest = class
      FS : String;
      FF : Float;
      FI : Integer;
      property S : String write FS;
      property F : Float write FF;
      property I : Integer write FI;
   end;


var s : String;
var f : Float;
var i : Integer;

s:=s;
s:=f;
s:=i;

f:=s;
f:=f;
f:=i;

i:=s;
i:=f;
i:=i;

var o := TTest.Create;

o.FS:=s;
o.FS:=f;
o.FS:=i;

o.FF:=s;
o.FF:=f;
o.FF:=i;

o.FI:=s;
o.FI:=f;
o.FI:=i;

o.S:=s;
o.S:=f;
o.S:=i;

o.F:=s;
o.F:=f;
o.F:=i;

o.I:=s;
o.I:=f;
o.I:=i;