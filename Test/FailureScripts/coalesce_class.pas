type TTest = class end;
type TSub = class(TTest) end;

var t : TTest;
var o : TObject;

t := t ?? new TObject;
o := t ?? new TObject;
t := t ?? new TTest;
o := t ?? new TTest;
t := t ?? new TSub;
o := t ?? new TSub;

o := t ?? o;
o := o ?? t;
t := t ?? o;
t := o ?? t;



