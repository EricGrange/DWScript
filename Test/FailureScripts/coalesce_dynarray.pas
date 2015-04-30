type TTest = class end;
type TSub = class(TTest) end;

var o : array of TObject;
var t : array of TTest;
var s : array of TSub;

var aot := o ?? t;
var aos := o ?? s;
var ats := t ?? s;
var ato := t ?? o;
var aso := s ?? o;
var ast := s ?? t;
