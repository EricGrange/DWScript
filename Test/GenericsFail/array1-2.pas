type
	TTest<T> = array of T;

var i0 : TTest< >;
var i1 : TTest<Integer>;
var i2 : TTest<Integer,Integer>;

type TBug< > = Integer;


