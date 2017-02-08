type TFoo = class end;
type TBar = class end;

type TTestFoo<T: TFoo> = array of T;
type TTestBar<T: TBar> = array of T;

var f1 : TTestFoo<TBar>;
var f2 : TTestFoo<TFoo>;

var b1 : TTestBar<TBar>;
var b2 : TTestBar<TFoo>;

