type TClassA = class;

type TClassB = class(TClassA) end;

type TClassC = class(TClassB) end;

type TClassA = class(TClassC) end;