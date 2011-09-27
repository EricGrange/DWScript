procedure Dummy1(a : array of const); external;

procedure Dummy2(lazy a : Integer = 1); external;
procedure Dummy3(var a : Integer = 1); external;
procedure Dummy4(const a : Integer = 1); external;

procedure Dummy5(a : Float = Random); external;

const f : Float = 1.5;

procedure Dummy6(a : Integer = f); external;

procedure Dummy7(a : Integer
