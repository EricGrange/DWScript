procedure Test1(v : Float); begin end;
procedure Test2(v : type Float); begin end;

Test1(1);
Test1(1.5);
Test1('foo');
Test2(1);
Test2(1.5);
Test2('foo');

procedure Test3(v : Float); overload; begin end;
procedure Test3(v : Variant); overload; begin end;

Test3(1);
Test3(1.5);
Test3('foo');

procedure Test4(v : type Float); overload; begin end;
procedure Test4(v : type Variant); overload; begin end;

Test4(1);
Test4(1.5);
Test4('foo');