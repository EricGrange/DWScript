type TEnum1 = (Hello = 1);
type TEnum2 = (Hello = 2);

procedure Test1(a : TEnum1); begin end;
procedure Test2(a : TEnum2); begin end;

Test1( Hello );
Test2( Hello );
