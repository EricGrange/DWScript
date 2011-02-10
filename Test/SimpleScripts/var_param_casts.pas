var v : Variant := 0;

procedure TestFloat(var f : Float);
begin
   f:=v;
end;

var f : Float;
TestFloat(f);
PrintLn(f-f);

procedure TestInteger(var i : Integer);
begin
   f:=v;
end;

var i : Integer;
v:=1.0;
TestInteger(i);
PrintLn(i-i);

procedure TestString(var s : String);
begin
   s:=v;
end;

var s : String;
v:='hello';
TestString(s);
PrintLn(v+v);

procedure TestBool(var b : Boolean);
begin
   b:=v;
end;

var b : Boolean;
v:=True;
TestBool(b);
PrintLn(b and b);