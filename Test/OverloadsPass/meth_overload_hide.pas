type
   TBase = class
      procedure Test(f : Float);
   end;
type
   TTest = class(TBase)
      procedure Test; overload;
   end;
type
   TSubTest = class(TTest)
      procedure Test(a : Integer); overload;
   end;

procedure TBase.Test(f : Float);
begin
   PrintLn('Float');
end;

procedure TTest.Test;
begin
   PrintLn('no param');
end;

procedure TSubTest.Test(a : Integer);
begin
   PrintLn(a);
end;

var s : TSubTest;

s:=TSubTest.Create;
s.Test(123);
s.Test;
s.Test(12.5);

