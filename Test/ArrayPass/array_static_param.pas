type TArr = array [1..2] of Integer;

procedure Test(p : TArr);
var
   i : Integer;
begin
   i:=123;
   PrintLn('= '+IntToStr(p[1])+', '+IntToStr(p[2]));
   Assert(i=123);
   Assert(p[2]>0);
end;

const c : TArr = [1, 2];
var v1 : TArr = [3, 4];
var v2 : TArr;
v2 := [5, 6];

Test(c);
Test(v1);
Test(v2);
Test([7, 8]);