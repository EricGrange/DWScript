type TA2 = array [0..1] of Integer;
type TA3 = array [0..2] of Integer;
type TA13 = array [1..3] of Integer;

type
   TTest = class
      class procedure Test(const a : TA2); overload;
      class procedure Test(const a : TA3); overload; 
      class procedure Test(const a : TA13); overload; 
   end;
   
class procedure TTest.Test(const a : TA2);
begin 
   PrintLn('A2');
end;

class procedure TTest.Test(const a : TA3);
begin 
   PrintLn('A3');
end;

class procedure TTest.Test(const a : TA13);
begin 
   PrintLn('A13');
end;

procedure Test(const a : TA2); overload;
begin 
   PrintLn('A2');
end;

procedure Test(const a : TA3); overload; 
begin 
   PrintLn('A3');
end;

procedure Test(const a : TA13); overload; 
begin 
   PrintLn('A13');
end;

var a2 : TA2;
var a3 : TA3;
var a13 : TA13;

TTest.Test(a2);
TTest.Test(a3);
TTest.Test(a13);

Test(a2);
Test(a3);
Test(a13);
