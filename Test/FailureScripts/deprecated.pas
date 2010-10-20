type
   TTest = class
      method Meth; deprecated 'Meth is gone';
   end;

method TTest.Meth;
begin
end;

procedure TestProc; deprecated;
begin
end;

function TestFunc : Integer; deprecated 'returns 1';
begin
   Result:=1;
end;

TestProc;
TestFunc;

var t = TTest.Create;
t.Meth;