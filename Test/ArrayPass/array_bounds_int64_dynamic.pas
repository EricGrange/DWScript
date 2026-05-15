// Red test for a bounds-check bypass on dynamic arrays accessed through
// a non-variable base expression.
//
// TDynamicArrayExpr.EvalAsInteger / EvalAsString / ... (Source/dwsArrayExprs.pas
// around lines 1367..1459) declares the index as a 32-bit `Integer`,
// even though IndexExpr.EvalAsInteger returns an Int64. Any index whose
// low 32 bits land inside the array's length silently bypasses
// BoundsCheckPassed and indexes the wrong element. The function-call base
// here forces the non-`Var` code path so the bug manifests on every platform
// (the variable-base path adds a separate Cardinal-truncation issue that we
// keep out of this test to avoid platform-dependent access violations).
//
// Expected (safe) behaviour: an out-of-bounds exception is raised.
function MakeArr : array of Integer;
begin
   Result := new Integer[5];
   for var i := 0 to 4 do
      Result[i] := 100 + 10*i;
end;

var base : Integer := 0;
base := base + 4294967296;   // 2^32; low 32 bits are 0

try
   PrintLn(MakeArr()[base]);     // truncates to MakeArr()[0] today
   PrintLn('FAIL no exception (MakeArr()[2^32])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for 2^32');
end;

try
   PrintLn(MakeArr()[base + 4]); // truncates to MakeArr()[4] today
   PrintLn('FAIL no exception (MakeArr()[2^32 + 4])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for 2^32 + 4');
end;
