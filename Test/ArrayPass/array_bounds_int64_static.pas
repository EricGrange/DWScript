// Red test for a bounds-check bypass on static arrays.
//
// TStaticArrayExpr.GetIndex (Source/dwsArrayExprs.pas:1250) evaluates the
// index as Int64 but stores `EvalAsInteger(exec) - FLowBound` into a 32-bit
// `Result : Integer` before performing a Cardinal-based range check. Any
// index whose low 32 bits land inside the declared range silently bypasses
// the check and reads (or writes) the wrong element instead of raising
// `Upper bound exceeded`.
//
// Expected (safe) behaviour: an out-of-bounds exception is raised for every
// index >= Length even when bits above 2^31 are set.
var a : array[0..4] of Integer;
var i : Integer;
for i := 0 to 4 do
   a[i] := 100 + i;

// Build the indices at runtime so the compiler cannot constant-fold them
// into a static range error.
var base : Integer := 0;
base := base + 4294967296;   // 2^32; low 32 bits are 0

try
   PrintLn(a[base]);          // truncates to a[0] today
   PrintLn('FAIL no exception (a[2^32])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for 2^32');
end;

try
   PrintLn(a[base + 3]);      // truncates to a[3] today
   PrintLn('FAIL no exception (a[2^32 + 3])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for 2^32 + 3');
end;

try
   a[base + 2] := 999;        // truncates write to a[2] today
   PrintLn('FAIL no exception on write (a[2^32 + 2])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised on write');
end;

// Witness corruption: if the bounds check was bypassed the previous write
// will have clobbered a[2].
PrintLn(a[2]);
