var b := TStridedBuffer.Create(TDataType.Float16, [1]);

// Test Smallest Positive Denormal (approx 5.96e-8)
// Bit pattern $0001
b.SetData([0], 0.000000059604644775390625);
PrintLn(b.GetData([0]) > 0);

// Test Infinity
b.SetData([0], 1.0e30); // Should overflow to Infinity
var inf := b.GetData([0]);
PrintLn(inf > 1.0e10);

// Test Negative Infinity
b.SetData([0], -1.0e30);
var ninf := b.GetData([0]);
PrintLn(ninf < -1.0e10);

// Test Zero
b.SetData([0], 0);
PrintLn(b.GetData([0]));

// Test -0
b.SetData([0], -0.0);
PrintLn(b.GetData([0]));
