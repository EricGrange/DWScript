var comp := new TDeflateCompressor(1);

// Test 1: Verify WriteData processes all data (internally verified via exception if failed)
comp.WriteData('hello');

// Test 2: Verify large string (exceeding 64KB internal buffer)
// 100,000 characters will exceed the 65536 byte buffer, triggering the loop
var large := StringOfString('A', 100000);
comp.WriteData(large);

// Verify stats
if comp.SizeIn <> 100005 then PrintLn('Bug: SizeIn mismatch: ' + comp.SizeIn.ToString);

var data := comp.Flush;
if comp.SizeOut <> data.Length then PrintLn('Bug: SizeOut mismatch');

PrintLn('OK');
