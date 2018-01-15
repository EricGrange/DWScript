type
   TTest = class
      Field : String;
      constructor Create(f : String); begin Field := f end;
      destructor Destroy; override; begin PrintLn(Field) end;
   end;

var a : array [TTest] of TTest;

var k1 := new TTest('k1');

a[k1] := new TTest('alpha');
a[k1] := new TTest('beta');
a[new TTest('k2')] := new TTest('gamma');
PrintLn('- 1');
a.Delete(k1);
PrintLn('- 2');
a.Clear;
PrintLn('- 3 ');
a[k1] := new TTest('delta');
k1 := nil;
PrintLn('- 4');




