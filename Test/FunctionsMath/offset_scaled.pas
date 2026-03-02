var a1 : array of Float = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
var a2 : array of Float = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1];

procedure PrintA(const a : array of Float);
begin
   for var i := 0 to a.Length - 1 do begin
      if i > 0 then Print(' ');
      Print(FloatToStr(a[i]));
   end;
   PrintLn('');
end;

Print('3-arg: ');
a1.OffsetScaled(a2, 10);
PrintA(a1);

a1 := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
Print('6-arg: ');
a1.OffsetScaled(a2, 2, 2, 5, 3);
PrintA(a1);

procedure TestError(const msg : String; off1, off2, count : Integer);
begin
   var a : array of Float = [1.0, 2.0];
   var b : array of Float = [1.0, 2.0];
   try
      Print(msg + ': ');
      a.OffsetScaled(b, 1.0, off1, off2, count);
      PrintLn('Should have failed');
   except
      on e: Exception do PrintLn('Caught: ' + e.Message);
   end;
end;

TestError('Negative count', 0, 0, -1);
TestError('off1 < 0', -1, 0, 1);
TestError('off1 too large', 2, 0, 1);
TestError('off1+count too large', 1, 0, 2);
TestError('off2 < 0', 0, -1, 1);
TestError('off2 too large', 0, 2, 1);
TestError('off2+count too large', 0, 1, 2);
