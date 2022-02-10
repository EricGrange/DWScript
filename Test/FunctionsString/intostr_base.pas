for var i := 2 to 36 do begin
   PrintLn(IntToStr(0, i));
   PrintLn(IntToStr(123456789, i));
   PrintLn(IntToStr(-123456789, i));
end;

var b := 0;
try
   IntToStr(0, b);
except
   on E: Exception do
	  PrintLn(E.Message);
end;