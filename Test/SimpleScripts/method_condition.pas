type 
	TTest = class
		Field : Integer;
	  
		procedure TestPre;
		require
			Field <= 0;
		begin
			Field := 1;
		end;
	  
		procedure TestBoth;
		require
			Field <= 0;
		begin
			Field += 1;
		ensure
			Field > 0;
		end;
	  
   end;

var t := new TTest;
PrintLn(1);
t.TestPre;
PrintLn(2);
try
	t.TestPre;
except
	on E: Exception do PrintLn(E.Message);
end;

t.Field := 0;
PrintLn(3);
t.TestPre;
PrintLn(4);
try
	t.TestBoth;
except
	on E: Exception do PrintLn(E.Message);
end;

t.Field := -2;
try
	t.TestBoth;
except
	on E: Exception do PrintLn(E.Message);
end;
