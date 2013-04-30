var e : string;

try
	PrintLn('ok');
except
   on e : EDelphi do begin
		PrintLn(EDelphi(e).ClassName);
	end;
   on e : Exception do 
		PrintLn(e.ClassName);
end;
