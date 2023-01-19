try
	try
		if True then
			Fool
		else Bar;
	except
		on E : Exception1 do
			Booh;
		on E : Exception2 do begin
			Foobar;
		end;
	else
		Boobooh
	end
finally
	Done;
end;
