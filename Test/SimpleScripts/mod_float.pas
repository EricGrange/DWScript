
for var i := -5 to 5 do begin
	var f := i * 0.5;
	for var j := -3 to 3 do begin
		if j = 0 then continue;
		var d := j * 0.5;
		var m := f mod d;
		Print(m);
		Print(', ');
	end;
	PrintLn('');
end;

