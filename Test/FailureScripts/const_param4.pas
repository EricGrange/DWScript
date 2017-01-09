type
	TStat = array [1..3] of Integer;

procedure Test(const s1 : TStat; const a1 : String);
begin
	procedure SubTest(const s2 : TStat; const a2 : String);
	begin
		s1[1] := 2;
		s2[1] := 3;
		a1 := '';
		a2 := '';
	end;
	SubTest(s1, a1);
end;

var s : TStat;

Test(s, '');