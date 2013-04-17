var i, j : Integer;

for i:=-1 to 1 do begin
	for j:=-1 to 1 do begin
		Print('**** '+IntToStr(i)+', '+IntToStr(j)+' : ');
		
		if i=j then Print('=,');
		if i<>j then Print('<>,');
		if i<j then Print('<,');
		if i<=j then Print('<=,');
		if i>j then Print('>,');
		if i>=j then Print('>=,');

		PrintLn('');
	
	end;
end;

var ih, jh : Integer;

for i:=-1 to 1 do begin
	for j:=-1 to 1 do begin
		ih := i * (1 shl 32);
		jh := j * (1 shl 32);
		Print('**** '+IntToStr(ih)+', '+IntToStr(jh)+' : ');
		
		if ih=jh then Print('=,');
		if ih<>jh then Print('<>,');
		if ih<jh then Print('<,');
		if ih<=jh then Print('<=,');
		if ih>jh then Print('>,');
		if ih>=jh then Print('>=,');

		PrintLn('');
	
	end;
end;

for i:=-1 to 1 do begin
	for j:=-1 to 1 do begin
		ih := i + (1 shl 32);
		jh := j + (1 shl 32);
		Print('**** '+IntToStr(ih)+', '+IntToStr(jh)+' : ');
		
		if ih=jh then Print('=,');
		if ih<>jh then Print('<>,');
		if ih<jh then Print('<,');
		if ih<=jh then Print('<=,');
		if ih>jh then Print('>,');
		if ih>=jh then Print('>=,');

		PrintLn('');
	
	end;
end;

for i:=-1 to 1 do begin
	for j:=-1 to 1 do begin
		ih := i - (1 shl 32);
		jh := j - (1 shl 32);
		Print('**** '+IntToStr(ih)+', '+IntToStr(jh)+' : ');
		
		if ih=jh then Print('=,');
		if ih<>jh then Print('<>,');
		if ih<jh then Print('<,');
		if ih<=jh then Print('<=,');
		if ih>jh then Print('>,');
		if ih>=jh then Print('>=,');

		PrintLn('');
	
	end;
end;
