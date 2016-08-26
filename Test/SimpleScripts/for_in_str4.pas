var s := 'bancza';
var cc := 0;
for cc in s do begin
   if cc = Ord('n') then continue;
   if cc = Ord('z') then break;
	PrintLn(Chr(cc));
end;