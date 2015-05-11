var a : ComVariant;

PrintLn('before');
for var s in a do begin
	PrintLn('bug');
	break;
end;
PrintLn('after');