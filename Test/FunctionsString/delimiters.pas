var i : Integer;

for i:=1 to 7 do
  if IsDelimiter(',:', 'a:b;c:d', i) then
     PrintLn(i);

i:=LastDelimiter(',:', 'a:b;c:d');
PrintLn(i);
i:=LastDelimiter(',', 'a:b;c:d');
PrintLn(i);

i:=FindDelimiter(',:', 'a:b;c:d');
PrintLn(i);
i:=FindDelimiter(':,', 'a:b;c:d', 3);
PrintLn(i);
i:=FindDelimiter('!?)(', 'a:b;c:d', 3);
PrintLn(i);
