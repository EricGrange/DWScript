var i : Integer;

for i:=1 to 7 do
  if IsDelimiter(',:', 'a:b;c:d', i) then
     PrintLn(i);

PrintLn(LastDelimiter(',:', 'a:b;c:d'));
PrintLn(LastDelimiter(',', 'a:b;c:d'));