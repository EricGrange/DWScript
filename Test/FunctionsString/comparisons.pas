procedure PrintCmp(i : Integer);
begin
   if i<>0 then
      PrintLn('<>0')
   else PrintLn('=0');
end;


if SameText('a', 'A') then PrintLn('a same as A');
if SameText('A', 'A') then PrintLn('A same as A');

PrintCmp(CompareStr('a', 'A'));
PrintCmp(CompareStr('A', 'A'));
PrintCmp(CompareText('a', 'A'));
PrintCmp(CompareText('A', 'A'));

PrintCmp(AnsiCompareStr('a', 'A'));
PrintCmp(AnsiCompareStr('A', 'A'));
PrintCmp(AnsiCompareText('a', 'A'));
PrintCmp(AnsiCompareText('A', 'A'));