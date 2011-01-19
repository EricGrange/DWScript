var sl := TStringList.Create;

sl.Add('beta');
sl.Add('GAMMA');

procedure Test(s : String);
begin
   Print(s);
   if s in sl then
      PrintLn(' - IN');
   if s not in sl then
      PrintLn(' - NOT IN');
end;

procedure RunTests;
begin
   Test('alpha');
   Test('beta');
   Test('gamma');
   Test('delta');

   Test('ALPHA');
   Test('BETA');
   Test('GAMMA');
   Test('DELTA');
end;

sl.CaseSensitive:=True;
PrintLn('Case sensitive');
RunTests;

sl.CaseSensitive:=False;
PrintLn('Case in sensitive');
RunTests;
