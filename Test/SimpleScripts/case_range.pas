procedure PrintIntRange(const value : Integer);
begin
   case value of
      -2..-1 : PrintLn('negative');
      0 : PrintLn('zero');
      5, 10 : PrintLn('multiple of five');
      1..4, 6..9 : PrintLn('between 1 and 9 but 5');
   else
      PrintLn('elsewhere');
   end;
end;

var i : Integer;
for i:=-3 to 11 do begin
   Print(IntToStr(i)+Chr(32));
   PrintIntRange(i);
end;

case 'Hello' of
   'Bonjour'..'Icare' : PrintLn('Hello!');
   'Alpha'..'Omega' : PrintLn('oopsie!');
else
   PrintLn('oopsie!');
end;
