var i : Integer;

for i:=1 to 5 step 3 do
   PrintLn(i);

for i:=5 downto 1 step 3 do
   PrintLn(i);

try
   var s := -1;
   for i:=1 to 5 step s do
      PrintLn(i);
except
   on E: Exception do PrintLn(E.Message);
end;