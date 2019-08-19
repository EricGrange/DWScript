type TEnum = (one, two, three);
type TSet = set of TEnum;
type TObj = class
   class function Set : TSet; begin Result := [ two ]; end;
   class function Set2(a : Boolean) : TSet; begin if a then Result := [ one, two ] else Result := [ two, three ]; end;
end;
for var e in TObj.Set do PrintLn(e.Name);
PrintLn('-');
for var e in TObj.Set2(True) do PrintLn(e.Name);
PrintLn('-');
for var e in TObj.Set2(False) do PrintLn(e.Name);