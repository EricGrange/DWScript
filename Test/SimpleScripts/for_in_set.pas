type TMonEnumere = (meRouge, meVert, meBleu);

type TMonEnsemble = set of TMonEnumere;

var ens : TMonEnsemble;
ens := [meBleu, meRouge, meVert];

for var c in ens do begin
  if (c = meRouge) then
   continue;
  PrintLn(c.Name);
end;
