type TTest = class end;
type TSub = class(TTest) end;

var o : array of TObject;
var t : array of TTest;
var s : array of TSub;
s.Add(nil);

PrintLn((t ?? s).Length);
t.Add(nil, nil);
PrintLn((t ?? s).Length);

PrintLn((o ?? t).Length);
o.Add(nil, nil, nil);
PrintLn((o ?? s).Length);

o.Clear;
PrintLn((o ?? o ?? t).Length);
PrintLn((o ?? t ?? o).Length);
PrintLn((o ?? s ?? t).Length);
