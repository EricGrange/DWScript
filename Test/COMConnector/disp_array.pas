var v : OleVariant := DispCallProxy;
var params : Array Of Variant;

params.Add(1);
params.Add('two');
params.Add(3.14);
params.Add(OleDate(Now));

PrintLn(v.showArrayInfo(params))