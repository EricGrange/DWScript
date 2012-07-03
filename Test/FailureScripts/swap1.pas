var a : Integer;
var b : String;

Swap(a, a);
Swap(a, 1);
Swap(a, b);
Swap(b, a);
Swap(@Print, a);
Swap(a, Print(''));
Swap(a);

type TA = class
   end;
   
var oo : TObject;
var oa : TA;

Swap(oo, oo);
Swap(oo, oa);
Swap(oa, oo);
Swap(oa, nil);
Swap(nil, oo);