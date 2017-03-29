type
    TBase = class
        property alpha : Integer;
    end;
    
type
    TChild = class (TBase)
        property beta : String;
    end;

var a : array of TBase;

a.Add(new TBase);
a.Add(new TChild);
a.Add(nil);

PrintLn(JSON.Stringify(a));
