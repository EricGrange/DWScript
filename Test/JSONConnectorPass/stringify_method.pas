type
    TBase = class
        property alpha : Integer;
    end;
    
type
    TChild = class (TBase)
        property beta : String;
    end;

type
    TSubChild = class (TChild)
        function Stringify : String;
        begin
            Result := '"hello"';
        end;
    end;
    
var a : array of TBase;

a.Add(new TBase);
a.Add(new TChild);
a.Add(new TSubChild);

PrintLn(JSON.Stringify(a));
