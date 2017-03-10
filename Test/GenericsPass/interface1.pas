type 
    ITest<T> = interface
        function GetP : T;
        procedure SetP(v : T);
        property P : T read GetP write SetP;
    end;
    
type
    TTest = class (ITest<Integer>)
        Field : Integer;
        function GetP : Integer; begin Result := Field; end;
        procedure SetP(v : Integer); begin Field := v; end;
    end;
    
var o := new TTest;
o.Field := 123;

var i := o as ITest<Integer>;

PrintLn(i.GetP);
i.SetP(456);
PrintLn(i.P);
i.P := 789;
PrintLn(o.Field);
