type 
    TTest<A> = class external
        Field : A;
        function Hello(p : A) : A;
    end;

var i : TTest<Integer>;
var j : TTest<Integer>;
    
j := i;
PrintLn(j = nil);