type 
    TTest<T> = class
        A : array of T;
        function Test(const v : T) : T;
    end;
    
function TTest<T>.Test(const v : T) : T;
begin
    repeat
        Result += A.Peek;
    until A.Pop = v;
end;

var s := new TTest<String>;
s.A := ['a', 'b', 'c'];
PrintLn(s.Test('a'));
       
var i := TTest<Integer>.Create;
i.A := [1,2,3,4];
PrintLn(i.Test(2));
    
