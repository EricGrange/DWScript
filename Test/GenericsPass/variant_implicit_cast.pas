type 
    TTest<T> = class
        A : array of T;
        constructor Create(n : Integer);
    end;
    
constructor TTest<T>.Create(n : Integer);
begin
    for var i := 1 to n do
        A.Add(Variant(i));
    for var i := 10 to n*10 step 10 do
        A.Add(Variant(i));
end;

var s := new TTest<String>(4);
PrintLn(s.A.Join(','));
       
var i := new TTest<Integer>(4);
PrintLn(i.A.Map(IntToStr).Join(','));
    
