type 
    TTest<T> = class
        A : array of T;
        constructor Create(n : Integer);
    end;
    
constructor TTest<T>.Create(n : Integer);
begin
    while A.Length < n do
        A.Add(Default(T));
end;

var s := new TTest<String>(4);
PrintLn(s.A.Join(','));
       
var i := new TTest<Integer>(4);
PrintLn(i.A.Map(IntToStr).Join(','));
    
