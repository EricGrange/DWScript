type
    TTest<T> = record
        Field : T;
        function Add(a : T) : T;
        begin
            Result := a + Field; 
        end;
        function Sub(const a : T) : T;
        begin
            Field := Field - a; 
            Result := Field;
        end;
    end;

var i : TTest<Integer>;
i.Field := 123;
PrintLn(i.Add(456));
PrintLn(i.Sub(111));
PrintLn(i.Add(321));

