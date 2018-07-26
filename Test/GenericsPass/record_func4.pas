type
    TTest<T> = record
        Field : T;
        function Add(a : T) : T;
        begin
            var v := a;
            Result := a + v; 
        end;
        function CompareDef(a : T) : T;
        begin
            if a > Default(T) then
                Result := a;
        end;
    end;

var i : TTest<Integer>;
PrintLn(i.Add(123));
PrintLn(i.CompareDef(1));
PrintLn(i.CompareDef(-1));

var s : TTest<String>;
PrintLn(s.Add('abc'));
PrintLn(s.CompareDef('a'));
PrintLn(s.CompareDef(''));

var f : TTest<Float>;
PrintLn(f.Add(1.25));
PrintLn(f.CompareDef(1.5));
PrintLn(f.CompareDef(-1.5));
