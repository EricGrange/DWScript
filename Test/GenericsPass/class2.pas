type 
    TTest<T> = class

        class function Identity(v : T) : T;
        begin
            Result := v;
        end;
        class function Sum(v, w : T) : T;
        begin
            Result := v + w;
        end;
        class function SumN(v : T; n : Integer) : T;
        begin
            Result := v + n;
        end;

    end;
PrintLn(TTest<Integer>.Identity(10));
PrintLn(TTest<Integer>.Sum(10, 2));
PrintLn(TTest<Integer>.SumN(10, 3));

PrintLn(TTest<Float>.Identity(1.5));
PrintLn(TTest<Float>.Sum(1.5, 2.25));
PrintLn(TTest<Float>.SumN(1.5, 3));

