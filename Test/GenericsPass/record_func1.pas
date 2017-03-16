type
    TTest<T> = record
        Dummy : Integer;
        class function Identity(a : T) : T;
        begin
            Result := a; 
        end;
    end;

var i : Integer := TTest<Integer>.Identity(123);
PrintLn(i);

var o1 := new TObject;
var o2 := TTest<TObject>.Identity(o1);

PrintLn(o1=o2);