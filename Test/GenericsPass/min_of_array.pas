type 
    TMy<T> = record
        A : array of T;

        function Min : T;
        begin
           if Length(A) > 0 then begin
              Result := A[0];
              for var i := 1 to A.High do
                 if A[i] < Result then
                    Result := A[i];
           end;
        end;

        function Max : T;
        begin
           // yes, it's not a correct Max implementation
           // ...it's a test, not a library :)
           var item : T; 
           for item in A do
              if item > Result then 
                 Result := item;
        end;
    end;

var i : TMy<Integer>;
PrintLn(i.Min);
i.A := [2, 1, 3];
PrintLn(i.Min);
PrintLn(i.Max);

var s : TMy<String>;
PrintLn(s.Min);
s.A := ['def', 'xyz', 'abc'];
PrintLn(s.Min);
PrintLn(s.Max);
