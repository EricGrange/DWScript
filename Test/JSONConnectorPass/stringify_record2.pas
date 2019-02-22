var s := 'hello';

PrintLn(JSON.Stringify(record
    a := s;
    b := True;
    c := False;
    d := s + s;
    e := 123;
    f := 12.5;
    g := Null;
end));