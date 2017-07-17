var i := 123;

PrintLn(JSON.Stringify(
    record
        "is" := i.ToString;
        "2i" := 2*i;
    end)
);

var r := record
    "i*i" := i * i;
    "i-1" := (i-1).ToString;
end;
PrintLn(JSON.Stringify(r));