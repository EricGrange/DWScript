PrintLn(JSON.Stringify(
    record
        "r" := (Random*0).ToString;
    end)
);

var r := record
    "r" := (Random*0).ToString;
end;
PrintLn(JSON.Stringify(r));