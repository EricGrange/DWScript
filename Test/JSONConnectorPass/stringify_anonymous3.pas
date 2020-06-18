PrintLn(JSON.Stringify(
    record
        a := [];
        b := [ [] ];
        c := [ [], [] ];
    end)
);

var ra := record
   a := 1;
   b := [];
end;
var rb := record
   a := [];
   b := 2;
end;

var ra1 := ra;
var rb1 := rb;

PrintLn(JSON.Stringify(ra));
PrintLn(JSON.Stringify(ra1));
PrintLn(JSON.Stringify(rb));
PrintLn(JSON.Stringify(rb1));

   
