var p : function : Integer;

p := lambda => 123;
PrintLn(p());

p := lambda result := 450+6 end;
PrintLn(p());

var pp : function (i : Integer) : Integer;

pp := lambda (v) => v*2;
PrintLn(pp(11));

var inferenced := lambda (t : Integer) => t*3;
PrintLn(inferenced(22));
