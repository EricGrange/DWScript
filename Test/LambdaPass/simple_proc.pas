var p : procedure;

p := lambda PrintLn('hello') end;
p();

var pp : procedure (str : String);

pp := lambda (v) PrintLn(v) end;
pp('world');

pp := lambda (a) begin PrintLn(a+'!') end end;
pp('bye');