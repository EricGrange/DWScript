PrintLn(Format('hello', []));
PrintLn(Format('hello %s', ['world']));
PrintLn(Format('hello %s %d', ['world', 123]));
PrintLn(Format('hello %s %d %.1f', ['world', 123, 12.5]));

var s = 'WORLD';
var i = 456;
var f = 3.14;

PrintLn(Format('hello %s', [s]));
PrintLn(Format('hello %s %d', [s, i]));
PrintLn(Format('hello %s %d %f', [s, i, f]));

