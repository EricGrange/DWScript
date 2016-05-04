var test := JSON.Parse('{"test":"hello","num":123}');

PrintLn(test.test);

test.test += ' world';

PrintLn(test.test);

test.num += 456;

PrintLn(test.test);
