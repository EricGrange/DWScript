var test := JSON.Parse('"hello"');
WriteGlobalVar('test', test);

PrintLn(ReadGlobalVar('test'));

test := JSON.Parse('["hello"]');
WriteGlobalVar('test', test);

PrintLn(ReadGlobalVar('test'));
