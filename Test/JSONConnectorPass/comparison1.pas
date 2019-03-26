PrintLn('array');

PrintLn(JSON.Parse('[0]')[0] = 0);
PrintLn(JSON.Parse('[0]')[0] = '0');
PrintLn(JSON.Parse('["0"]')[0] = '0');
PrintLn(JSON.Parse('["0"]')[0] = 0);
PrintLn(JSON.Parse('[0]')[0] = JSON.Parse('["0"]')[0]);

PrintLn('object');

PrintLn(JSON.Parse('{"test":0}').test = 0);
PrintLn(JSON.Parse('{"test":0}').test = '0');
PrintLn(JSON.Parse('{"test":"0"}').test = '0');
PrintLn(JSON.Parse('{"test":"0"}').test = 0);

PrintLn('value');

PrintLn(JSON.Parse('0') = 0);
PrintLn(JSON.Parse('0') = '0');
PrintLn(JSON.Parse('"0"') = '0');
PrintLn(JSON.Parse('"0"') = 0);

PrintLn('variant');

var v : Variant = 1;

PrintLn(v = JSON.Parse('0'));
PrintLn(JSON.Parse('1') <> JSON.Parse('0'));
v := JSON.Parse('0');
PrintLn(v = v);