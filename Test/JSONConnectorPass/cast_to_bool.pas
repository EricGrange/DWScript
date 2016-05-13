var v : JSONVariant;
if not v then PrintLn('unassigned is false');

v := JSON.Parse('null');
if not v then PrintLn('null is false');

v := JSON.Parse('{}');
if v then PrintLn('object is true');

v := JSON.Parse('[]');
if v then PrintLn('array is true');

v := JSON.Parse('"a"');
if v then PrintLn('non empty string is true');
v := JSON.Parse('""');
if not v then PrintLn('empty string is false');

v := JSON.Parse('1');
if v then PrintLn('1 is true');
v := JSON.Parse('0');
if not v then PrintLn('0 is false');

v := JSON.Parse('true');
if v then PrintLn('true is true');
v := JSON.Parse('false');
if not v then PrintLn('false is false');
