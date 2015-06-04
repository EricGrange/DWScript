var v : Variant;

if v = nil then PrintLn('Ok1');
if v <> nil then PrintLn('Bug1');

v := 0;

if v = nil then PrintLn('Bug2');
if v <> nil then PrintLn('Ok2');

v := Null;

if v = nil then PrintLn('Ok3');
if v <> nil then PrintLn('Bug3');

