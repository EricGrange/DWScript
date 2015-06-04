var v : ComVariant;

if v = nil then PrintLn('Ok1');
if v <> nil then PrintLn('Bug1');

v := CreateOleObject('Shell.Application');

if v = nil then PrintLn('Bug2');
if v <> nil then PrintLn('Ok2');

v := v.NameSpace('thisdoesnotexists');

if v = nil then PrintLn('Ok3');
if v <> nil then PrintLn('Bug3');

v := Null;

if v = nil then PrintLn('Ok4');
if v <> nil then PrintLn('Bug4');

