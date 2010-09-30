var i : Integer := 2;

var j := i;

if i=j then
   PrintLn('match int');

var iStr = IntToStr(i);

if iStr='2' then
   PrintLn('match str');

var obj := TObject.Create;