type TMyEnum = (enumOne, enumTwo);
type TMySet = set of TMyEnum;

var s : TMySet;

if enumOne in s then PrintLn('bug1');
if enumTwo in s then PrintLn('bug2');

s.Include(enumOne);

if enumOne in s then PrintLn('ok1');
if enumTwo not in s then PrintLn('ok2');

s.Include(enumTwo);

if enumOne in s then PrintLn('ok3');
if enumTwo in s then PrintLn('ok4');

s.Exclude(enumOne);

if enumOne in s then PrintLn('bug3');
if enumTwo in s then PrintLn('ok5');
