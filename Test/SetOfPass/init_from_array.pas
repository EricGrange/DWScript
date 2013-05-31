type TMyEnum = (enumOne, enumTwo);
type TMySet = set of TMyEnum;

var s : TMySet := [enumTwo];

if enumOne in s then PrintLn('bug1');
if enumTwo in s then PrintLn('ok1');

s := [enumOne];

if enumOne in s then PrintLn('ok2');
if enumTwo in s then PrintLn('bug2');

s := [enumOne, enumTwo];

if enumOne not in s then PrintLn('bug3');
if enumTwo not in s then PrintLn('bug4');

