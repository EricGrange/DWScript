type TMyEnum = (enumOne, enumTwo);
type TMySet = set of TMyEnum;
type TRecord = record
	A : TMySet;
	B : TMySet;
end;

var r : record F : TMySet end  = (F: [enumTwo]);

if enumOne in r.F then PrintLn('bug1');
if enumTwo in r.F then PrintLn('ok1');

r.F := [enumOne];

if enumOne in r.F then PrintLn('ok2');
if enumTwo in r.F then PrintLn('bug2');

const rA : TRecord = (A: [enumOne]);
const rB : TRecord = (B: [enumTwo]);

if enumOne in rA.A then PrintLn('ok3');
if enumTwo in rA.A then PrintLn('bug3');
if enumOne in rA.B then PrintLn('bug4');
if enumTwo in rA.B then PrintLn('bug5');

if enumOne in rB.A then PrintLn('bug6');
if enumTwo in rB.A then PrintLn('bug7');
if enumOne in rB.B then PrintLn('bug8');
if enumTwo in rB.B then PrintLn('ok4');
