type TMyEnum = (enumOne, enumTwo);
type TMySet = set of TMyEnum;

var s : TMySet;

s := s + [ TMyEnum(3) ];

PrintLn(Integer(s * [ TMyEnum(0) ]));

PrintLn(Integer(s * [ TMyEnum(-1) ]));

