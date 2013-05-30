type TMyEnum = (enumOne, enumTwo);
type TMySet = set of TMyEnum;

var s : TMySet;

Include(s, 1);

Include(s, '');

procedure Test; begin end;

Include(s, Test);
Include(s, @Test);