type TMyEnum = (enumOne, enumTwo);
type TMySet = set of TMyEnum;

function Test : TMySet; begin end;

Include(Test, enumOne);

Test.Exclude(enumTwo);
