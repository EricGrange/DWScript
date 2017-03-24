var a : array of Integer := [1, 3, 2];

var s := a.Map(lambda (e) => '(' + e.ToString + ')');

PrintLn(s.Join(','));