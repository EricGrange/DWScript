type TXY = array [0..1] of Integer;
type TLine = array [0..1] of TXY;

var s : TLine = [ [ 1, 2 ], [ 3, 4 ] ];

var d : array of TXY;
d := s;

PrintLn(d.Length);
PrintLn(d[0][0]);
PrintLn(d[0][1]);
PrintLn(d[1][0]);
PrintLn(d[1][1]);