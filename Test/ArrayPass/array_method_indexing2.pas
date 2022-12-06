type TXY = array [0..1] of Integer;
var coords : array of TXY;
var i := 1;


coords.Add([3, 4], [5, 6]);

PrintLn(coords.Sort(lambda (a,b) => a[0].Compare(b[0]))[0][0]);
PrintLn(coords.Sort(lambda (a,b) => a[0].Compare(b[0]))[i][0]);

PrintLn(coords.Sort(lambda (a,b) => a[0].Compare(b[0])).Reverse[0][0]);
PrintLn(coords.Sort(lambda (a,b) => a[0].Compare(b[0])).Reverse[i][0]);

