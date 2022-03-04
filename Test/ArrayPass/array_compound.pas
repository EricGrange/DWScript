var d : array of Integer;
var s : array [0..1] of Float;

d.Add(1, 2);

var i := 0;
var j := 1;

d[i] += 10;
d[j] += 100;
s[i] += 10;
s[j] += 100;

d[i] -= 1;
d[j] -= 2;
s[i] -= 1;
s[j] -= 2;

d[i] *= 2;
d[j] *= 3;
s[i] *= 2;
s[j] *= 3;

s[i] /= 2;
s[j] /= 4;

PrintLn(d[0]);
PrintLn(d[1]);

PrintLn(s[0]);
PrintLn(s[1]);




