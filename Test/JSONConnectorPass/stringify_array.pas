var ad : array of Integer;
ad.Add(1, 2, 3);
PrintLn(JSON.Stringify(ad));

var sd : array of String;
sd.Add('hello', 'world');
PrintLn(JSON.Stringify(sd));

type TFloat2 = array [1..2] of Float;

var a2 : TFloat2;
a2:=[1.5, 2.5];

PrintLn(JSON.Stringify(a2));

var aa : array of TFloat2;

aa.Add(a2);
a2[1]:=0.5;

aa.Add(a2);

PrintLn(JSON.Stringify(aa));
