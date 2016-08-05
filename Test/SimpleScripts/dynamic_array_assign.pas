type TEnum = ( e1 = 1, e2, e3);

var s : array of String;

s := ['hello', 'world'];
PrintLn(s.Join(','));
s := [];
PrintLn(s.Join(','));

var i : array of Integer;

i := [1, 2];
PrintLn(i.Length.ToString+','+i[0].ToString+','+i[1].ToString);
i := [e2, e3];
PrintLn(i.Length.ToString+','+i[0].ToString+','+i[1].ToString);

var e : array of TEnum;

e := [1, 3];
PrintLn(e.Length.ToString+','+e[0].Name+','+e[1].Name);
e := [e2, e1];
PrintLn(e.Length.ToString+','+e[0].Name+','+e[1].Name);



