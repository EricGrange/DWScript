var i : Integer;
var o : TObject;
var c : TClass;
var b : Boolean;

b:=(i is TObject);
b:=(o is i);
PrintLn(Variant(o as i));
b:=(o=i);
b:=(o<o);
b:=(o<>i);
o:=(c as TObject);
c:=(o as TClass);
c:=(c as o);
(i as TObject).Free;

