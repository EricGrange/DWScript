var i : Integer;
var o : TObject;
var b : Boolean;

b:=(i is TObject);
b:=(o is i);
(i as TObject).Free;
PrintLn(Variant(o as i));
b:=(o=i);
b:=(o<o);
b:=(o<>i);
