function StrAddVar(AString : String; AVar : ComVariant) : String;
Begin

 Result := AString + Variant(AVar);

End;

operator << (string, ComVariant) : string uses StrAddVar;

var s := '';
var v : COMVariant := DispCallProxy;

s := s << v.method(1, 2, 3, 4, 5); // expected: method call and using result as right operand
println(s);