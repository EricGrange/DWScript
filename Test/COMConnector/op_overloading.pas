function StrAddVar1(AString : String; AVar : ComVariant) : String;
Begin

 Result := 'Ole> '+ AString + Variant(AVar);

End;

function StrAddVar2(AString : String; AVar : Variant) : String;
Begin

 Result := 'Var> '+ AString + AVar;

End;

operator << (string, ComVariant) : string uses StrAddVar1;
operator << (string, Variant) : string uses StrAddVar2;

var s := '';

var v : OleVariant := DispCallProxy;
s := s << v.method(1, 2, 3, 4, 5); // expected: method call and using result as right operand
println(s);

s := '' << Variant(v.method('a', 'b', 'c')); // expected: method call and using result as right operand
println(s);