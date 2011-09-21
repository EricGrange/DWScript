Var v : ComVariant := DispCallProxy;

If v.method() = '' Then
   PrintLn('empty')
else PrintLn('not empty');

If v.method(123) = '' Then
   PrintLn('empty')
else PrintLn('not empty: '+v.method(123));