Function IntToStr(Const A : Variant) : String; Overload;
Begin
   Result:='Overloaded '+VarToStr(A);
End;

PrintLn(IntToStr(1));
PrintLn(IntToStr('abc'));
