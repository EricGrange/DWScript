Type
 THelper1 = Helper For TObject
 Class Function ClassName : String;
 Begin
  Result := 'Helper1.' + ClassName;
 End;
 End;

PrintLn(TObject.Create.ClassName);
 
Type
 THelper2 = Helper For TObject
 Class Function ClassName : String;
 Begin
  Result := 'Helper2.' + ClassName;
 End;
 End;

PrintLn(TObject.Create.ClassName);