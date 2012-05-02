Type

 TClassHelper = Helper For TObject
 
  Function ClassName : String;
  Begin
   Result := 'Helper.' + Self.ClassName;
  End;
 
 End;
 
PrintLn(TObject.Create.ClassName);