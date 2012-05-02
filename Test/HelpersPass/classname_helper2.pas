Type

 TClassHelper = Helper For TObject
 
  Function ClassName : String;
   
 End;
 
Function TClassHelper.ClassName : String;
Begin
  Result := 'Helper.' + inherited ClassName;
End;
 
PrintLn(TObject.Create.ClassName);