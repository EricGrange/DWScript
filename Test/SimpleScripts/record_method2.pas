Type

 TRec = Record

  Field : String;
 
  Function GetName : String;
  Begin
     Result:='Hello '+Field;
  End;
 
 End;
 
Function GetRec : TRec;
Begin
   Result.Field:='World';
End;

PrintLn(GetRec.GetName);