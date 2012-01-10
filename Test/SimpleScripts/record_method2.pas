Type

 TRec = Record

  Field : String;
 
  Function GetName : String;
  Begin
     Result:='Hello '+Field;
  End;
  
  procedure SetExclamation;
  begin
     Field:=Field+'!';
  end;
 
 End;
 
Function GetRec : TRec;
Begin
   Result.Field:='World';
End;

PrintLn(GetRec.GetName);
var r:=GetRec;
r.SetExclamation;
PrintLn(r.GetName);