Type

 TObj = class
 
  Function FF : String; // must be class function
  Begin
  
   Result := 'test';
  
  End;
 
  Class Property Func : Function : String Read (FF);
 
 End;
 
PrintLn(TObj.Func());