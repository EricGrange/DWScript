Type

 TObj = Class
 
  Const Value = 5;
  
  FField := Value; // TObj.Value - OK
  
 End;

Type

 TObj2 = Class

  Private
   
   Const Value = 6;

  public

   FField := TObj2.Value;
 
  
 End;

PrintLn(new TObj.FField);
PrintLn(new TObj2.FField);