Type

 TBaseClass = Class
 
  Constructor Create;// Virtual;
  Begin
  
   PrintLn(ClassName);
  
  End;
  
 End;
 
Type

 TDescendant = Class(TBaseClass)
 End; 

Type

 TClassRef = Class Of TBaseClass;
 
Var

 cr : TClassRef;
 
cr := TDescendant;
cr.Create;  