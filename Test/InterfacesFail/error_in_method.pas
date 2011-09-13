Type

 IIntf = Interface
 
  Procedure Proc;
  
 End;
 
Type

 TObj = Class(TObject, IIntf)
 
  Procedure Proc;
  Begin
  
   Format(''); // syntax error here
   
  End;
 
 End;
 
Var i : IIntf := TObj.Create;
