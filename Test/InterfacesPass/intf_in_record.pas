Type

 IIntf = Interface
 
  Procedure Proc;
  
 End;
 
Type

 TRec = Record
 
  FIntf : IIntf;
  
  Procedure Proc;
  Begin
  
   FIntf.Proc;
  
  End;
 
 End;
 
Var r : TRec;

r.Proc;