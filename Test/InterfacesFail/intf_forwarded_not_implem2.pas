type IIntf = Interface;

type
   IIntf2 = interface(IIntf)
   end;

type 
   TMyClass = class(TObject, IIntf)
   end;
   
type IIntf = interface
   end;