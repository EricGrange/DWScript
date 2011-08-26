type IIntf1 = Interface 
      method Hello;
      method GetWorld : Integer;
      method SetWorld(i : Integer);
      
      property World : Integer read GetWorld write SetWorld;
     
   end;
   
type IIntf2 = Interface(IIntf1 end;