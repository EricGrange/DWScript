type 
   IIntf = Interface 
      ['fake GUID to be ignored']
      procedure Hello(s : bug);
	  function World(i : bugbug) : Integer;
	  property Prop : Integer read World write Hello;
   end;

   