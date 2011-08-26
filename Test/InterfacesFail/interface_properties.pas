type
   IMyIntf = interface
       function GetHello : String;
       method SetHello(v : String);
       
       method GetItem(s : String) : Integer;
       method SetItem(s : String; i : Integer);
       
       property Hello1 : Integer read SetHello write GetHello;
       property Hello2 : Integer read GetHello write SetHello;
       
       property Items1[s : Integer] : String read SetItem write GetItem; default;
       property Items2[s : Integer] : String read GetItem write SetItem; default;
       
    end;
      