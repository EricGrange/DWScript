type
   TMyClass = class
      FField : Integer;
      property Direct : Integer write FField;
   end;

var o:=TMyClass.Create;

o.Direct := bug;