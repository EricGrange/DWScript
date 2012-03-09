type
   TMyClass = class
      FField : Integer;
      procedure SetField(v : Integer); begin FField:=v; end;
      property Indirect : Integer write SetField;
   end;

var o:=TMyClass.Create;

o.Indirect:=bug;