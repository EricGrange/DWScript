type
   IBase = interface
     function GetField: integer;
     procedure SetField(value: integer);
     property Field: integer read GetField write SetField;
     property MultBy2: integer read (Field*2);
   end;
  
type
  TBase = class(IBase)
     FField: integer := 1;
     function GetField: integer;
     procedure SetField(value: integer);
     property Field: integer read GetField write SetField;
     property MultBy2: integer read (GetField*42);
  end;
  
function TBase.GetField: integer;
begin
  Result := FField;
end;

procedure TBase.SetField(value: integer);
begin
  FField := value;
end;

var cls := new TBase;
var intf : IBase := TBase.Create;
  
PrintLn(cls.Field);
PrintLn(intf.Field);
PrintLn(cls.MultBy2);
PrintLn(intf.MultBy2);