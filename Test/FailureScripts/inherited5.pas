type
   TBase = class
      Field : Integer;
      property Prop : Integer write Field;
   end;
   
type
   TChild = class(TBase)
      function Func : Integer;
   end;
   
function TChild.Func : Integer;
begin
   Result := inherited Prop;
end;
