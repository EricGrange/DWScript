type
   TBase = class
      function Func : Integer; virtual;
   end;
   
function TBase.Func : Integer;
begin
   Result := inherited ClassType[];
end;
