type TMY = class
   end;
   
constructor TMy.Create;
begin
   inherited Create;
end;

constructor TMy.Destroy;
begin
   inherited Destroy;
end;

method TMY.Stuff;
begin
end;

class function TMY.Func : Integer;
begin
   Result:=inherited Func;
end;

