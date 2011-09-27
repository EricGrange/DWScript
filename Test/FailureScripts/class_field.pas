type TMY = class
      Field : Integer;
      property Prop : Integer read Field;
      class function Func : Integer;
   end;
   
class function TMY.Func : Integer;
begin
   Result:=Prop;
   Result:=Field;
end;
