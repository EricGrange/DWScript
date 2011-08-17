type
   TBase = class
      procedure Proc1(a : Integer);
   end;
   
type
   TChild = class(TBase)
      procedure Proc2;
   end;   
   
procedure TChild.Proc2;
begin
   inherited Proc1(.);
end;

