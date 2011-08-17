type 
   TBase = class
      procedure Hello; virtual; abstract;
      class method HelloClass;
   end;
   
type 
   TChild = class(TBase)
   end;
   
class procedure TChild.Hello;
begin
end;

procedure TBase.HelloClass;
begin
end;