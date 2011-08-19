// comment
unit ScopeBase;

interface

type
   TBase = class
      procedure Hello; virtual;
      class method World; virtual;
      procedure Scope;
   end;
   
implementation

type
   TChild = class(TBase)
   end;

procedure TBase.Hello;
begin
   PrintLn('Base '+ClassName+' says Hello');
end;

class method TBase.World;
begin
   PrintLn('Base '+ClassName+' says World');
end;

procedure TBase.Scope;
var
   c : TChild;
begin
   c:=new TChild;
   c.Hello;
   c.World;
end;

end.