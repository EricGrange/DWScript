// comment
unit ScopeChild;

interface

uses ScopeBase;

type
   TChild = class(TBase)
      procedure Hello; override;
      class method World; override;
   end;
   
implementation

procedure TChild.Hello;
begin
   PrintLn('Child '+ClassName+' says Hello');
end;

class method TChild.World;
begin
   PrintLn('Child '+ClassName+' says World');
end;

end.