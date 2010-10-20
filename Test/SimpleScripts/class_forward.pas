type TChild = class;

type TBase = class
   function Stuff : TChild; virtual; abstract;
end;

type TChild = class (TBase)
   function Stuff : TChild; override;
   procedure PrintHello;
   class procedure PrintWorld; virtual;
end;

type TChild2 = class (TChild)
   function Stuff : TChild; override;
   class procedure PrintWorld; override;
end;

function TChild.Stuff : TChild;
begin
   Result:=Self;
end;

procedure TChild.PrintHello;
begin
   Print('Hello ');
end;

class procedure TChild.PrintWorld;
begin
   PrintLn('World');
end;

function TChild2.Stuff : TChild;
begin
   Result:=inherited Stuff;
end;

class procedure TChild2.PrintWorld;
begin
   Print('My ');
   inherited PrintWorld;
end;

var o : TBase;
o:=TChild.Create;

var c = o.Stuff;
c.PrintHello;
o.Stuff.PrintWorld;

o:=TChild2.Create;
c:=o.Stuff;
c.PrintHello;
o.Stuff.PrintWorld;


