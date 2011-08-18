type TBase = class end;
type TChild = class(TBase) end;

procedure VarObj(var o : TObject);
begin
end;

procedure VarBase(var o : TBase);
begin
end;

procedure VarChild(var o : TChild);
begin
end;

var o : TObject;
var b : TBase;
var c : TChild;

VarObj(o);
VarObj(b);
VarObj(c);

VarBase(o);
VarBase(b);
VarBase(c);

VarChild(o);
VarChild(b);
VarChild(c);