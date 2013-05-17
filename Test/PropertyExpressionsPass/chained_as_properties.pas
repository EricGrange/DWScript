type TClassA = class
  end;
type TClassASub = class (TClassA)
  private
    FProp: Integer;
  public
    property Prop: Integer read FProp write FProp;
  end;

type TClassB = class
  public
    FAClass: TClassA;
    property Prop: Integer read ((FAClass as TClassASub).Prop) write ((FAClass as TClassASub).Prop);
end;

var a := TClassASub.Create;
a.Prop:=123;

var b := TClassB.Create;
b.FAClass := a;

PrintLn(b.Prop);

b.Prop:=456;

PrintLn(b.Prop);