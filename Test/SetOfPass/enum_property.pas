type
  TEnum = (eOne = 1, eTwo, eThree, eFor);

type
  TEnumSet = set of TEnum;

type
  TMyClass = class
  private
    v: Set of TEnum;
    procedure setEnum(value: TEnumSet);
    function getEnum(): TEnumSet;
  public
    property Val: Set of TEnum read getEnum write setEnum;
  end;

procedure TMyClass.setEnum(value: TEnumSet);
begin
 v:= value;
end;

function TMyClass.getEnum(): TEnumSet;
begin
 Result := v;
end;

var m := new TMyClass;
m.Val := [ eTwo ];
PrintLn(eOne in m.Val);
PrintLn(eTwo in m.Val);