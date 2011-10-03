type
TMypropertyType = (semFirst,semSecond,semThird);

type
TMyClass = Class(Tobject)
private
  function getKind:String;
public
  property MyType:TMypropertyType read getKind write setKind;
  property ;
End;