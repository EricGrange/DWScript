type TUnitInfos = record
  published
     Data1 : String;
     Prec : Integer;
end;
type TItemInfos = record
  published
     Data1, Data2 : Integer ;
     Ref, Data3 : String;
     UnitInfos : TUnitInfos;
end;
type TInfos = record
  published
     Data1 : Integer;
     Data2 : Float;
     Data3 : String;
     Item : TItemInfos;
end;

var t : array of TInfos ;
t.SetLength(1);
t[0].Item.UnitInfos.Prec := 4;

PrintLn(t[0].Item.Ref);
PrintLn(t[0].Item.UnitInfos.Prec);
