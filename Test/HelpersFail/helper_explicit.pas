type
   TDummy = helper for Integer 
      function Next : Integer; begin Result:=Self+1; end;
      class function Two : Integer; begin Result:=2; end;
   end;

var i := (1).Next;

i:=TDummy.Next(2);
i:=TDummy.Two;

TDummy.Next;