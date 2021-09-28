type TRec = record
   published
      N : Integer;
      S : String;
      class function Create(k : Integer) : TRec; begin Result.N := k; Result.S := k.ToHexString(2); end;
      function ToString : String; begin Result := N.ToString + ',' + S; end;
end;

var a : array [Integer] of TRec;

procedure PrintA;
begin
   for var k in a.Keys.Sort do
      Print('_' + k.ToString + ':' + a[k].ToString);
   PrintLn('');
end;


a[1] := TRec.Create(1);
a[2] := TRec.Create(2);
a[3] := TRec.Create(3);

a.Delete(2);

PrintA;

a[2] := TRec.Create(22);
a[4] := TRec.Create(4);
a.Delete(3);

PrintA;

a.Delete(1);

PrintA;

a.Delete(11);

PrintA;

