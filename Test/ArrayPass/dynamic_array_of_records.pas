type TMyRec = record
      V : Integer;
      N : String;
   end;

type TMyRecs = array of TMyRec;

procedure PrintRecs(recs : TMyRecs);
var
   i : Integer;
begin
   PrintLn(recs.Length());
   for i:=recs.Low() to recs.High() do
      PrintLn(IntToStr(recs[i].V)+': '+recs[i].N);
end;

procedure AddRec(recs : array of TMyRec; v : Integer; n : String);
var
   k : Integer;
begin
   k:=recs.Length;
   recs.SetLength(k+1);
   recs[k].V:=v;
   recs[k].N:=n;
end;

var recs := new TMyRec[0];

PrintRecs(recs);

AddRec(recs, 2, 'one');

PrintRecs(recs);

AddRec(recs, 2, 'two');

recs.SetLength(3);

PrintRecs(recs);

recs.SetLength(1);

PrintRecs(recs);

const cRec : TMyRec = (V: 123; N: 'bye');

recs[0]:=cRec;
recs[0].N:='BYE';
recs.Add(cRec);

PrintRecs(recs);

recs.Delete(0);

PrintRecs(recs);