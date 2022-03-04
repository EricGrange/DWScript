type
  TRec = record
    name: string;
    id: integer;
    //why doesn't constructor syntax work here?!?
    class function Create(id: integer; name: string): TRec;
  end;

class function TRec.Create(id: integer; name: string): TRec;
begin
  result.id := id;
  result.name := name;
end;

procedure Testing;
var
  recs: array of TRec;
  r : Trec;
begin
  recs.add(TRec.Create(1, 'A'));
  r:=TRec.Create(2, 'b');
  recs.Add(r);
  
  print(recs[0].id);
  print(recs[0].name);
  println(format('%s', [recs[0].name]));
  print(recs[1].id);
  print(recs[1].name);
  println(format('%s', [recs[1].name]));
end;

testing;