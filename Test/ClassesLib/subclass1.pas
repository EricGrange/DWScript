procedure Process(List: TStringList);
var
   i: Integer;
begin
   for i := 0 to List.Count-1 do
      println(Format('%d. %s', [i, List[i]]));
end;

type
   TMyStringList = class(TStringList)
   end;

var MyList = TMyStringList.Create;
var i: Integer;

for i := 1 to 10 do
   MyList.Add(IntToStr(i));

Process(MyList);

