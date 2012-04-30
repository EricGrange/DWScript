type
   TIntArray = array of Integer;

type
   THelper = helper for TIntArray
      class function Series(n : Integer) : TIntArray; overload;
      class function Series(b, n : Integer) : TIntArray; overload;
      procedure Print; overload;
      procedure Print(b, e : String); overload;
   end;
   
class function THelper.Series(n : Integer) : TIntArray;
begin
   Result:=Series(0, n);
end;

class function THelper.Series(b, n : Integer) : TIntArray;
var
   i : Integer;
begin
   Result.SetLength(n);
   for i:=0 to n-1 do
      Result[i]:=b+i;
end;

procedure THelper.Print;
var
   i : Integer;
begin
   for i:=Self.Low to Self.High do
      PrintLn(Self[i]);
end;

procedure THelper.Print(b, e : String);
begin
   PrintLn(b);
   Print;
   PrintLn(e);
end;

var a := TIntArray.Series(3);
a.Print('[', ']');
TIntArray.Series(1, 3).Print;

