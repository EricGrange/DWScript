type
   TTest = record
      Field : Integer;
      class function Sum(const a, b : TTest) : TTest; overload;
      class function Sum(const a : TTest; i : Integer) : TTest; overload;
      class function Create(i : Integer) : TTest; static;
   end;

class function TTest.Sum(const a, b : TTest) : TTest;
begin
   Result.Field:=a.Field+b.Field;
end;

class function TTest.Sum(const a : TTest; i : Integer) : TTest;
begin
   Result:=Sum(a, TTest.Create(i));
end;

class function TTest.Create(i : Integer) : TTest;
begin
   Result.Field:=i;
end;

var r := TTest.Sum(TTest.Create(10), 2);

PrintLn(r.Field);
   
