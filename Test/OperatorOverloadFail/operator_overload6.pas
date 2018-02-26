function Func1(a : TObject; const b : TObject) : Integer;
begin
   Result:=0;
end;

function Func2(a : TObject; b : TObject) : Integer;
begin
   operator + (TObject, TObject) : Integer uses Func1;

   Result:=0;
end;

begin 
   operator + (TObject, TObject) : Integer uses Func2;
end;

operator + (TObject, TObject) : Integer uses Func2;
operator + (TObject, TObject) : Integer uses Func2;