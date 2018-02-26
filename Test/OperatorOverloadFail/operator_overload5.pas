function Func1(var a : TObject; const b : TObject) : Integer;
begin
   Result:=0;
end;

function Func2(a : TObject; var b : TObject) : Integer;
begin
   Result:=0;
end;

operator + (TObject, TObject) : Integer uses Func1;
operator + (TObject, TObject) : Integer uses Func2;

operator + (TObject, TObject) : Integer uses Integer;
operator + (TObject, TObject) : Integer uses ;
operator + (TObject, TObject) : Integer ;