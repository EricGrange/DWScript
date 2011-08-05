function Func1 : String;
begin
   Result:='';
end;

procedure Proc;
begin
end;

function Func2(a, b : TObject) : String;
begin
   Result:='';
end;

function Func3(a : TObject; i : Integer) : String;
begin
   Result:='';
end;

operator + (Integer) : Integer uses Func1;
operator + (TObject, TObject) : String uses Func1;
operator + (TObject, TObject) : String uses Func2;
operator + (TObject, TObject) : String uses Func3;
operator + (Integer, Integer) : String uses Func2;

operator dummy ;