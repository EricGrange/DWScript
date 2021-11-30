function Test(const s : String) : String;
begin
   Result := 'test' + s;
end;


var a : array of String;
a.Map(Test);
