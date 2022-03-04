function func : integer;
begin
   Result:=123;
end;

procedure test(const aparams : Array Of function : integer);
begin
   PrintLn(aparams[0]);
end;

test([@func]); // OK

procedure Hello;
begin
   PrintLn('Hello');
end;

function func2 : procedure;
begin
   Result:=Hello;
end;

procedure test2(const aparams : Array Of function : procedure);
begin
   aparams[0]();
end;

test2([@func2]); // error