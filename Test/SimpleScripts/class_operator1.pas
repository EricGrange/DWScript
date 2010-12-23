type TTest = class
   Field : String;
   procedure AppendString(str : String);
   class operator += String uses AppendString;
end;

type TSubTest = class(TTest) end;

procedure TTest.AppendString(str : String);
begin
   Field:=Field+str+',';
end;

var t = TTest.Create;

t.AppendString('first');
PrintLn(t.Field);

t += 'second';
PrintLn(t.Field);

var st = TSubTest.Create;

st.Field:='Hello ';
st += 'World';

PrintLn(st.Field);