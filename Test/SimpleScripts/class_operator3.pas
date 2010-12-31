type
   toa = array of const;

type TTest = class
   Field : String;
   procedure AppendStrings(const str : toa);
   class operator += toa uses AppendStrings;
end;

procedure TTest.AppendStrings(const str : array of const);
var
   i : Integer;
begin
   for i:=0 to High(str) do
      Field+=str[i];
end;

var t = TTest.Create;

t += [1, 2];
PrintLn(t.Field);

t += ['a', 'b', 'c'];
PrintLn(t.Field);

t += [];
PrintLn(t.Field);

