type StringArray = array of String;

procedure Test(const AOp: Integer; const AValues: StringArray); overload;
begin
  for var I:= Low(AValues) to High(AValues) do begin
    Print(' ');
    PrintLn(AValues[I]);
  end;
end;

procedure Test(const AOp: Integer; const AValue: String); overload;
begin
  Print(AValue);
end;


const
  V = ['flower'];

Test(1, 'lily');
Test(2, V);
Test(3, ['bucket']);
