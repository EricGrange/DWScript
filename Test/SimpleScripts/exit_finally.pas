function Hello : String;
begin
   Result:='duh';
   try
      Result:='Hello';
      if True then Exit;
   finally
      Result:=Result+' world';
   end;
   Result:='Bye bye';
end;

PrintLn(Hello);

function Test(i : Integer) : Boolean;
var
   ts : TObject;
begin
   Result := False;
   ts := TObject.Create;
   try
      if (i < 10) then Exit;
   finally
      ts.Free;
   end;
   Result := True;
end;

//if not Test(10) then PrintLn('Test bugged 10');
if Test(5) then PrintLn('Test bugged 5');

try
   PrintLn('Try');
   Exit;
finally
   PrintLn('Finally');
end;
PrintLn('Bug');
