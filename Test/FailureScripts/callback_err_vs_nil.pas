type TSomeCallback = procedure( x, y: Integer );

procedure CBmethod( x, y:Integer );
begin
  if( x <> 1000 ) and ( y >= 20 ) then
    x := 1000;
  PrintLn(x.ToString + ' ' + y.ToString);
end;

function Methodwithcalback_good( x, y:Integer; callback:TSomeCallback ):Boolean;
begin
  if Assigned(callback) then
  begin
    callback(x,y);
  end;
  Result := True;
end;

function Methodwithcalback_bad( x, y:Integer; callback:TSomeCallback ):Boolean;
begin
  if callback <> nil then
  begin
    callback(x,y);
  end;
  Result := false;
end;

begin
  Methodwithcalback_good(50,100,CBmethod);
  Methodwithcalback_bad(50,100,CBmethod);
end.