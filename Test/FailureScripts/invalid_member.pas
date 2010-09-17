type TDummy = class
  Field : TObject;
  procedure Method;
end;

procedure TDummy.Method;
begin
   Field.Method;
end;