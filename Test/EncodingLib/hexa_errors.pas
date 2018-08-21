procedure TryDecode(s : String);
begin
   try
      PrintLn(HexadecimalEncoder.Decode(s)); 
   except
      on E : Exception do 
         PrintLn(E.Message + ' for "' + s + '"');
   end;
end;

TryDecode('');
TryDecode('1');
TryDecode('z1');
TryDecode('1z');
TryDecode('Aaz1');
TryDecode('Aa1z');
