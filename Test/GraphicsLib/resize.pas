
procedure Test(x, y : Integer);
begin
   var pixmap := CreatePixmap(2, 2);
   pixmap.SetPixel(0, 0, $A1A2A3A4);
   pixmap.SetPixel(1, 0, $B1B2B3B4);
   pixmap.SetPixel(0, 1, $C1C2C3C4);
   pixmap.SetPixel(1, 1, $D1D2D3D4);
   pixmap.Resize(x, y);
   Print(x.ToString + ',' + y.ToString + ': ');
   for var py := 0 to y-1 do begin
      if py > 0 then Print('     ');
      for var px := 0 to x-1 do begin
         var c := pixmap.GetPixel(px, py);
         Assert((c and $0F0F0F0F) = $01020304, 'failed ' + px.ToString + ',' + py.ToString);
         Print(((c shr 4) and $F).ToHexString(1));
      end;
      PrintLn('');
   end;
end;

Test(2, 2);
Test(4, 4);
Test(4, 2);
Test(2, 4);
Test(1, 1);
Test(2, 1);
Test(1, 2);
Test(4, 1);
Test(1, 4);
