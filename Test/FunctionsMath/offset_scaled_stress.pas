procedure Stress;
begin
   var a1 : array of Float;
   var a2 : array of Float;
   
   // Test various sizes and alignments
   for var count := 1 to 64 do begin
      for var o1 := 0 to 8 do begin
         for var o2 := 0 to 8 do begin
             a1 := []; a2 := [];
             for var i := 0 to 128 do begin a1.Add(i); a2.Add(1); end;
             
             var scale := 0.5;
             a1.OffsetScaled(a2, scale, o1, o2, count);
             
             for var i := 0 to count-1 do begin
                if Abs(a1[o1+i] - (o1+i + scale)) > 1e-9 then begin
                   PrintLn('Error at o1=' + IntToStr(o1) + ' o2=' + IntToStr(o2) + ' count=' + IntToStr(count) + ' i=' + IntToStr(i));
                   Exit;
                end;
             end;
         end;
      end;
   end;
   PrintLn('Stress complete');
end;

Stress;
