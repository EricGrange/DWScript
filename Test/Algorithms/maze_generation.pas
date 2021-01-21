SetRandSeed(123456789);

procedure MakeMaze(size : Integer);
require
   size >= 1;
begin
   var cells : array of Boolean;
   type TMovement = record
      Start : Integer;
      Direction : Integer;
   end;
   var stack : array of TMovement;
   var m : TMovement;

   var n := 2*size + 1;
   cells.SetLength(Sqr(2*size + 1));
   var toVisit := size*size;

   var x := RandomInt(size);
   var y := RandomInt(size);
   var p := (1 + 2*x) + (1 + 2*y) * n;

   while True do begin
      x := (p mod n) div 2;
      y := (p div n) div 2;
      cells[p] := True;
      toVisit -= 1;
      if toVisit = 0 then break;

      var directions : array of Integer = [];
      if (x > 0) and not cells[p-2] then
         directions.Add(-2);
      if (y > 0) and not cells[p-2*n] then
         directions.Add(-2*n);
      if (x < size-1) and not cells[p+2] then
         directions.Add(2);
      if (y < size-1) and not cells[p+2*n] then
         directions.Add(2*n);
      for var i := directions.High downto 1 do
         directions.Swap(i, RandomInt(i+1));
      for var dir in directions do begin
         m.Start := p;
         m.Direction := dir;
         stack.Push(m);
      end;

      while stack.Count > 0 do begin
         m := stack.Pop;
         p := m.Start;
         if not cells[p + m.Direction] then begin
            cells[p + m.Direction div 2] := True;
            p += m.Direction;
            break;
         end;
      end;
   end;

   for var i := 0 to cells.High step n do begin
      for var j := 0 to n-1 do
         if cells[i+j] then
            Print('  ')
         else Print(#$2588#$2588);
      PrintLn('');
   end;
end;

MakeMaze(12);
