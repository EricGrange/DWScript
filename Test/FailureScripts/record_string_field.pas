type
   TMyRec = record
      s : String;
   end;
 
function Test : TMyRec;
begin
end;

Test.s[2] := 'b';

const r : TMyRec = (s:'hello');

r.s[2] := 'b';