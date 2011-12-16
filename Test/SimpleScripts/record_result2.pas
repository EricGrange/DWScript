type
   Tmt=record
      c1 : String;
      c2 : string;
   end;
   
   
function titi : Tmt;
begin
   result.c1:='aaa';
   result.c2:='bbb';
end;

procedure toto(a : Tmt);
begin
   PrintLn(a.c1+#13#10+a.c2);
end;


toto(titi);     // marche pas

var x := titi;  // marche
toto(x);