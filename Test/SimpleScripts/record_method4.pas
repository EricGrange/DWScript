type 
   TRec = record
      x : Integer;
      procedure test; begin println(x); end;
   end;
   
var r : TRec;

r.test;
r.x:=1;
r.test;