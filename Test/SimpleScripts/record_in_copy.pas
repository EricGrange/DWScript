type 
   TSubRec = record
      i : Integer;
      a : array [1..2] of Boolean;
   end;
   
type
   TRec = record
      i : Integer;
      s : String;
      d : array of Integer;
      r : TSubRec;
   end;
   
var sr, sr2 : TSubRec;

sr.i:=123;
sr.a[1]:=False;
sr.a[2]:=True;

sr2:=sr;

sr.i:=456;
sr.a[1]:=True;
sr.a[2]:=False;

PrintLn(sr2.i);
PrintLn(sr2.a[1]);
PrintLn(sr2.a[2]);

var r, r2 : TRec;

r.i:=789;
r.s:='Hello';
r.d:=new Integer[1];
r.d[0]:=123456;
r.r:=sr2;

r2:=r;

r.i:=111;
r.s:='World';
r.d[0]:=321;
r.r:=sr;
sr2.i:=0;

PrintLn(r2.i);
PrintLn(r2.s);
PrintLn(r2.d[0]);
PrintLn(r2.r.i);
PrintLn(r2.r.a[1]);
PrintLn(r2.r.a[2]);



   
