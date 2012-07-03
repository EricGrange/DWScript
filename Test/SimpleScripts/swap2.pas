type 
   TRec = record
      a, b : String;
   end;

var r : TRec;   
r.a:='hello';
r.b:='world';

PrintLn(r.a);
PrintLn(r.b);
Swap(r.a, r.b);
PrintLn(r.a);
PrintLn(r.b);

var ar : array [0..1] of TRec;

ar[0].a := 'a0';
ar[0].b := 'b0';
ar[1].a := 'a1';
ar[1].b := 'b1';

Swap(ar[0].a, ar[1].b);
Swap(ar[1].a, ar[0].b);

PrintLn(ar[0].a+ar[0].b);
PrintLn(ar[1].a+ar[1].b);

