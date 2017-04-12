type
   TRec1 = record
     num: Integer;
   end;

var a1: array of TRec1 = ((num: 1), (num: 3), (num: 7));

PrintLn(a1[0].num + a1[1].num + a1[2].num);

for var rec in a1 do
    rec.num := rec.num + 1; 
    
PrintLn(a1[0].num + a1[1].num + a1[2].num);

type
   TRec2 = record
     num1, num2: Integer;
   end;

var a2: array of TRec2 = ((num1: 1; num2: 10), (num1: 3; num2: 30), (num1: 7; num2: 70));

PrintLn(a2[0].num1 + a2[1].num1 + a2[2].num1);
PrintLn(a2[0].num2 + a2[1].num2 + a2[2].num2);

for var rec in a2 do begin
    rec.num1 := rec.num1 + 1;
    rec.num2 += rec.num2; 
end;
    
PrintLn(a2[0].num1 + a2[1].num1 + a2[2].num1);
PrintLn(a2[0].num2 + a2[1].num2 + a2[2].num2);

