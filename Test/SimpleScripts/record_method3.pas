type 
   TRec1 = record
      F1, F2 : Integer;
      function AsString : String;
      begin
         Result:=IntToStr(F1)+IntToStr(F2);
      end;
      class procedure Here;
      begin
         PrintLn('Rec1');
      end;
   end;

type 
   TRec2 = record
      Field : String;
      function AsString : String;
      begin
         Result:=Field;
      end;
      class procedure Here;
      begin
         PrintLn('Rec2');
      end;
   end;
   
var r1 : TRec1 := (F1:12; F2:34);
var r2 : TRec2 := (Field:'Hello');

r1.Here;
PrintLn(r1.AsString);
r2.Here;
PrintLn(r2.AsString);