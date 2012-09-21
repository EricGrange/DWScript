type 
   TRec = record
      Field : TRec;
   end;
   
var r : TRec;
r.Field:=r;