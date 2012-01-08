type 
   TRec = record
      Field : Integer;
   end;
   
type
   TObj = class

      procedure Pop(var a : TRec);
      begin
         a.Field:=2;
      end;

      procedure SetProp(v : TRec);
      begin
         PrintLn(v.Field);
      end;
      property Prop : TRec write SetProp;
      
      procedure Test;
      begin
         var v : TRec;
         Pop(v);
         Prop:=v;
      end;
   end;


var obj := new TObj;
obj.Test;

