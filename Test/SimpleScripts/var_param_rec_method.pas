type
   TRec = record
      Field : Integer;
      
      procedure Double;
      begin
         Field*=2;
      end;
   
      procedure Pop(v : Integer; var a : TRec);
      begin
         a.Field:=2+v;
      end;

      procedure SetProp(v : TRec);
      begin
         PrintLn(v.Field);
      end;
      property Prop : TRec write SetProp;
      
      procedure Test;
      begin
         var v : TRec;
         Pop(10, v);
         Prop:=v;
         v.Double;
         Prop:=v;
      end;
   end;

var obj : TRec;
obj.Test;

