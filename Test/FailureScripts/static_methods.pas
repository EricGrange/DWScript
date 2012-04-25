type
   TDummy = class 
      Field : Integer;
      procedure SetField(v : Integer);
      property Prop : Integer read Field write SetField;
      
      class procedure Test; static;
      constructor Bug; static;
      destructor Bug2; static;
      procedure ReBug; static;
   end;
   
type
   TRec = record
      Field : Integer;
      class procedure Test; static;
      procedure Bug; static;
   end;   
   
class procedure TDummy.Test;
begin
   Field:=1;
   Prop:=2;
end;   
   
 {$FATAL 'aborted'}