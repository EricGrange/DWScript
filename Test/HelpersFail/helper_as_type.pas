type
   TDummy = helper for Boolean 
   end;
   
var b : TDummy;

type
   TBug = helper for TDummy
   end;
   
procedure Bug(a : TBug);
begin
end;
