type
   TEnum = enum ( zero, one, two );
type
   TSet = set of TEnum;

var vS : TSet;
var vS2 : TSet;

procedure Test;
begin
   if TEnum.two in vS2 then
      PrintLn('ok')
   else PrintLn('bug');
   PrintLn(TEnum.two in vS2);
end;

vS2.Include(TEnum.two);
Test;

if TEnum.two in vS2 then
   PrintLn('ok')
else PrintLn('bug');
