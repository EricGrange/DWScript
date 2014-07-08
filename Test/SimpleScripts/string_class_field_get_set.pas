type
   TTest = class
      F : String := 'hello';
   end;

var o := new TTest;

function Get : TTest;
begin
   Result := o;
end;

PrintLn(o.F);
PrintLn(o.F[3]);
o.F[3]:='z';
PrintLn(o.F);
PrintLn(o.F[3]);
Get.F[3]:='y';
PrintLn(Get.F);
PrintLn(Get.F[3]);
