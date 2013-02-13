type 
   TBase = class
      procedure Test(a : Integer); virtual;
      begin
         PrintLn('base '+IntToStr(a));
      end;
      property Prop : Integer write Test;
   end;
   
type TSub = class abstract (TBase);

type TSub2 = class (TSub);
type TSub3 = class (TSub2);
type TSub4 = class (TSub3);
type TSub5 = class (TSub4)
      procedure Test(a : Integer); override;
      begin
         PrintLn('here '+IntToStr(a));
      end;
   end;

TSub3.Create.Prop:=1;
TSub4.Create.Prop:=2;
TSub5.Create.Prop:=3;
   
   