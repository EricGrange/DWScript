var pf : function : Integer;

pf := function : Integer
 begin
   Result:=1;
 end;
 
pf := lambda => 1;

var pt := lambda => 2;

var pp : function (i : Integer) : Integer;

pp := lambda (i) => i+i;
pp(1);
pp := lambda (a) => a+..a;
pp(2);

