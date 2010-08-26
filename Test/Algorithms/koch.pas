// Koch

const f = 1.793;

procedure koch(x1,y1,x2,y2:float; n:integer);
begin
var xa,ya,xb,yb,xc,yc:float;
if n = 0 then
  println('line('+inttostr(round(x1))+','+inttostr(round(y1))+','+inttostr(round(x2))+','+inttostr(round(y2))+')')
 else
  begin
   xa := (2*x1+x2)/3;
   ya := (2*y1+y2)/3;
   xc := (x1+2*x2)/3;
   yc := (y1+2*y2)/3;
   xb := (x1+x2+(y1-y2)/f)/2;
   yb := (y1+y2+(x2-x1)/f)/2;
   koch(x1,y1,xa,ya,n-1);
   koch(xa,ya,xb,yb,n-1);
   koch(xb,yb,xc,yc,n-1);
   koch(xc,yc,x2,y2,n-1);
  end;
end;

var n:integer;
var max:integer;

max := 100;
n := 3;
koch(0,max/4,f*max/4,max,n);
koch(f*max/4,max,f*max/2,max/4,n);
koch(f*max/2,max/4,0,max/4,n);

