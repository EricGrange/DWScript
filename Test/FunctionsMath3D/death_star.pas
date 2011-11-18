const cShades = '.:!*oe&#%@';

var light := VectorNormalize(Vector(-50.0, 30, 50, 0));
 
type 
   TSphere = record
      c : TVector;
      r : Float;
   end;
   
const big : TSphere = (c: Vector(20, 20, 0, 1); r: 20);
const small : TSphere = (c: Vector(7, 7, -10, 1); r: 15);
 
function HitSphere(sph : TSphere; x, y : Float; var z1, z2 : Float) : Boolean;
begin
   x -= sph.c.x;
   y -= sph.c.y;
   var zsq = sph.r * sph.r - (x * x + y * y);
   if (zsq < 0) then Exit False;
   zsq := Sqrt(zsq);
   z1 := sph.c.z - zsq;
   z2 := sph.c.z + zsq;
   Result:=True;
end;
 
procedure DrawSphere(k, ambient : Float);
var
   i, j, intensity : Integer;
   b : Float;
   eye : TVector;
   zb1, zb2, zs1, zs2, dot : Float;
   vec : TVector;
begin
   for i:=Trunc(big.c.y-big.r) to Trunc(big.c.y+big.r)+1 do begin
      eye.y := i + 0.5;
      for j := Trunc(big.c.x-2*big.r) to Trunc(big.c.x+2*big.r) do begin
         eye.x := (j-big.c.x)/2 + 0.5 + big.c.x;
 
         if not HitSphere(big, eye.x, eye.y, zb1, zb2) then begin
            Print(' ');
            continue;
         end;
         eye.Z := zb1;
         if not HitSphere(small, eye.x, eye.y, zs1, zs2) then
            vec := eye - big.c
         else begin
            if zs1 < zb1 then begin
               if zs2 > zb2 then begin
                  Print(' ');
                  continue;
               end;
               if zs2 > zb1 then begin
                  eye.Z := zs2;
                  vec := small.c - eye;
               end else vec := eye - big.c;
            end else vec := eye - big.c;
         end;
 
         dot := Clamp( -(light * VectorNormalize(vec)), 0, 1);
         b := Power(dot, k) + ambient;
         intensity := Round((1 - b) * Length(cShades));
         Print(cShades[ClampInt(intensity+1, 1, Length(cShades))]);
      end;
      PrintLn('');
   end;
end;
 
DrawSphere(2, 0.3);
