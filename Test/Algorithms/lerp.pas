type
   TPointF = record
      X, Y : Float;
      procedure Print; 
      begin 
         PrintLn(Format('%.2f / %.2f', [X, Y]));
      end;
   end;

function LinearInterpolation(A, B, P: Float): Float; overload;
begin
  Result := A + P * (B - A);
end;

function LinearInterpolation(A, B: TPointF; P: Float): TPointF; overload;
begin
  Result.X := A.X + P * (B.X - A.X);
  Result.Y := A.Y + P * (B.Y - A.Y);
end;

function LinearInterpolationC(const A, B: TPointF; P: Float): TPointF; overload;
begin
  Result.X := A.X + P * (B.X - A.X);
  Result.Y := A.Y + P * (B.Y - A.Y);
end;

procedure LinearInterpolationCV(const A, B: TPointF; P: Float; var Result : TPointF); overload;
begin
   Result:=LinearInterpolationC(A, B, P);
end;


PrintLn(LinearInterpolation(1, 3, 0));
PrintLn(LinearInterpolation(1, 3, 0.5));
PrintLn(LinearInterpolation(1, 3, 1));

var p1, p2 : TPointF;

p1.X:=1;
p1.Y:=2;

p2.X:=3;
p2.Y:=4;

LinearInterpolation(p1, p2, 0).Print;
LinearInterpolation(p1, p2, 0.5).Print;
LinearInterpolation(p1, p2, 1).Print;

LinearInterpolationC(p1, p2, 0).Print;
LinearInterpolationC(p1, p2, 0.5).Print;
LinearInterpolationC(p1, p2, 1).Print;

var r : TPointF;

LinearInterpolationCV(p1, p2, 0, r); r.Print;
LinearInterpolationCV(p1, p2, 0.5, r); r.Print;
LinearInterpolationCV(p1, p2, 1, r); r.Print;
