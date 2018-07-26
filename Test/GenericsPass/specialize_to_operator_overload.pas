//
// New Script
//

type
   TTest<A> = class static
      class function Clamp(v, mini, maxi : A) : A;
      begin
         if v < mini then
            Result := mini
         else if v > maxi then
            Result := maxi
         else Result := v;
      end;
      
      class function Clamp2(v, mini, maxi : A) : A;
      begin
         Result := 
            if v < mini then
                mini
            else if v > maxi then
                maxi
            else v;
      end;
   end;

PrintLn(TTest<Integer>.Clamp(3, 1, 4));
PrintLn(TTest<Integer>.Clamp(3, 1, 2));
PrintLn(TTest<Integer>.Clamp(3, 5, 6));

PrintLn(TTest<Integer>.Clamp2(3, 1, 4));
PrintLn(TTest<Integer>.Clamp2(3, 1, 2));
PrintLn(TTest<Integer>.Clamp2(3, 5, 6));

type
   TRec = record a, b : Integer; end;

function RecLess(v1, v2 : TRec) : Boolean;
begin
   Result := (v1.a < v2.a) and (v1.b < v2.b);
end;

function RecGreater(v1, v2 : TRec) : Boolean;
begin
   Result := (v1.a > v2.a) and (v1.b > v2.b);
end;

function RecToVariant(r : TRec) : Variant;
begin
    Result := r.a.ToString + ',' + r.b.ToString;
end;

operator < (TRec, TRec) : Boolean uses RecLess;
operator > (TRec, TRec) : Boolean uses RecGreater;
operator implicit (TRec) : Variant uses RecToVariant;

var r1 : TRec = (a:1; b:10);
var r2 : TRec = (a:2; b:20);
var r3 : TRec = (a:3; b:30);

PrintLn(TTest<TRec>.Clamp(r1, r2, r3));
PrintLn(TTest<TRec>.Clamp(r2, r1, r3));
PrintLn(TTest<TRec>.Clamp(r3, r1, r2));

r2.b := 22;

PrintLn(TTest<TRec>.Clamp2(r1, r2, r3));
PrintLn(TTest<TRec>.Clamp2(r2, r1, r3));
PrintLn(TTest<TRec>.Clamp2(r3, r1, r2));


