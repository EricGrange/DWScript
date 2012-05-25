type
   TRec = record
      a : Integer;
      b : String;
   end;

type
   TSub = record
      a : array [0..1] of TRec;
      procedure P;
      begin
         Print('0: '); Print(a[0].a); PrintLn(a[0].b);
         Print('1: '); Print(a[1].a); PrintLn(a[1].b);
      end;
   end;
   
var r1, r2 : TRec;

r1.a:=1;
r1.b:='one';
r2.a:=2;
r2.b:='two';

var s1, s2 : TSub;

procedure CopySubRec(var s : TSub; i : Integer; const r : TRec);
begin
   s.a[i]:=r;
end;

procedure CopySub(const src : TSub; var dest : TSub);
begin
   dest:=src;
end;

CopySubRec(s1, 0, r1);
CopySubRec(s1, 1, r2);

s1.P;
r1.a:=10;
r2.b:='bug';

s1.P;

CopySub(s1, s2);

s1.a[0].a:=123;
s1.a[1].b:='modded';

s1.P;
s2.P;

s1:=s2;
s2.a[0].a:=456;
s2.a[1].b:='rebug';

s1.P;
