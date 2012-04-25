type
 TTest = class
   procedure A(s1: string = ''); overload;
   procedure A(s1, s2: string; s3: string = ''); overload;
   procedure A(s1: string; s2: integer; s3: string = ''); overload;
 end;

procedure TTest.A(s1: string);
begin
   PrintLn(1);
end;

procedure TTest.A(s1, s2: string; s3: string);
begin
   PrintLn(2);
end;

procedure TTest.A(s1: string; s2: integer; s3: string = '');
begin
   PrintLn(3);
end;

var t:=new Ttest;
t.A;
t.A('', '');
t.A('', 0);