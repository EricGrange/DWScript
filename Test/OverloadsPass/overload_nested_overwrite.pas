type
  TTest = class
    procedure Overloaded(a: integer); overload;
    procedure Overloaded(s: string); overload;
    procedure Test1; 
    procedure Test2; 
  end;

procedure TTest.Overloaded(a: integer);
begin
   PrintLn('Integer: '+IntToStr(a));
end;

procedure TTest.Overloaded(s: string);
begin
   PrintLn('String: '+s);
end;

procedure TTest.Test1;
begin

  procedure Overloaded(i : Integer);
  begin
     PrintLn('Local: '+IntToStr(i));
  end;

   Overloaded(123);
end;

procedure TTest.Test2;
begin
   Overloaded(456);
   Overloaded('abc');
end;

var t := new TTest;
t.Test1;
t.Test2;