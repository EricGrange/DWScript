unit OverloadB;

interface

uses overloadA;

procedure Test(a, b : Integer); overload;

procedure ExecB;

implementation

procedure Test(a, b : Integer);
begin
   PrintLn('a,b: '+IntToStr(a)+','+IntToStr(b));
end;

procedure Test(s : String); overload;
begin
   PrintLn('Hello '+s+' From B');
end;

procedure ExecB;
begin
   Test('ExecB');
end;

end.
