unit OverloadC;

interface

uses overloadA;

procedure Test(c : Integer); overload;

procedure ExecC;

implementation

procedure Test(c : Integer);
begin
   PrintLn('c: '+IntToStr(c));
end;

procedure Test(s : String); overload;
begin
   PrintLn('Hello '+s+' From C');
end;

procedure ExecC;
begin
   Test('ExecC');
end;

end.
