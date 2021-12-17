procedure MyProc(a : Integer);
begin
end;

IntToBin(45, 12, 0);
MyProc(45, 12);

var v := '12';
MyProc(45, StrToInt(v));
MyProc(45, StrToInt(v, 1, 2));
