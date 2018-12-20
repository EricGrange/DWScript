function Test : Integer;
begin
  exit();
end;

function Test2 : Integer;
begin
   exit PrintLn('');
end;

type
  JExternalClass = class external
  end;

function Test3 : JExternalClass
begin
  exit();
end;