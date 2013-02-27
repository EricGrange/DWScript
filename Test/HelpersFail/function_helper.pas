procedure Hello; helper SayHello;
begin
end;

function Test(s : String) : String; helper;
begin
   Result:=s+' world';
end;

function Test2(s, t : String) : String; helper;
begin
   Result:=s+t;
end;

var s := ('hello').Test;
('hello').Test('456');
('hello').Test(456);

('hello').Test2;
('hello').Test2('456');
('hello').Test2(456);
