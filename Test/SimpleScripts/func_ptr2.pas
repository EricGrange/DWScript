type
   TMyProcStr = procedure (s : String);
type
   TMyProcInts = procedure (i1, i2 : Integer);

  
procedure TestS1(s : String);
begin
   PrintLn('Hello '+s)
end;

procedure TestS2(str : String);
begin
   PrintLn('Bye bye '+str)
end;

procedure TestI1(i1, i2 : Integer);
begin
   PrintLn(i1+i2);
end;

procedure TestI2(i1, i2 : Integer);
begin
   PrintLn(i1-i2);
end;

var ps : TMyProcStr;
var pis : TMyProcInts;

procedure RunPtrs;
begin
   ps('Run');
   pis(20, 3);
end;

ps:=TestS1;
pis:=TestI1;
ps('World');
pis(10, 5);

RunPtrs;

ps:=TestS2;
pis:=TestI2;

ps('World');
pis(10, 5);

RunPtrs;
