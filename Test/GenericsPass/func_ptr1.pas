type TProc<T> = procedure (p : T);

type TA = array [0..1] of Integer;

procedure TestS(s : String);
begin
    PrintLn(s);
end;

procedure TestA(a : TA);
begin
    PrintLn(a[0]+a[1]);
end;

var ps : TProc<String>;
var pa : TProc<TA>;

ps := @TestS;
pa := @TestA;

ps('hello');
pa([1200,34]);
