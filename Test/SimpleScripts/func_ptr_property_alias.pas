type TEvent  = procedure (s : String);
type TMyEvent = TEvent;

var e : TEvent;
var m : TMyEvent;

procedure Test(s : String);
begin
	PrintLn(s);
end;

e:=Test;
e('alpha');
m:=e;
m('beta');
e:=m;
e('gamma');

