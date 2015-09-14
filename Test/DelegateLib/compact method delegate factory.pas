type
	TMyStuff = class
		public
			procedure DoIt(const Msg: string);
			begin
				PrintLn(Msg);
			end;
	end;

var vStuff : TMyStuff = nil;

function Stuff : TMyStuff;
begin
	if vStuff = nil then
		vStuff := TMyStuff.Create;
	Result := vStuff;
end;

procedure DoOnTest(Sender: TObject);
begin
	PrintLn(Sender.ClassName);
	Stuff.DoIt(Sender.ClassName);
end;

var Foo := TFoo.Create;
Foo.OnTest := DoOnTest;
Foo.Test;
