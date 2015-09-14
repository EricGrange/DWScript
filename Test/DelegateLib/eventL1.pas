type
	TTest = class
		procedure Test1(Sender : TObject);
		begin
			Print(ClassName);
			PrintLn(Sender<>nil);
		end;
		procedure Test2(Sender : TObject);
		begin
			Print(ClassName);
			PrintLn(Sender.ClassName);
		end;
		procedure Test31(Sender : TObject);
		begin
			Test1(Sender);
		end;
		procedure Test32(Sender : TObject);
		begin
			Test2(Sender);
		end;
	end;
type
	TSubTest = class(TTest) end;

procedure DoIt;
begin
	var t := TTest.Create;
	var s := TSubTest.Create;

	CallEvent(t.Test1, t);
	CallEvent(t.Test2, t);
	CallEvent(t.Test31, t);
	CallEvent(t.Test32, t);

	CallEvent(t.Test1, s);
	CallEvent(t.Test2, s);
	CallEvent(t.Test31, s);
	CallEvent(t.Test32, s);
end;

DoIt;
