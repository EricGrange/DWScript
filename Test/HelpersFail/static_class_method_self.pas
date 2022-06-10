type
	THelper = Helper for TObject
		class function Hello(who : String) : String; static;
	end;
	
class function THelper.Hello(who : String) : String;
begin
	Result := Self.ClassName + ' hello ' + who;
end;

var o : TObject;
PrintLn(o.Hello('world'));