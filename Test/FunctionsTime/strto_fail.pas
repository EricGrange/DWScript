var t : Float;

try
	t := StrToDateTime('bug1');
    {$ifdef JS_CODEGEN}
    if t = 0 then raise Exception.Create('ok 1');
    {$endif}
except
	on E: Exception do
		PrintLn(E.Message);
end;

try
	t := StrToDate('bug2');
    {$ifdef JS_CODEGEN}
    if t = 0 then raise Exception.Create('ok 2');
    {$endif}
except
	on E: Exception do
		PrintLn(E.Message);
end;

try
	t := StrToTime('bug3');
    {$ifdef JS_CODEGEN}
    if t = 0 then raise Exception.Create('ok 3');
    {$endif}
except
	on E: Exception do
		PrintLn(E.Message);
end;