var js1 := JSON.NewObject;
try
	js1.a := js1;
except
	on E: Exception do PrintLn('1: ' + E.Message);
end;

var js2 := JSON.NewArray;
try
	js2.Add(js2);
except
	on E: Exception do PrintLn('2: ' + E.Message);
end;

js1.a := js2;

try
	js2.Add(js1);
except
	on E: Exception do PrintLn('3: ' + E.Message);
end;

var js3 := JSON.NewObject;
js3.a := js1;

try
	js1.b := js3;
except
	on E: Exception do PrintLn('4: ' + E.Message);
end;
