var a : JSONVariant;

try
	a.Add(1);
except
	on E: Exception do PrintLn(E.Message);
end;

a := JSON.NewArray;

try
	a.Add(Unassigned);
except
	on E: Exception do PrintLn(E.Message);
end;
