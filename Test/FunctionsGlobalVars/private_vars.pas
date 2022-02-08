uses unit_private_vars1, unit_private_vars2;

try
	ReadPrivateVar('test');
except
	on E: Exception do
		PrintLn(E.Message);
end;

unit_private_vars1.PrepareTest;
unit_private_vars2.PrepareTest;

unit_private_vars1.RunTestA;
unit_private_vars2.RunTestA;

unit_private_vars1.RunTestB;
unit_private_vars2.RunTestB;
