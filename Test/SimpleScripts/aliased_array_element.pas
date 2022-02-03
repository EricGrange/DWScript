type TTest = class
  Value : Integer;
  function Meth : Integer; begin Result := -Value; end;
end;
type ATest = TTest;

type TAHelper = helper for ATest
		function Meth : Integer; begin Result := 10 * Self.Value; end;
	end;


var o := ATest.Create();
var a : array of ATest;
a.Add(o);
a[0].Value := 123;
PrintLn(a[0].Value.ToString);

PrintLn(a[0].Meth.ToString);
PrintLn(TTest(a[0]).Meth.ToString);