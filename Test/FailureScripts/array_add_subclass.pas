type TestParent=class
end;
type TestEnfant=class(TestParent)
end;

procedure Test(tab : array of TestEnfant);
begin
end;

var a: array of TestParent;
Test(a);

