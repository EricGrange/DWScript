type TestParent=class
end;
type TestEnfant=class(TestParent)
end;

procedure Test(tab : array of TestParent);
begin
end;

var a: array of TestEnfant;
Test(a);

