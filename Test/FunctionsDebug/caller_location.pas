var loc := CallerSourceCodeLocation;

function Test : TSourceCodeLocation;
begin
	Result := CallerSourceCodeLocation;
end;

function SubTest : TSourceCodeLocation;
begin
	Result := Test;
end;

PrintLn(loc.File);
PrintLn(loc.Line);
PrintLn(loc.Name);

loc := Test;

PrintLn(loc.File);
PrintLn(loc.Line);
PrintLn(loc.Name);

loc := SubTest;

PrintLn(loc.File);
PrintLn(loc.Line);
PrintLn(loc.Name);


