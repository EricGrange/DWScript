unit conditionalsUnit;

procedure Test;
begin
	{$ifndef DWSCRIPT}
	PrintLn('DWSCRIPT missing');
	{$endif}

	{$ifdef CONDITION}
	PrintLn('unit ok');
	{$else}
	PrintLn('unit not ok');
	{$endif}

	{$undef CONDITION}

	{$ifndef CONDITION}
	PrintLn('unit undef ok');
	{$else}
	PrintLn('unit undef not ok');
	{$endif}
	
	{$define CONDITION}
end;