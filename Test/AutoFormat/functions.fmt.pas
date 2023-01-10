function Hello : String;
begin
	Result := 'World';
end;

procedure Test(a, b : Integer);
begin
	for var i := 1 to 10 do
		PrintLn(Hello)
end;

// commented
procedure CommentMe; //tail
begin /* blabla */
	PrintLn('foo'); // bar
end; (* we're done here *)