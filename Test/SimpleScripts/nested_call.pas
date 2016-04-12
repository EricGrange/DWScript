function Test(a, b : Integer) : Integer;
begin
	Result := a + b;
end;

PrintLn( 
	Test( 
		Test ( 
			Test ( 1, 2 ),
			Test ( 3, 4 )
		),
		Test ( 
			Test ( 5, 6 ),
			Test ( 7, 8 )
		)
	)
);