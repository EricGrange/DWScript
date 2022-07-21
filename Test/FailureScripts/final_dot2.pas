procedure Main;
begin

	procedure test;
	begin
	  try
		 PrintLn('a1');
		 PrintLn('a2');
	  except
		 PrintLn('a3');
	//  end;
	end;

	begin
	  test();
	end.

end;