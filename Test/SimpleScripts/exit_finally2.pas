function Hello(doExit : Boolean) : String;
begin
   Result:='duh';
   try
      try
         Result:='Hello';
         if doExit then Exit;
	 Result+=' one';
      finally
         PrintLn('1st finally');
      end;
      Result+=' two';
   finally
      PrintLn('2nd finally');
      Result+=' world'
   end;
   Result:='Bye bye';
end;

PrintLn(Hello(False));
PrintLn(Hello(True));
