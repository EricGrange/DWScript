PrintLn(FormatFloat('0.00', 1.2345));
PrintLn(FormatFloat('0.00', 1.2355));
PrintLn(FormatFloat('#.##', 1.2345));
PrintLn(FormatFloat('000.00', 1.2));
PrintLn(FormatFloat('#,##0.00', 1234567.89));
PrintLn(FormatFloat('0.00%', 0.123));
PrintLn(FormatFloat('0.00%', 1.23));
PrintLn(FormatFloat('0.00%;-0.00%', 0.123));
PrintLn(FormatFloat('0.00%;-0.00%', -0.123));
PrintLn(FormatFloat('Positive;Negative;Zero', 10));
PrintLn(FormatFloat('Positive;Negative;Zero', -10));
PrintLn(FormatFloat('Positive;Negative;Zero', 0));
PrintLn(FormatFloat('0.###E+00', 12345.678));
PrintLn(FormatFloat('0.###E-00', 12345.678));
try
   PrintLn(FormatFloat('0.00', 0.0/0.0));
except
   on E: Exception do PrintLn(E.Message);
end;
try
   PrintLn(FormatFloat('0.00', 1.0/0.0));
except
   on E: Exception do PrintLn(E.Message);
end;
