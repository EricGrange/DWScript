var notANumber = 0/0;

PrintLn(JSON.Stringify(notANumber));
PrintLn(JSON.Stringify([-0.0, notANumber, 1e99]));