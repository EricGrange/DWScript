var locator : ComVariant;

try
   locator := CreateOleObject('WbemScripting.foobar');
except
   on E: Exception do
      PrintLn(E.Message);
end;
locator := CreateOleObject('WbemScripting.SWbemLocator');

try
   PrintLn(locator);
except
   on E: Exception do
      PrintLn(E.Message);
end;

try
   locator.ConnectServer('localhost', 'rootbug\CIMV2', '', '');
except
   on E: Exception do
      PrintLn(E.Message);
end;

try
   locator.crash_me_once := 'me';
except
   on E: Exception do
      PrintLn(E.Message);
end;

try
   PrintLn(locator.crash_me_again());
except
   on E: Exception do
      PrintLn(E.Message);
end;

