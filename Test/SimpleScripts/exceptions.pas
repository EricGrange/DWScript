{
Demo: Exception handling.
}

type
  MyException = class(Exception)
  end;

type
  OtherException = class(Exception)
  end;

try
  raise MyException.Create('exception message');
except
  on e: MyException do
    PrintLn('MyException: ' + e.Message);
  on e: OtherException do
    PrintLn('OtherException: ' + e.Message);
else
  PrintLn('Else');
end;

try
  raise OtherException.Create('exception message');
except
  on e: MyException do
    PrintLn('MyException: ' + e.Message);
  on e: OtherException do
    PrintLn('OtherException: ' + e.Message);
else
  PrintLn('Else');
end;

try
  raise Exception.Create('exception message 1');
except
  on e: MyException do
    PrintLn('MyException: ' + e.Message);
  on e: OtherException do
    PrintLn('OtherException: ' + e.Message);
else
  PrintLn('Else');
end;

try
  raise OtherException.Create('exception message 2');
except
  on e: Exception do
    PrintLn('MyException: ' + e.Message);
else
  PrintLn('Invisible');
end;

try
  try
    raise Exception.Create('exception message 3');
    PrintLn('Invisible');
  finally
    PrintLn('Finally');
  end;
  PrintLn('Invisible');
except
  PrintLn('Except');
end;