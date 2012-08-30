type TNotifyEvent = procedure (Sender : TClass);

var p : TNotifyEvent;

p := lambda (s) PrintLn(s.ClassName) end;
p(TObject);

p := lambda PrintLn('hello world') end;
p(TObject);
p(nil);