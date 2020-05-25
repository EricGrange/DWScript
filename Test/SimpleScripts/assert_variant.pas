var v : Variant;

try
   Assert(v, 'empty');
except
   on E: EAssertionFailed do
      PrintLn(E.Message);
end;

v := True;

Assert(v, 'bug True');

v := 1;

Assert(v, 'bug 1');

v := 0;
try
   Assert(v, 'ok');
except
   on E: EAssertionFailed do
      PrintLn(E.Message);
end;
