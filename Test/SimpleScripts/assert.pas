var v = True;

Assert(1=1, 'should optimize itself away');

Assert(v);
try
   Assert(not v);
except
   on E: EAssertionFailed do
      PrintLn(E.Message);
end;

Assert(v, 'bug');
try
   Assert(not v, 'boom');
except
   on E: EAssertionFailed do
      PrintLn(E.Message);
end;

Assert(True);
try
   Assert(False);
except
   on E: EAssertionFailed do
      PrintLn(E.Message);
end;

Assert(True, 'rebug');
try
   Assert(False, 'reboom');
except
   on E: EAssertionFailed do
      PrintLn(E.Message);
end;
