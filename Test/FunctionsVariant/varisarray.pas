{$ifdef JS_CODEGEN}
uses JS_JSON;
{$endif}

procedure Test(v : Variant);
begin
   Print(v);
   PrintLn(' is ' + if VarIsArray(v) then 'array' else 'not array');
end;

var v : Variant;
Test(v);
v := 'hello';
Test(v);

type TTest = class (IInterface) end;
var i : IInterface;
Test(i);
i := new TTest;
Test(i);

Test(JSON.Parse('123'));
Test(JSON.Parse('[123]'));
Test(JSON.Parse('[[]]')[0]);
Test(JSON.Parse('[123]')[1]);
Test(JSON.Parse('null'));
Test(JSON.Parse('{"a":false}').a);
Test(JSON.Parse('{"a":false}').b);

