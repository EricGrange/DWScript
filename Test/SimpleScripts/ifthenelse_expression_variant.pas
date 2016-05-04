var v : Variant;

v := True;

PrintLn(if v then 'ok' else 'bug');

v := not v;

PrintLn(if v then 'bug' else 'ok');

PrintLn(if not v then 'ok' else 'bug');