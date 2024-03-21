var k := $100000000;

if k in [ $200000000 .. $300000000 ] then PrintLn('bug a');
if k not in [ $200000000 .. $300000000 ] then PrintLn('ok a');

k := 2*k;

if k in [ $200000000 .. $300000000 ] then PrintLn('ok b');
if k not in [ $200000000 .. $300000000 ] then PrintLn('bug b');

k := k - 1;

if k in [ $200000000 .. $300000000 ] then PrintLn('bug c');
if k not in [ $200000000 .. $300000000 ] then PrintLn('ok c');

k := $300000000;

if k in [ $200000000 .. $300000000 ] then PrintLn('ok d');
if k not in [ $200000000 .. $300000000 ] then PrintLn('bug d');

k := k + 1;

if k in [ $200000000 .. $300000000 ] then PrintLn('bug e');
if k not in [ $200000000 .. $300000000 ] then PrintLn('ok e');
