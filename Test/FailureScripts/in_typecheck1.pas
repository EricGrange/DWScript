if 1 in [2.5..3.5, 4.5] then PrintLn('doh');
if 1.5 in [2.5..3.5, 4.5] then PrintLn('doh');
if 'doh' in [2.5..3.5, 4.5] then PrintLn('doh');

if 1 in [2..3, 4] then PrintLn('doh');
if 1.5 in [2..3, 4] then PrintLn('doh');
if 'doh' in [2..3, 4] then PrintLn('doh');

if 1 in ['a'..'b', 'doh'] then PrintLn('doh');
if 'doh' in ['a'..'b', 'doh'] then PrintLn('doh');

if 'doh' in ['a'..'b', 3] then PrintLn('doh');
if 3.5 in ['a'..'b', 3] then PrintLn('doh');

if 'doh' in [1..2, 'doh'] then PrintLn('doh');
if 1 in [1..2, 'doh'] then PrintLn('doh');
