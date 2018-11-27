var a : array [String] of array of String;

a['alpha'].Add('beta');

PrintLn('> ' +a.Keys.Join(','));
PrintLn(a['alpha'].Join(','));

a['alpha'].Add('gamma');
a['delta'].Add('beta');

PrintLn('> ' +a.Keys.Join(','));
PrintLn(a['alpha'].Join(','));
PrintLn(a['delta'].Join(','));
