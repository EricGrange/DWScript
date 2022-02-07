// from Wikipedia:
// For French, the last accent in a given word determines the order.
// For example, in French, the following four words would be sorted this way: cote < côte < coté < côté.

PrintLn('fr');
PrintLn(CompareLocaleStr('cote', 'côte', 'fr', False));
PrintLn(CompareLocaleStr('côte', 'coté', 'fr', False));
PrintLn(CompareLocaleStr('coté', 'côté', 'fr', False));

PrintLn('en');
PrintLn(CompareLocaleStr('cote', 'côte', 'en', False));
PrintLn(CompareLocaleStr('côte', 'coté', 'en', False));
PrintLn(CompareLocaleStr('coté', 'côté', 'en', False));

PrintLn('case');
PrintLn(CompareLocaleStr('Hello', 'hello', 'en', True));
PrintLn(CompareLocaleStr('Hello', 'hello', 'en', False));