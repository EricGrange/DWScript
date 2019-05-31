PrintLn(StrReplaceMacros('hello', ['ell', '-'], '<', '>'));
PrintLn(StrReplaceMacros('hello', ['ell', '-'], 'h', 'o'));

PrintLn(StrReplaceMacros('<ab>cd<ef>gh<ab>', ['ab', '123', 'cd', '456'], '<', '>'));
PrintLn(StrReplaceMacros('<ab<cd<ef<gh<cd<', ['ab', '123', 'cd', '456789'], '<', '<'));

PrintLn(StrReplaceMacros('a<b<cde<b', ['b', '-'], '<'));

PrintLn(StrReplaceMacros('a<>b<c>de<fg>hij', ['', '+', 'c', '**', 'fg', '/'], '<', '>'));
PrintLn(StrReplaceMacros('a</>b<c/>de<fg/>hij', ['', '+', 'c', '**', 'fg', '/'], '<', '/>'));
PrintLn(StrReplaceMacros('a<\/>b<\c/>de<\fg/>hij', ['', '+', 'c', '**', 'fg', '/'], '<\', '/>'));

PrintLn(StrReplaceMacros('abcdef', ['', '+'], 'abc', 'def'));
PrintLn(StrReplaceMacros('abcdef', ['', '+'], 'defg', 'ef'));
PrintLn(StrReplaceMacros('abcdef', ['', '+'], 'abc', 'fg'));
PrintLn(StrReplaceMacros('abcdefg', ['cd', '+'], 'b', 'e'));
PrintLn(StrReplaceMacros('abcdefg', ['cd', '+'], 'b', ''));

PrintLn(StrReplaceMacros('a<bc<d', ['', '+'], '<'));