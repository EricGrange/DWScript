{$HINT 'H1'}
{$WARNING 'W1'}

{$PUSH}
{$HINTS OFF}
{$WARNINGS OFF}
{$HINT 'HiddenHint'}
{$WARNING 'HiddenWarning'}
{$POP}

{$HINT 'H2'}
{$WARNING 'W2'}

{$PUSH}
{$HINTS OFF}
{$HINT 'HiddenHint2'}
{$PUSH}
{$WARNINGS OFF}
{$WARNING 'HiddenWarning2'}
{$POP}
{$HINT 'HiddenHint3'}
{$WARNING 'W3'}
{$POP}

{$HINT 'H3'}
{$WARNING 'W4'}
