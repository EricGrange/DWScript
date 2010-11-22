if Defined('ALPHA') then PrintLn('ALPHA 1');
if Defined('BETA') then PrintLn('BETA 1');

{$if Defined('ALPHA')}
PrintLn('Alpha defined');
{$else}
PrintLn('Alpha not defined');
{$endif}

{$define ALPHA}

{$if Defined('ALPHA')}
PrintLn('Alpha defined');
{$else}
PrintLn('Alpha not defined');
{$endif}

if Defined('ALPHA') then PrintLn('ALPHA 2');
if Defined('BETA') then PrintLn('BETA 2');

{$define BETA}

if Defined('ALPHA') then PrintLn('ALPHA 3');
if Defined('BETA') then PrintLn('BETA 3');

{$undef ALPHA}

if Defined('ALPHA') then PrintLn('ALPHA 4');
if Defined('BETA') then PrintLn('BETA 4');

{$if Defined('ALPHA')}
PrintLn('Alpha defined');
{$else}
PrintLn('Alpha not defined');
{$endif}

