if ConditionalDefined('ALPHA') then PrintLn('ALPHA 1');
if ConditionalDefined('BETA') then PrintLn('BETA 1');

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

if ConditionalDefined('ALPHA') then PrintLn('ALPHA 2');
if ConditionalDefined('BETA') then PrintLn('BETA 2');

{$define BETA}

if ConditionalDefined('ALPHA') then PrintLn('ALPHA 3');
if ConditionalDefined('BETA') then PrintLn('BETA 3');

{$undef ALPHA}

if ConditionalDefined('ALPHA') then PrintLn('ALPHA 4');
if ConditionalDefined('BETA') then PrintLn('BETA 4');

{$if Defined('ALPHA')}
PrintLn('Alpha defined');
{$else}
PrintLn('Alpha not defined');
{$endif}

