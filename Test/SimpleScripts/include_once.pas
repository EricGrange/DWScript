PrintLn('before');
{$include_once 'include.inc'}
{$include_once 'include.inc'}
PrintLn('after');
{$include 'include.inc'}
{$include_once 'include.inc'}
PrintLn('done');