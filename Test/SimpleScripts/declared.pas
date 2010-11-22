type TMyRecord = record
      Dummy : Integer;
   end;

var myVar : String = IntToStr(2);

if Declared('dummy') then PrintLn('bug dummy');
if not Declared('TObject') then PrintLn('bug TObject');
if not Declared('TObject.Create') then PrintLn('bug TObject.Create');
if not Declared('Internal.TObject.Create') then PrintLn('bug Internal.TObject.Create');
if not Declared('Internal.Sin') then PrintLn('bug Internal.Sin');

if not Declared('TMyRecord') then PrintLn('bug TMyRecord');
if not Declared('TMyRecord.Dummy') then PrintLn('bug TMyRecord.Dummy');
if Declared('TMyRecord.Oops') then PrintLn('bug TMyRecord.Oops');

{$if Declared('dummy')}
PrintLn('Dummy is declared (bug)');
{$endif}
{$if not Declared('dummy')}
PrintLn('Dummy is not declared');
{$endif}

