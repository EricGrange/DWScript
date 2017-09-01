Type

 THelper = Helper For TObject
 Procedure proc;
 Begin
  PrintLn(ClassName+'.Proc!');
 End;
 End;

{$IF Declared('THelper.Proc')}
 THelper.Proc(TObject.Create);
{$ELSE}
 {$FATAL 'THelper.Proc is not declared'}
{$ENDIF}

{$IF Declared('THelper.ProcBug')}
 {$FATAL 'THelper.ProcBug is declared'}
{$ELSE}
 PrintLn('THelper.ProcBug not declared');
{$ENDIF}

{$IF Declared('TObject.Proc')}
 TObject.Create.Proc;
{$ELSE}
 {$FATAL 'TObject.Proc is not declared'}
{$ENDIF}

{$IF Declared('TObject.ProcBug')}
 {$FATAL 'TObject.ProcBug is declared'}
{$ELSE}
 PrintLn('TObject.ProcBug not declared');
{$ENDIF}