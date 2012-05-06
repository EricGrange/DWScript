Type

 TAction = Class Abstract
   Function GetTitle : String; Virtual; Abstract;
   Function GetHint : String; Virtual; Abstract;
 End;

Type

 TFileOpenAction = Class(TAction)
   Function GetTitle : String; Override;
   Begin
   End;
 End;

var a := TAction.Create;
PrintLn(a.GetHint);

var f := TFileOpenAction.Create;
PrintLn(f.GetHint);
