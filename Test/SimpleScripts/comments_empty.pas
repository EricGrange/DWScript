PrintLn('a');
{}
PrintLn('b');
(**)
PrintLn('c');
/**/
PrintLn('d');

procedure Populate1;
Begin
  Println('Hello 1') ;
  (*
  (**)
End;

procedure Populate2;
Begin
  Println('Hello 2') ;
  /*
  /**/
End;

procedure Populate3;
Begin
  Println('Hello 3') ;
  {
  {}
End;

Populate1;
Populate2;
Populate3;