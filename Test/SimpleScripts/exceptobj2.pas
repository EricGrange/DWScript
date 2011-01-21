Try
  Raise Exception.Create('1');
Finally
  If Assigned(ExceptObject) Then
     Raise Exception.Create('2');
End;
