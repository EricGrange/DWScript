var i : Integer;

case i of
   TObject(nil).Free..PrintLn : ;
   Print('bug'), 2 : ;
end;