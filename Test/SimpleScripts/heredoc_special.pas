var v := #'hello "world" !';
PrintLn(v);

v:=#"hello 'again' !";
PrintLn(v);

PrintLn(#'
  first form
  ''single''
  ""double"" 
  with quotes');
  
PrintLn(#"
  second form
  ''single''
  ""double"" 
  with quotes");