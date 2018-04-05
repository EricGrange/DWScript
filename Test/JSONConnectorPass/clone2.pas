var v := JSON.Parse(#'
   {
   "str":"world",
   "num":123,
   "bool":true
   }');

PrintLn(v.Clone());
PrintLn(v['str'].Clone());
PrintLn(v['num'].Clone());
PrintLn(v['bool'].Clone());

