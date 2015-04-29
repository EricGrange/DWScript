var v : Variant;

PrintLn(v ?? TObject);

PrintLn(TObject ?? 1);

var s : String;

PrintLn(s ?? 1);
PrintLn(s ?? Null);

var p : procedure;

PrintLn(p ?? '');
