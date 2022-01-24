var a := JSON.Parse('"CHECK"');
PrintLn(a = 'CHECK');  // true
PrintLn(a in ['CHECK']); // true

PrintLn(a in [a]); // true

var v : Variant = 1;

PrintLn(a in [v]); // false

PrintLn(v in [1]); // true
PrintLn(v in ['1']); // true
PrintLn(v in ['2']); // false