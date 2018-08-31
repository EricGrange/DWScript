var ai := 1;
var bi := 2;
var af := 1.5;
var bf := 2.5;

PrintLn('---ints');
PrintLn(CompareNum(ai, bi));
PrintLn(CompareNum(bi, ai));
PrintLn(ai.Compare(ai));
PrintLn(bi.Compare(bi));

PrintLn('---floats');
PrintLn(CompareNum(af, bf));
PrintLn(CompareNum(bf, af));
PrintLn(af.Compare(af));
PrintLn(bf.Compare(bf));

PrintLn('---mixed');
PrintLn(ai.Compare(af));
PrintLn(af.Compare(ai));
PrintLn(CompareNum(bi, af));
PrintLn(CompareNum(af, bi));

