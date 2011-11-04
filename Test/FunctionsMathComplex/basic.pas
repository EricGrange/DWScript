var c : TComplex;

const c2 : TComplex = (Re: 10; Im: 20);

PrintLn(ComplexToStr(Complex(1, 2)));
PrintLn(ComplexToStr(c2));

c := Complex(1, -2);

PrintLn(ComplexToStr(c));

c:=ComplexAdd(c, Complex(4, 8));

PrintLn(ComplexToStr(c));

c := c + c2;

PrintLn(ComplexToStr(c));

c := c + c;

PrintLn(ComplexToStr(c));

c := c - c2;

PrintLn(ComplexToStr(c));

PrintLn(ComplexToStr(Complex(1, 0)));
