var vi : Variant = 5;
var vf : Variant = 5.5;
var vs : Variant = 'world';

PrintLn(vf*3.5);
PrintLn(Round(vf*3));

PrintLn(3.5*vf);
PrintLn(Round(3*vf));

PrintLn(vi*3.5);
PrintLn(Round(vi*3));

PrintLn(3.5*vi);
PrintLn(Round(3*vi));

PrintLn('hello '+vs);
PrintLn(vs+' hello');

PrintLn(55/vf);
PrintLn(vf/5);

PrintLn(10-vf);
PrintLn(vf-5);

PrintLn(vi-vf);
PrintLn(vf-vi);

PrintLn(-vi);
PrintLn(-vf);

const v1 : Variant = 5;

PrintLn(2*v1);