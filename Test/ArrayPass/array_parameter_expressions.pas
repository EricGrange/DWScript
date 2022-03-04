var a : array of Float := [0];

var x := 1.5;

a.Add( x*2 );

PrintLn(a.IndexOf( x * 2));

a.Insert(1, x*4 );

a.Remove( x*2 );

var  s := a.Map(FloatToStr);

PrintLn(s.Join(';'));
