const
  X: array [0..1, 0..2] of Integer = [[1, 2, 3], [4, 5, 6]];
  
PrintLn(X[0,0]);  
  
for var i := Low(X) to High(X) do
   for var j := Low(X[i]) to High(X[i]) do
      PrintLn(IntToStr(i)+', '+IntToStr(j)+' : '+IntToStr(X[i, j]));