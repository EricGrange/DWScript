case (1=1) of
   True : PrintLn('T');
   False : PrintLn('F');   
end;

case (1=1) of
   False : PrintLn('F');   
   True : PrintLn('T');
end;

case (1=1) of
   True : PrintLn('T');
else
   PrintLn('F');   
end;

case (1=1) of
   False : PrintLn('F');
else
   PrintLn('T');   
end;