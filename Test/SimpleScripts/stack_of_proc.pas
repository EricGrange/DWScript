var s : array of procedure;

procedure Hello;
begin
   PrintLn('Hello');
end;

procedure World;
begin
   PrintLn('World');
end;

s.Push(World);
s.Push(Hello);

s.Pop();
s.Pop();
try
   s.Pop();
except
   on e : Exception do
      PrintLn(e.Message);
end;