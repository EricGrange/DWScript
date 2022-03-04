var Stack: array of Integer = [];

procedure SimpleProc;
begin
  PrintLn(Stack.Peek);
end;

Stack.Add(42);

// Works
PrintLn(Stack.Peek);

// Doesn't work
SimpleProc;
