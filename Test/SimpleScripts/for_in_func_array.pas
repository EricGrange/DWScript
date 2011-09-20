Type TProc = Procedure;

procedure Hello;
begin
   PrintLn('Hello');
end;

procedure World;
begin
   PrintLn('World');
end;

Var arr : Array Of TProc;

arr.Add(Hello);
arr.Add(World);
arr.Add(World);
arr.Add(Hello);

Var proc : TProc;

For proc In arr Do
   proc;