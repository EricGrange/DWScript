{
  Hanoi
}

procedure Hanoi(n:integer; x,y,z:string);
begin
 if n=1 then println('put disc from '+x+' to '+y) else
  begin 
   Hanoi(n-1,x,z,y);
   Hanoi(1,x,y,z);
   Hanoi(n-1,z,y,x);
  end;
end;

Hanoi(10,'A','B','C'); // 2^10 -1 iterations!
