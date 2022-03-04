Type TProc = procedure;

procedure Test;
begin
end;

var a : Array Of TProc;
var proc : TProc;

a.add(proc);

If nil In a Then
  println('0');
If proc In a Then
  println('1');
If @proc In a Then
  println('2'); 
  
If Test In a Then
  println('bug1'); 
If @Test In a Then
  println('bug2'); 
  
