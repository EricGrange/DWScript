Type TProc = procedure;

var a : Array Of TProc;

a.add(@nil); // error here (@nil)
a.add(TProc); // error here
a.add();