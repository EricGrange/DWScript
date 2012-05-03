type
   IHello = interface
      procedure Hello;
   end;

type
   IWorld = interface
      procedure World;
   end;
   
type
   TTest = partial class (IHello)
      procedure Hello; begin PrintLn('Hello'); end;
   end;
   
type
   TTest = partial class (IWorld)
      procedure World; begin PrintLn('World'); end;
   end;

var t := new TTest;
var ih : IHello := t;
var iw : IWorld := t;

ih.Hello;
iw.World;