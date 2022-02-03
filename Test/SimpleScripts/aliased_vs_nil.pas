type TMy = class end;
type TMyClass = class of TMy;
type IMy = interface end;

type AMy = TMy;
var c : AMy;
if c = nil then PrintLn('class nil');

type AMyClass = TMyClass;
var m : AMyClass;
if m = nil then PrintLn('metaclass nil');

type AIntf = IMy;
var i : AIntf;
if i = nil then PrintLn('intf nil');