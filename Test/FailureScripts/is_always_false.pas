type T1 = class end;
type T2 = class end;

type T1s = class (T1) end;
type T2s = class (T2) end;

var o1 : T1;
var o2 : T2;

if o1 is T1 then ;
if o1 is T2 then ;
if o2 is T1 then ;
if o2 is T2 then ;

var o1s : T1s;
var o2s : T2s;

if o1s is T1 then ;
if o1s is T2 then ;
if o2s is T1 then ;
if o2s is T2 then ;

if o1 is T1s then ;
if o1 is T2s then ;
if o2 is T1s then ;
if o2 is T2s then ;

