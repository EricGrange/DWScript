
var tab := new TabularData;

var v := new Float[1000];
for var i := 0 to v.High do
   v[i] := i+1;

tab.AddColumn('v', v);

tab.EvaluateNewColumn('sqr', [ '"v"', 'sqr' ]);
tab.EvaluateNewColumn('sub', [ '"sqr"', '"v"', '-' ]);
tab.DropColumn('v');
tab.DropColumn('sqr');
PrintLn(tab.EvaluateAggregate('sum', [ '"sub"' ]));
