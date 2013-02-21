var a : array of Integer;

Print(a.Low(1));

Print(a.setlength(1, 2));

a.Delete;

a.Delete('bug');
a.Delete(1, 'bug');

var i := a.indexOf(1, 'bug');
i := a.indexOf(1, 2, 3);

a.Insert;
a.Insert('bug', 1);
a.Insert(1, 'bug');

a.swap(1, 2, 3);
a.swap('bug', 1);
a.swap(1, 'bug');

var b := a.copy('bug', 1);
b := a.copy(1, 'bug');
b := a.copy(1, 2, 3);
