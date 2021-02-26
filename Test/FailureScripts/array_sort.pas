type
	TRec = record
		Field : Boolean;
	end;
	
var ab : array of Boolean;
ab.Sort(1, 2, 3);

var ar : array of TRec;
ar.Sort;

ar.Sort(@CompareStr);
ar.Sort(CompareStr);

