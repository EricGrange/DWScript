function Func1n(a : Integer) : Integer; forward;
function Func2n(a : Integer) : Integer; forward;
function Func3n(a : Integer) : Integer; forward;
procedure Func4n(a : Integer); forward;
function Func5n(a : Integer) : Integer; forward;
function Func6n(a : Integer = 1) : Integer; forward;
function Func7n(a : Integer = 1) : Integer; forward;

function Func1n(const a : Integer) : Integer; begin Result:=1 end;
function Func2n(var a : Integer) : Integer; begin Result:=1 end;
procedure Func3n(a : Integer); begin Result:=1 end;
function Func4n(a : Integer) : Integer; begin end;
function Func5n(a : Integer = 1) : Integer; begin Result:=1 end;
function Func6n(a : Integer) : Integer; begin Result:=1 end;
function Func7n(const a : Integer) : Integer; begin Result:=1 end;

function Func1c(const a : Integer) : Integer; forward;
function Func2c(const a : Integer) : Integer; forward;
function Func3c(const a : Integer) : Integer; forward;

function Func1c(a : Integer) : Integer; begin Result:=1 end;
function Func2c(var a : Integer) : Integer; begin Result:=1 end;
function Func3c(const a : Integer = 1) : Integer; begin Result:=1 end;

function Func1v(var a : Integer) : Integer; forward;
function Func2v(var a : Integer) : Integer; forward;

function Func1v(a : Integer) : Integer; begin Result:=1 end;
function Func2v(const a : Integer) : Integer; begin Result:=1 end;