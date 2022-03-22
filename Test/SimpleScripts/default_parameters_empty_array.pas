
procedure Test(a : array of Integer = []); forward;

procedure Test(a : array of Integer = []);
begin
	PrintLn(a.Length.ToString +':' + a.Map(IntToStr).Join(','));
end;

type
   TTest = class
      constructor Create(p : String; parameters: array of string = []);
   end;

constructor TTest.Create(p : String; parameters: array of string = []);
begin
	PrintLn(p + parameters.Join(',') + ':' + parameters.Length.ToString);
end;

Test;
Test([1]);

var t := new TTest('hello');
t := new TTest('world', []);
t := new TTest('world', [ 'foo', 'bar' ]);
