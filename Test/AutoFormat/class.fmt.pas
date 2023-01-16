type
	TTest = class
		private
			FField : Integer;
			FOther : class of TTest;

		protected
			function GetField : Integer;
			procedure SetField(v : Integer);

		public
			constructor Create(hello, world : Integer); override;

			property Prop : Integer read GetField write SetField;
			property Direct;

			class function Foobar : array of TTest;
	end;

constructor TTest.Create(hello, world : Integer);
begin
	inherited Create;
	FField := world;
end;

function TTest.GetField : Integer;
begin
	if foo then
		exit FField
	else Result := FField * 2 + 1
end;

class function Foobar : array of TTest;
begin
	Result := nil;
end;