// Adapted from Luis Carlos F. Dias version from http://99-bottles-of-beer.net/

type
   TAbstractSinger = class
		private
			FLine : String;
			FCanSing : Boolean;
			function GetLine : String;
			begin
				Result := FLine;
			end;
         
      protected
			procedure SetLine(aLine : String); virtual;
			
		public
			method Sing : String; virtual; abstract;

			property CanSing : Boolean read FCanSing write FCanSing;
			property Line : String read GetLine write SetLine;
		end;

type		
	TBottlesSinger = class(TAbstractSinger)
		private
			FCounter: integer;
			
			function EvalS: String;
			procedure SetCounter(aValue: integer);
			procedure PrepareLine;
			
			property Counter: integer read FCounter write SetCounter;
			
		public
			constructor Create;
			
			method Sing : String; override;
	end;

const
	CRLF : String = '' + #13#10;


{ TBottlesSinger }

procedure TAbstractSinger.SetLine(aLine : String);
begin
	FLine := aLine;
end;

{ TBottlesSinger }

constructor TBottlesSinger.Create;
begin
	FCounter := 99;
	CanSing := true;
end;

function TBottlesSinger.EvalS: String;
var
	res : string;
begin
	res := 's';
	if Counter = 1 then
		res := '';
	result := res;
end;

procedure TBottlesSinger.PrepareLine;
var
	lineForOne, lineForMore, lineForNone: String;
begin
	lineForMore:= IntToStr(Counter)
                +' bottles of beer on the wall, ' + IntToStr(Counter) 
                +' bottles of beer.' + CRLF 
                +'Take one down and pass it around, ' + IntToStr(Counter-1) 
                +' bottle' + EvalS + ' of beer on the wall.' + CRLF;

   lineForOne:= '1 bottle of beer on the wall, 1 bottle of beer.' + CRLF
               +'Take one down and pass it around,'
               +' no more bottles of beer on the wall.' + CRLF;

	lineForNone:= 'No more bottle of beer on the wall,'
                +' no more bottles of beer.' + CRLF
                +'Go to the store and buy some more,'
                +' 99 bottles of beer on the wall.' + CRLF;

	case Counter of
		1 : begin
			Line := lineForOne;
			Counter := Counter-1;
		end;
		0 : begin
			Line := lineForNone;
			Counter := 99;
			CanSing := false;
      end;
	else
		Line := lineForMore;
      Counter := Counter-1;
	end;
end;

procedure TBottlesSinger.SetCounter(aValue : integer);
begin
   FCounter := aValue;
end;

method TBottlesSinger.Sing : String;
begin
   PrepareLine;
   Result := Line;
end;

var Singer := TBottlesSinger.Create ;

while Singer.CanSing do
   PrintLn(Singer.Sing) ;
