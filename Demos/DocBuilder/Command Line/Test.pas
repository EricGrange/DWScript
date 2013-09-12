unit Test;

interface

type
   TInt = Integer;

   TRecord = record
      Val: TInt;
      Name: String;
   end;

   TTestClass = class
   private
      FSomething: String;
   public
      Field: Integer;

      constructor Create;

      procedure TestProc(Parameter: String);
      function TestFunction(Parameter: Integer): Integer;

      property Something: String read FSomething;
   end;

const
   CValue: TInt = 20;

procedure TestSimple(Parameter: String);

implementation

procedure TestSimple(Parameter: String);
begin
   // do nothing
end;


{ TTestClass }

constructor TTestClass.Create;
begin
   FSomething := 'Something';
   Field := 100;
end;

function TTestClass.TestFunction(Parameter: Integer): Integer;
begin
   Result := Parameter + CValue;
end;

procedure TTestClass.TestProc(Parameter: String);
begin
   TestFunction(10);
   TestSimple(Parameter);
end;

end.
