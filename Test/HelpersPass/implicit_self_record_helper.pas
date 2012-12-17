type
  TRec1 = record
    Field : String;
    procedure Test;
  end;
  
type
  TRecHelper1 = helper for TRec1
    procedure ABC;
  end;

procedure TRecHelper1.ABC;
begin
   PrintLn('ABC:'+Field);
end;

procedure TRec1.Test;
begin
  Self.ABC;
  ABC;
end;

var o : TRec1;
o.Field:='world';
o.Test;