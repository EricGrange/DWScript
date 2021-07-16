type TAlertType = enum (
     weightUpper,
     weightLower
  );

function AddAlert(alert : TAlertType) : set of TAlertType ;
begin
  var alerts : set of TAlertType ;
  Include(alerts,alert);
  Result:= alerts ;
end ;

var alert := TAlertType.weightLower ;
var alerts := AddAlert(alert);

PrintLn(TAlertType.weightUpper in alerts);
PrintLn(TAlertType.weightLower in alerts);