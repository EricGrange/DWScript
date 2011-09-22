// array params are not supported because of dwsComConnector.DispatchInvoke limitations
// if you want to change that, feel free to submit source code upgrades!

var DispVar : ComVariant;

procedure Bug;
begin
end;

DispVar.methodCall(Bug);
DispVar.methodCall(@Bug);