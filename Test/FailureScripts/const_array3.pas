var ad : array of Integer;
const cad : array of Integer = [1];

const a1 = ad;
const a2 = cad;

procedure Test1(const oa : array of Integer);
begin
   const a2 = oa;
end;

procedure Test2(const oa : array of const);
begin
   const a3 = oa;
end;