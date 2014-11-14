type TEnum = flags (Alpha, Beta, Gamma);

procedure PrintInt(i : Integer);
begin
	Print('int ');
	PrintLn(i);
end;

procedure PrintEnum(e : TEnum);
begin
	Print('enum ');
	PrintLn(e);
end;

PrintInt(TEnum.Alpha);
PrintInt(TEnum.Beta);
PrintInt(TEnum.Gamma);

PrintEnum(TEnum.Alpha);
PrintEnum(TEnum.Beta);
PrintEnum(TEnum.Gamma);

PrintInt(TEnum.Alpha or TEnum.Gamma);
PrintInt(TEnum.Beta xor TEnum.Gamma);
PrintInt(TEnum.Beta and TEnum.Alpha);

PrintEnum(TEnum.Alpha or TEnum.Gamma);
PrintEnum(TEnum.Beta xor TEnum.Gamma);
PrintEnum(TEnum.Beta and TEnum.Alpha);
