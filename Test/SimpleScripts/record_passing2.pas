type 
   TRect = record
      Left, Right : Integer;
      function Width : Integer;
      begin
         Result:=Right-Left;
      end;
   end;

function P_Copy(R: TRect): Integer;
begin
   Result := R.Width;
end;

function P_Const(const R: TRect): Integer;
begin
   Result := R.Width;
end;

function P_Var(var R: TRect): Integer;
begin
   Result := R.Width;
end;

procedure Test_Copy_Const;
var
   R: TRect;
Begin
   R.Left := 0;
   R.Right := 64;
   PrintLn(P_Copy(R));
   PrintLn(P_Const(R));
End;

procedure Test_Copy_Var;
var
   R: TRect;
Begin
   R.Left := 0;
   R.Right := 48;
   PrintLn(P_Copy(R));
   PrintLn(P_Var(R));
End;

procedure Test_Const_Var;
var
   R: TRect;
Begin
   R.Left := 0;
   R.Right := 24;
   PrintLn(P_Const(R));
   PrintLn(P_Var(R));
End;

Test_Copy_Const;
Test_Copy_Var;
Test_Const_Var;