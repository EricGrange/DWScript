type
   TFloatPoint = record
      x, y: Float;
   end;

type
   TSomeClass = class
   private
      function GetFloatPoint: TFloatPoint;
      procedure SetFloatPoint(Value: TFloatPoint);
   public
      property FloatPoint: TFloatPoint read GetFloatPoint write
SetFloatPoint;
   end;

var SomeClass := TSomeClass;

SomeClass.FloatPoint // 