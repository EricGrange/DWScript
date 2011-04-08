type
   TBaseClassA = class

      Alpha : Integer;
      property Beta : Integer read Alpha;
      procedure Gamma;

      Beta : Integer;
      property Gamma : Integer read Alpha;
      procedure Alpha;

   end;
   
type   
   TBaseClassB = class

      Alpha : Integer;
      property Beta : Integer read Alpha;
      procedure Gamma;

      Gamma : Integer;
      property Alpha : Integer read Alpha;
      procedure Beta;  

   end;
   
type   
   TBaseClassC = class

      Alpha : Integer;
      property Beta : Integer read Alpha;
      procedure Gamma;

      Alpha : Integer;
      property Beta : Integer read Alpha;
      procedure Gamma;
	  
   end;      