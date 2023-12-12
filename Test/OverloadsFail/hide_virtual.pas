type A = class
  procedure Toto; virtual;
end;

type B = class(A)
  procedure Toto; overload;
end;