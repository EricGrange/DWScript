type class1 = class
  method method1(); virtual ; abstract;
end;

type class2 = class (class1)
  method method1(); override;
  method method1(); override;
end;