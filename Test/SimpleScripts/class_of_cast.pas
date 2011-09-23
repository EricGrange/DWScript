type
  TBaseTemplate = class
    class procedure Execute;
  end;
  
type
  TSubTemplate = class(TBaseTemplate);

type
  TBaseTemplateClass = class of TBaseTemplate;

procedure Foo(FooClass: TBaseTemplateClass);
begin
   PrintLn(FooClass.ClassName);
end;

class procedure TBaseTemplate.Execute;
begin
  Foo(ClassType as TBaseTemplateClass);
  Foo(TBaseTemplateClass(ClassType));
end;

TBaseTemplate.Execute;
TSubTemplate.Execute;