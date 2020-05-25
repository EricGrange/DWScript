{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Eric Grange / Creative IT                               }
{                                                                      }
{**********************************************************************}
unit dwsGabelouStrings;

{$I ../dws.inc}

interface

const

   GAB_HintMessage = 'Gabelou: %s';

   GAB_CamelCaseParameters_Name = 'camelCase parameters';
   GAB_CamelCaseParameters_Description = 'Parameter names should follow camelCase and start with a lower-case character';

   GAB_CamelCaseLocalVariables_Name = 'camelCase local variables';
   GAB_CamelCaseLocalVariables_Description = 'Local variables names should follow camelCase and start with a lower-case character';

   GAB_PrefixedClassVariables_Name = 'Prefixed class variables';
   GAB_PrefixedClassVariables_Description = 'Class variables whould be prefixed by "v" followed by PascalCase';

   GAB_PrefixedPrivateFields_Name = 'Prefixed private fields';
   GAB_PrefixedPrivateFields_Description = 'Private or protected fields should start with "F" followed by PascalCase.';

   GAB_PrefixedPublicFields_Name = 'Prefixed public fields';
   GAB_PrefixedPublicFields_Description = 'Public fields should follow PascalCase.';

   GAB_ConstsNamingRules_Name = 'Constant naming rules';
   GAB_ConstsNamingRules_Description = 'Constant names should either start with a "c" followed by PascalCase or be ALL_CAPS up to the first "_"';

   GAB_PascalCaseFunctions_Name = 'PascalCase functions';
   GAB_PascalCaseFunctions_Description = 'Function names should follow PascalCase and start with an upper-case character';

   GAB_PascalCaseProperties_Name = 'PascalCase properties';
   GAB_PascalCaseProperties_Description = 'Property names should follow PascalCase and start with an upper-case character';

   GAB_PascalCaseTypes_Name = 'PascalCase types';
   GAB_PascalCaseTypes_Description = 'Type names should follow PascalCase and start with an upper-case character';

   GAB_AttributeClassNaming_Name = 'Attribute class names';
   GAB_AttributeClassNaming_Description = 'Attribute class names should end with "Attribute" and not "T" prefix';

implementation

end.
