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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{
    This unit wraps SynDBOracle from Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info
}
unit dwsSynOleDBDatabase;

interface

uses
   SynDB, SynOleDB,
   dwsUtils, dwsDatabase, dwsExprs,
   dwsSynDBDatabase;

type

   TOleDBConnectionPropertiesClass = class of TOleDBConnectionProperties;

   TdwsSynOleDBDataBaseFactory = class (TdwsDataBaseFactory)
      private
         FConnPropsClass : TOleDBConnectionPropertiesClass;

      public
         constructor Create(cls : TOleDBConnectionPropertiesClass);

         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;

         property ConnPropsClass : TOleDBConnectionPropertiesClass read FConnPropsClass write FConnPropsClass;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure RegisterSynOleDBDriver(const name : String; cls : TOleDBConnectionPropertiesClass);
begin
   TdwsDatabase.RegisterDriver(name, TdwsSynOleDBDataBaseFactory.Create(cls));
end;

// ------------------
// ------------------ TdwsSynOleDBDataBaseFactory ------------------
// ------------------

// Create
//
constructor TdwsSynOleDBDataBaseFactory.Create(cls : TOleDBConnectionPropertiesClass);
begin
   inherited Create;
   FConnPropsClass:=cls;
end;

// CreateDataBase
//
function TdwsSynOleDBDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynDBDataBase;
begin
   db:=TdwsSynDBDataBase.Create(FConnPropsClass, parameters);
   Result:=db;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterSynOleDBDriver('OleDB Oracle', TOleDBOracleConnectionProperties);
   RegisterSynOleDBDriver('OleDB MSOracle', TOleDBMSOracleConnectionProperties);
   RegisterSynOleDBDriver('OleDB MSSQL', TOleDBMSSQLConnectionProperties);
   RegisterSynOleDBDriver('OleDB MSSQL2005', TOleDBMSSQL2005ConnectionProperties);
   RegisterSynOleDBDriver('OleDB MSSQL2008', TOleDBMSSQL2008ConnectionProperties);
   RegisterSynOleDBDriver('OleDB MSSQL2012', TOleDBMSSQL2012ConnectionProperties);
   RegisterSynOleDBDriver('OleDB MySQL', TOleDBMySQLConnectionProperties);
   {$ifndef WIN64}
   RegisterSynOleDBDriver('OleDB Jet', TOleDBJetConnectionProperties);
   {$endif}
   RegisterSynOleDBDriver('OleDB AS400', TOleDBAS400ConnectionProperties);
   RegisterSynOleDBDriver('OleDB ODBC', TOleDBODBCSQLConnectionProperties);

end.
