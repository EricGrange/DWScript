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
   dwsSynDBDatabase, dwsDatabase, dwsExprs;

type

   TdwsSynOleDBOracleDataBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

   TdwsSynOleDBMSOracleDataBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

   TdwsSynOleDBMSSQLDataBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

   TdwsSynOleDBMySQLDataBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

   TdwsSynOleDBJetDataBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

   TdwsSynOleDBAS400DataBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

   TdwsSynOleDBODBCDataBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsSynOleDBOracleDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynOleDBOracleDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynDBDataBase;
begin
   db:=TdwsSynDBDataBase.Create(TOleDBOracleConnectionProperties, parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynOleDBMSOracleDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynOleDBMSOracleDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynDBDataBase;
begin
   db:=TdwsSynDBDataBase.Create(TOleDBMSOracleConnectionProperties, parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynOleDBMSSQLDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynOleDBMSSQLDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynDBDataBase;
begin
   db:=TdwsSynDBDataBase.Create(TOleDBMSSQLConnectionProperties, parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynOleDBMySQLDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynOleDBMySQLDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynDBDataBase;
begin
   db:=TdwsSynDBDataBase.Create(TOleDBMySQLConnectionProperties, parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynOleDBJetDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynOleDBJetDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynDBDataBase;
begin
   db:=TdwsSynDBDataBase.Create(TOleDBJetConnectionProperties, parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynOleDBAS400DataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynOleDBAS400DataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynDBDataBase;
begin
   db:=TdwsSynDBDataBase.Create(TOleDBAS400ConnectionProperties, parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynOleDBODBCDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynOleDBODBCDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynDBDataBase;
begin
   db:=TdwsSynDBDataBase.Create(TOleDBODBCSQLConnectionProperties, parameters);
   Result:=db;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsDatabase.RegisterDriver('OleDB Oracle', TdwsSynOleDBOracleDataBaseFactory.Create);
   TdwsDatabase.RegisterDriver('OleDB MSOracle', TdwsSynOleDBMSOracleDataBaseFactory.Create);
   TdwsDatabase.RegisterDriver('OleDB MSSQL', TdwsSynOleDBMSSQLDataBaseFactory.Create);
   TdwsDatabase.RegisterDriver('OleDB MySQL', TdwsSynOleDBMySQLDataBaseFactory.Create);
   TdwsDatabase.RegisterDriver('OleDB Jet', TdwsSynOleDBJetDataBaseFactory.Create);
   TdwsDatabase.RegisterDriver('OleDB AS400', TdwsSynOleDBAS400DataBaseFactory.Create);
   TdwsDatabase.RegisterDriver('OleDB ODBC', TdwsSynOleDBODBCDataBaseFactory.Create);

end.
