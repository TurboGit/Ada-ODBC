------------------------------------------------------------------------------
--                                Databases                                 --
--                                                                          --
--                        Copyright (C) 1999-2018                           --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------
--
--  NB: a recent version of Databases may be located
--      here: https://github.com/TurboGit/Ada-ODBC
--
--     *** Constructors / Destructors
--
--     Connect (DB, Driver, UID, Passwd)
--        Ouvre une base de donnees ODBC.
--        Driver : DSN (Data Source Name) de la base ODBC.
--        UID, Passwd : nom et mot de passe de l'utilisateur.
--        => SQL_Error
--
--     Close
--        ferme la base de donnees.
--
--     Bind (Query, Column, Name, Address, Size, Data_Motel)
--        permet de designer les champs a recuperer lors du Select.
--        Column  : un numero de colonne.
--        Name    : le nom de la colonne.
--        Address : pointe sur une variable qui contiendra la valeur de la
--                  colonne.
--        Size    : la taille en octect de cette variable.
--        Data_Model : le type de donnees dans la colonne.
--        => Data_Type_Error
--
--     Query (Query, Column, Operator, Value)
--        permet de specifier un masque (clause Where) pour les colonnes.
--        Un AND est utilise entre toutes les conditions sur les colonnes pour
--        creer la clause Where.
--
--     Query (Query, Where_Clause, SQL_Clause)
--        La condition Were_Clause est ajoutee a la fin de la clause Where
--        construite a partir des masques (voir Query ci-dessus) avec un
--        operateur AND. Si aucun masque n'est specifie alors on utilise
--        uniquement cette clause.
--        SQL_Clause est ajoute a la fin du select. Ceci est a utiliser pour
--        ajouter un tri (ORDER BY) par exemple.
--
--     Reset_Query
--        efface les informations de lien (Bind ci-dessus) et les masque
--        (Query ci-dessus) pour les colonnes.
--
--     *** Accessors :
--
--     Name
--        retourne le nom de la colonne Column.
--
--     Query_Value
--        retourne le masque de selection pour la colonne Column.
--        la clause Where est un And entre tous les masques.
--
--     Last
--        retourne la taille de la colonne Column apres un Select.
--        cette fonction est utilisable avec les models : SQL_CHAR et
--        SQL_VARCHAR.
--
--
--     Get_SQL_Select
--        retourne la requete sous la forme d'une chaine de carateres.
--        => Data_Type_Error
--
--     SQL_Select (DB, Query, Table, Cursor)
--        execute un select avec les colonnes definie par Bind sur Table
--        et avec la clause where definie par la commande Query.
--        => SQL_Error
--
--     Fetch (Query, Found)
--        retourne la ligne suivante pour la Query.
--        => SQL_Error
--
--     Parameter
--        permet de construire un objet Parameter_Set. Ce sont des
--        parametres d'instruction SQL (SQLBindParameter).
--
--     Execute (DB, Command, Parameters)
--        execute Command SQL sur la base DB. Il est possible d'appeler une
--        procedure cataloguee et de passer des parameteres dans
--        parameters. La construction d'un objet Parameter_Set se fait par
--        l'intermediaire de la procedure Parameter.
--        => SQL_Error

with Ada.Strings.Unbounded;
with Win32.Sql;
with Win32.Sqlext;

with System;

package Databases is

   type Switch is (On, Off);

   type Database is limited private;
   type Cursor is private;

   No_Cursor : constant Cursor;

   Maximum_Number_Of_Column : constant := 100;

   subtype Column_Number is Natural range 0 .. Maximum_Number_Of_Column;

   type Select_Statement (N : Column_Number) is limited private;

   type For_Update_Options is (None, Yes, Yes_No_Wait);

   type Operators is (Equal, Not_Equal, Like);

   type Parameter_Set (N : Column_Number) is private;

   No_Parameter : constant Parameter_Set;

   --  exceptions

   SQL_Error       : exception;
   Data_Type_Error : exception;

   --  data type

   type Data_Type is (SQL_CHAR, SQL_VARCHAR,
                      SQL_DATE, SQL_TIME, SQL_TIMESTAMP,
                      SQL_NUMERIC, SQL_DECIMAL, SQL_INTEGER, SQL_SMALLINT,
                      SQL_FLOAT, SQL_REAL, SQL_DOUBLE);

   type Date_Record is record
      Year  : Short_Integer;
      Month : Short_Integer;
      Day   : Short_Integer;
   end record;

   SQL_PARAM_INPUT  : constant := Win32.Sqlext.SQL_PARAM_INPUT;
   SQL_PARAM_OUTPUT : constant := Win32.Sqlext.SQL_PARAM_OUTPUT;

   --  -----------------------------------------------------------------  --
   --  Connect-Open / Close

   procedure Connect (DB : in out Database; Driver, UID, PASSWD : in String);
   procedure Close   (DB : in out Database);

   --  -----------------------------------------------------------------  --
   --  Columns binding

   procedure Bind
     (Query      : in out Select_Statement;
      Column     : in     Column_Number;
      Name       : in     String;
      Address    : in     System.Address;
      Size       : in     Natural;
      Data_Model : in     Data_Type);

   procedure Query
     (Query    : in out Select_Statement;
      Column   : in     Column_Number;
      Operator : in     Operators;
      Value    : in     String);

   procedure Query
     (Query        : in out Select_Statement;
      Where_Clause : in     String := "";
      SQL_Clause   : in     String := "");

   procedure Reset_Select (Query : in out Select_Statement);

   --  -----------------------------------------------------------------  --
   --  Accessors

   function Name
     (Query  : in Select_Statement;
      Column : in Column_Number) return String;

   function Query_Value
     (Query  : in Select_Statement;
      Column : in Column_Number) return String;

   function Last
     (Query  : in Select_Statement;
      Column : in Column_Number) return Natural;

   --  -----------------------------------------------------------------  --
   --  Actions

   function Get_SQL_Select
     (Query : in Select_Statement;
      Table : in String) return String;

   procedure SQL_Select
     (DB      : in     Database;
      Query   : in out Select_Statement;
      Table   : in     String;
      Cursor  : in     Databases.Cursor := No_Cursor);

   procedure Fetch
     (Query : in     Select_Statement;
      Found :    out Boolean);

   procedure Parameter
     (Parameters : in out Parameter_Set;
      Column     : in     Column_Number;
      Mode       : in     Natural;
      Address    : in     System.Address;
      Size       : in     Natural;
      Data_Model : in     Data_Type);

   procedure Execute
     (DB         : in Database;
      Command    : in String;
      Parameters : in Parameter_Set := No_Parameter);

   procedure Free (Query : in out Select_Statement);

private

   package ODBC     renames Win32.Sql;
   package ODBC_EXT renames Win32.Sqlext;

   procedure Check_SQL_Error
     (DB               : in Database;
      RC               : in ODBC.RETCODE;
      Procedure_Name   : in String;
      Error_Message    : in String := "";
      Statement_Handle : in ODBC.HSTMT := System.Null_Address);
   use Ada.Strings.Unbounded;

   type String_Access is access all String;

   -------------------------------------------------------------------------

   type Database is record
      DBC_Handle             : aliased ODBC.HDBC;
      DBC_Environment_Handle : aliased ODBC.HENV;
      Driver, UID, PASSWD    : String_Access;
   end record;

   type Field_Data is record
      Name        : Unbounded_String;
      Data_Model  : Data_Type;
      Query_Value : Unbounded_String;
      Operator    : Operators;
      Address     : System.Address;
      Size        : ODBC.SDWORD;
      Last        : aliased ODBC.SDWORD;
   end record;

   type Fields_Array is array (Column_Number range <>) of Field_Data;

   type DB_Access is access Database;

   -------------------------------------------------------------------------

   type Select_Statement (N : Column_Number) is record
      Base                 : Database;
      DBC_Statement_Handle : aliased ODBC.HSTMT;
      SQL                  : String_Access;
      Fields               : Fields_Array (1 .. N);
      For_Update           : For_Update_Options := None;
      Where_Clause         : Unbounded_String;
      SQL_Clause           : Unbounded_String;
   end record;

   -------------------------------------------------------------------------

   type Parameter_Data is record
      Mode        : ODBC.SWORD;
      Data_Model  : Data_Type;
      Address     : System.Address;
      Size        : ODBC.SDWORD;
      Last        : aliased ODBC.SDWORD;
   end record;

   type Parameter_Array is array (Column_Number range <>) of Parameter_Data;

   type Parameter_Set (N : Column_Number) is record
      Parameters : Parameter_Array (1 .. N);
   end record;

   No_Parameter : constant Parameter_Set :=
                    (N => 0, Parameters => (1 .. 0 => <>));

   -------------------------------------------------------------------------

   type Cursor is new Unbounded_String;

   No_Cursor : constant Cursor := Cursor (Null_Unbounded_String);

end Databases;
