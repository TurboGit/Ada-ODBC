------------------------------------------------------------------------------
--                         Databases.Select_Query                           --
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
--     Gestion des requetes SQL Select. Cette abstraction est de plus haut
--     niveau que celle dans Databases. Ici, il n'y a pas de notion de
--     "column binding", c'est le systeme qui se charge de cette
--     tache. Chaque requete comporte un Context qui permet de recuperer
--     par des Fetch successifs l'ensemble des reponses.
--
--     Les donnees des colonnes sont retournees sous forme de chaine de
--     caracteres.
--
--     Execute
--        execution d'une commande SQL Select. Il est possible d'executer
--        une autre commande SQL qu'un Select mais dans ce cas il est
--        recommande d'utiliser Databases.Execute.
--
--     Fetch
--        recuperation de la ligne suivante.
--
--     Number_Of_Columns
--        donne le nombre de colonnes dans le lot resultat.
--
--     Get_Value
--        recupere la valeur d'une colonne de la ligne resultat.

package Databases.Select_Query is

   type Select_Data is private;

   procedure Execute
     (DB         : in     Database;
      Statement  : in     String;
      Context    :    out Select_Data;
      Parameters : in     Parameter_Set    := No_Parameter;
      Cursor     : in     Databases.Cursor := No_Cursor);

   procedure Fetch
     (Context : in     Select_Data;
      Found   :    out Boolean);

   function Number_Of_Columns (Context : in Select_Data) return Positive;

   function Get_Value
     (Context : in Select_Data;
      Column  : in Positive) return String;

   function Get_Name
     (Context : in Select_Data;
      Column  : in Positive) return String;

   function Get_Model_Name
     (Context : in Select_Data;
      Column  : in Positive) return String;

   function Simple_Query
     (Query, Driver, UID, PASSWD : in String) return String;
   --  Returns an atomic information from a database, that is
   --  the first element (first column of first row) of a query's result

private

   type Column_Data is record
      Name  : Unbounded_String;
      Value : Unbounded_String;
      Model : aliased ODBC.SWORD;
   end record;

   type Columns_Data is array (Positive range <>) of Column_Data;
   type Columns_Data_Access is access Columns_Data;

   type Select_Data is record
      Base                 : Database;
      DBC_Statement_Handle : aliased ODBC.HSTMT;
      Columns              : Columns_Data_Access;
      SQL                  : String_Access;
   end record;

end Databases.Select_Query;
