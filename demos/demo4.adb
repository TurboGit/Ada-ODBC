------------------------------------------------------------------------------
--                                  Demo4                                   --
--                                                                          --
--                         Copyright (C) 2007-2018                          --
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
--  This is a sample program to show some ODBC features.
--  You must set up the "Clients_DB" ODBC string before running this demo.
--  See readme.txt for instructions.
--
--  It compile fine under GNAT 3.04a. You must add the following
--  libraries : ODBC32.LIB
--

with Ada.Text_IO;
with Ada.Exceptions;

with Databases.Select_Query;
with Databases.SQL;

procedure Demo4 is

   use Ada;
   use Ada.Exceptions;

   package SQL_Select renames Databases.Select_Query;

   -------------
   -- Display --
   -------------

   procedure Display (S : in String) is
   begin
     Text_IO.Put_Line (S);
     Text_IO.Flush;
   end Display;

   Clients : aliased Databases.Database;
   Query   : SQL_Select.Select_Data;

begin
   Display ("Open...");
   Databases.Connect (Clients, "Clients_DB", "test", "test");

   Display ("Execute Select...");

   SQL_Select.Execute
     (Clients,
      Databases.SQL.Build_Select (From => "clients"),
      Query);

   Display ("Get data...");

   Get_Data :
   declare
      Found : Boolean;
   begin
      loop
         Display ("Fetch...");
         SQL_Select.Fetch (Query, Found);
         exit when not Found;

         Display ("data :");

         for Column in 1 .. SQL_Select.Number_Of_Columns (Query) loop
            Display (SQL_Select.Get_Value (Query, Column));
         end loop;

         Display ("----------");
      end loop;
   end Get_Data;

   Display ("Close...");
   Databases.Close (Clients);

exception
   when E : others =>
      Display ("Probleme avec ODBC :-( ... : " &  Exception_Message (E));
      Databases.Close (Clients);
end Demo4;
