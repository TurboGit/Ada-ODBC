------------------------------------------------------------------------------
--                               Test_Select                                --
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

with Ada.Text_IO;
with Ada.Exceptions;

with Databases.Select_Query;

procedure Test_Select is

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

   Clients : Databases.Database;
   Query   : SQL_Select.Select_Data;

   Driver  : constant String:= "PM_test 909";

begin
   Display ("Open (connect) driver [" & Driver & "]...");
   Databases.Connect (Clients, Driver, "", "");

   Display ("Execute Select...");

   SQL_Select.Execute
     (Clients,
      "select * from history_run", -- fx_rate_id, type
      --  "select * from bo where bomonth='2007-07-10'",
      --  Databases.SQL.Build_Select (From => "clients"),
      Query);

   Display ("Get data...");

   Get_Data :
   declare
      Found : Boolean;
      Row   : Natural:= 0;
   begin
      loop
         Display ("Fetch...");
         SQL_Select.Fetch (Query, Found);
         exit when not Found;

         Row := Row + 1;
         Display ("  data for row:" & Positive'Image (Row));

         for Column in 1 .. SQL_Select.Number_Of_Columns (Query) loop
            Display
              ("    "
               & SQL_Select.Get_Name (Query, Column) & ": "
               & SQL_Select.Get_Value (Query, Column));
         end loop;

         Display ("----------");
      end loop;

      Display ("Total rows:"& Positive'Image(Row));
   end Get_Data;

   Display ("Close...");
   Databases.Close (Clients);

   Text_IO.Put("[Press enter]");
   Text_IO.Skip_Line;

exception
   when E : others =>
      Display ("Problem with ODBC :-( ... : " & Exception_Message (E));
      Databases.Close (Clients);
      Text_IO.Put("[Press enter]");
      Text_IO.Skip_Line;
end Test_Select;
