------------------------------------------------------------------------------
--                                  Demo3                                   --
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
--
--  tests du Commit et Rollback

with Ada.Text_IO;
with Ada.Exceptions;

with Databases;
with Databases.Transactions;

procedure Demo3 is

   use Ada;
   use Ada.Exceptions;

   -------------
   -- Display --
   -------------

   procedure Display (S : in String) is
   begin
      Text_IO.Put_Line (S);
      Text_IO.Flush;
   end Display;

   Clients : Databases.Database;

begin
   Display ("Open...");

   Databases.Connect (Clients, "Clients_DB", "guest", "guest");

   Display ("Auto commit off...");

   Databases.Transactions.Auto_Commit (Clients, Databases.Off);

   Display ("Insert...");

   Databases.Execute
     (Clients, "insert into clients values ('N1111', 'P1111', 10)");

   Databases.Execute
     (Clients, "insert into clients values ('N2222', 'P2222', 20)");

   Databases.Execute
     (Clients, "insert into clients values ('N3333', 'P3333', 30)");

   Databases.Execute
     (Clients, "insert into clients values ('N4444', 'P4444', 40)");

   Display ("Rollback...");
   Databases.Transactions.Rollback (Clients);

   Display ("Insert...");

   Databases.Execute
     (Clients, "insert into clients values ('Ochon', 'Paul', 8)");

   Display ("Commit...");
   Databases.Transactions.Commit (Clients);

   Display ("Close...");
   Databases.Close (Clients);

exception

   when E : others =>
      Display ("Probleme avec ODBC :-( ... : " & Exception_Message (E));
      Databases.Close (Clients);
end Demo3;
