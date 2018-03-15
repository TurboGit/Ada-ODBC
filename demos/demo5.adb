------------------------------------------------------------------------------
--                                  Demo5                                   --
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
--  You must set up the "Clients_DB" ODBC string before running this demo.
--  See readme.txt for instructions.
--
--  Test des procedures

with Ada.Exceptions;
with Ada.Text_IO;

with Databases;

procedure Demo5 is

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

   Nom     : constant String := "Qwertsqdfsqdfqsdy";
   Prenom  : constant String := "Ruandsffsdgsfd";
   Age     : constant Integer := 78;

   Clients            : Databases.Database;
   Clients_Parameters : Databases.Parameter_Set (3);

begin
   Display ("Open...");
   Databases.Connect (Clients, "Clients_DB", "guest", "guest");

   Display ("Insert...");

   Databases.Parameter
     (Clients_Parameters,
      1,
      Databases.SQL_PARAM_INPUT,
      Nom (Nom'First)'Address,
      Nom'Length,
      Databases.SQL_CHAR);

   Databases.Parameter
     (Clients_Parameters,
      2,
      Databases.SQL_PARAM_INPUT,
      Prenom (Prenom'First)'Address,
      Prenom'Length,
      Databases.SQL_CHAR);

   Databases.Parameter
     (Clients_Parameters,
      3,
      Databases.SQL_PARAM_INPUT,
      Age'Address,
      4,
      Databases.SQL_INTEGER);

   Databases.Execute
     (Clients,
      "{call Nouveau_Client (?, ?, ?)}",
      Clients_Parameters);

   Display ("Close...");
   Databases.Close (Clients);

exception
   when E : others =>
      Display ("Probleme avec ODBC :-( ... : " & Exception_Message (E));
      Databases.Close (Clients);
end Demo5;
