------------------------------------------------------------------------------
--                                Database                                  --
--                                                                          --
--                        Copyright (C) 19999-2007                           --
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

with Ada.Strings.Maps;
with Ada.Strings.Fixed;

package body Databases.Cursors is

   use Ada;

   Cursor_ID : Natural := 0;

   Space_To_0 : Strings.Maps.Character_Mapping
              := Strings.Maps.To_Mapping (" ", "0");

   ------------
   -- Create --
   ------------

   procedure Create (Cursor : in out Databases.Cursor) is
      Cursor_String_ID : String := Natural'Image (Cursor_ID);
   begin
      Strings.Fixed.Translate (Cursor_String_ID, Space_To_0);
      Cursor := To_Unbounded_String ("DB_Cursor_" & Cursor_String_ID);
      Cursor_ID := Natural'Succ (Cursor_ID);
   end Create;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Cursor : in Databases.Cursor)
                      return String
   is
   begin
      return To_String (Cursor);
   end Get_Name;

   ----------------------
   -- For_Where_Clause --
   ----------------------

   function For_Where_Clause (Cursor : in Databases.Cursor)
                              return String
   is
   begin
      return "CURRENT OF " & Get_Name (Cursor);
   end For_Where_Clause;

end Databases.Cursors;
