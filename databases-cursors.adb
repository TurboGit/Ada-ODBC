
--  ----------------------------------------------------------------------  --
--
--  Author  : Pascal Obry
--  E-Mail  : pascal.obry@der.edfgdf.fr
--
--  ----------------------------------------------------------------------  --
--
--  $Id$
--
--  ----------------------------------------------------------------------  --
--
--       Module Name : Databases-Cursors
--         File name : databases-cursors.adb
--
--       Created by  : Pascal Obry
--               on  : Wed Apr 17 12:16:32 1996
--
--  Last modified by : $Author$
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  ======================================================================  --
--

with Ada.Strings.Maps;
with Ada.Strings.Fixed;

package body Databases.Cursors is

   use Ada;

   Cursor_ID : Natural := 0;

   Space_To_0 : Strings.Maps.Character_Mapping
              := Strings.Maps.To_Mapping (" ", "0");

   procedure Create (Cursor : in out Databases.Cursor) is
      Cursor_String_ID : String := Natural'Image (Cursor_ID);
   begin
      Strings.Fixed.Translate (Cursor_String_ID, Space_To_0);
      Cursor := To_Unbounded_String ("DB_Cursor_" & Cursor_String_ID);
      Cursor_ID := Natural'Succ (Cursor_ID);
   end Create;

   function Get_Name (Cursor : in Databases.Cursor)
                      return String
   is
   begin
      return To_String (Cursor);
   end Get_Name;

   function For_Where_Clause (Cursor : in Databases.Cursor)
                              return String
   is
   begin
      return "CURRENT OF " & Get_Name (Cursor);
   end For_Where_Clause;

end Databases.Cursors;
