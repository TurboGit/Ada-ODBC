------------------------------------------------------------------------------
--                                 Database                                 --
--                                                                          --
--                         Copyright (C) 1999-2018                          --
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

package body Databases.SQL is

   ------------------
   -- Build_Select --
   ------------------

   function Build_Select
     (Fields     : in String  := Empty_String;
      From       : in String;
      Where      : in String  := Empty_String;
      For_Update : in For_Update_Options := None) return String
   is
      Statement : Unbounded_String;
   begin
      Statement := To_Unbounded_String ("SELECT ");

      if Fields = Empty_String then
         Append (Statement, "*");
      else
         Append (Statement, Fields);
      end if;

      Append (Statement, " FROM ");
      Append (Statement, From);

      if Where /= Empty_String then
         Append (Statement, " WHERE ");
         Append (Statement, Where);
      end if;

      case For_Update is
         when None =>
            null;
         when Yes =>
            Append (Statement, " FOR UPDATE");
         when Yes_No_Wait =>
            Append (Statement, " FOR UPDATE NOWAIT");
      end case;

      return To_String (Statement);
   end Build_Select;

   ------------------
   -- Build_Update --
   ------------------

   function Build_Update
     (Table : in String;
      Set   : in String;
      Where : in String := Empty_String) return String
   is
      Statement : Unbounded_String;
   begin
      Statement := To_Unbounded_String ("UPDATE ");
      Append (Statement, Table);

      Append (Statement, " SET ");
      Append (Statement, Set);

      if Where /= Empty_String then
         Append (Statement, " WHERE ");
         Append (Statement, Where);
      end if;

      return To_String (Statement);
   end Build_Update;

end Databases.SQL;
