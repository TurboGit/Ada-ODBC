
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
--       Module Name : Databases-Sql
--         File name : databases-sql.adb
--
--       Created by  : Pascal Obry
--               on  : Wed Apr 24 10:22:03 1996
--
--  Last modified by : $Author$
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  ======================================================================  --
--

package body Databases.SQL is

   function Build_Select (Fields     : in String  := Empty_String;
                          From       : in String;
                          Where      : in String  := Empty_String;
                          For_Update : in For_Update_Options := None)
                          return String
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

   function Build_Update (Table : in String;
                          Set   : in String;
                          Where : in String := Empty_String)
                          return String
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
