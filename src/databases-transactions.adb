------------------------------------------------------------------------------
--                                Database                                  --
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

with Databases.Options;

package body Databases.Transactions is

   -----------------
   -- Auto_Commit --
   -----------------

   procedure Auto_Commit (DB       : in Database;
                          Position : in Switch) is
   begin
      if Position = On then
         Options.Set (DB,
                      ODBC_EXT.SQL_AUTOCOMMIT,
                      ODBC_EXT.SQL_AUTOCOMMIT_ON);
      else
         Options.Set (DB,
                      ODBC_EXT.SQL_AUTOCOMMIT,
                      ODBC_EXT.SQL_AUTOCOMMIT_OFF);
      end if;
   end Auto_Commit;

   ------------
   -- Commit --
   ------------

   procedure Commit (DB : in Database) is
      RC : ODBC.RETCODE;
   begin
      RC := ODBC.SQLTransact (DB.DBC_Environment_Handle,
                              DB.DBC_Handle,
                              ODBC.SQL_COMMIT);
      Check_SQL_Error (DB, RC, Procedure_Name => "Commit");
   end Commit;

   --------------
   -- Rollback --
   --------------

   procedure Rollback (DB : in Database) is
      RC : ODBC.RETCODE;
   begin
      RC := ODBC.SQLTransact (DB.DBC_Environment_Handle,
                              DB.DBC_Handle,
                              ODBC.SQL_ROLLBACK);
      Check_SQL_Error (DB, RC, Procedure_Name => "Rollback");
   end Rollback;

end Databases.Transactions;
