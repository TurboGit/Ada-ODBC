
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
--       Module Name : Databases-Transactions
--         File name : databases-transactions.adb
--
--       Created by  : Pascal Obry
--               on  : Wed Apr 17 11:49:09 1996
--
--  Last modified by : $Author$
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  ======================================================================  --
--

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
