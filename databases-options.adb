
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
--       Module Name : Databases-Options
--         File name : databases-options.adb
--
--       Created by  : Pascal Obry
--               on  : Wed Apr 17 11:53:15 1996
--
--  Last modified by : $Author$
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  ======================================================================  --
--

package body Databases.Options is

   procedure Set (Query  : in Select_Statement;
                  Option : in Natural;
                  Value  : in Natural)
   is
      RC : ODBC.RETCODE;
   begin
      RC := ODBC_EXT.SQLSetStmtOption (Query.DBC_Statement_Handle,
                                       Win32.SQL.UWORD (Option),
                                       Win32.SQL.UDWORD (Value));
      Check_SQL_Error (Query.Base, RC,
                       Procedure_Name   => "Set (Query)",
                       Statement_Handle => Query.DBC_Statement_Handle);
   end Set;

   -------------------------------------------------------------------------

   procedure Set (DB     : in Database;
                  Option : in Natural;
                  Value  : in Natural)
   is
      RC : ODBC.RETCODE;
   begin
      RC := ODBC_EXT.SQLSetConnectOption (DB.DBC_Handle,
                                          Win32.SQL.UWORD (Option),
                                          Win32.SQL.UDWORD (Value));
      Check_SQL_Error (DB, RC,
                       Procedure_Name   => "Set (DB)");
   end Set;

   -------------------------------------------------------------------------

   procedure Set (Query      : in out Select_Statement;
                  For_Update : in     For_Update_Options)
   is
   begin
      Query.For_Update := For_Update;
   end Set;

end Databases.Options;

