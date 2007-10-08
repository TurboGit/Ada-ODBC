------------------------------------------------------------------------------
--                                Database                                  --
--                                                                          --
--                        Copyright (C) 1999-2007                           --
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

package body Databases.Options is

   ---------
   -- Set --
   ---------

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


   ---------
   -- Set --
   ---------

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


   ---------
   -- Set --
   ---------

   procedure Set (Query      : in out Select_Statement;
                  For_Update : in     For_Update_Options)
   is
   begin
      Query.For_Update := For_Update;
   end Set;

end Databases.Options;

