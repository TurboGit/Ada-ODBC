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
--
--     Auto_Commit (DB, ON/OFF)
--        change le mode de transaction.
--        (Auto Commit est ON par defaut en ODBC)
--        => SQL_Error
--
--     Commit
--        effectue un commit des operations effectuees sur la base.
--        => SQL_Error
--
--     Rollback
--        effectue un rollback des operations effectuees sur la base.
--        => SQL_Error

package Databases.Transactions is

   procedure Auto_Commit (DB       : in Database;
                          Position : in Switch);

   procedure Commit   (DB : in Database);
   procedure Rollback (DB : in Database);

end Databases.Transactions;
