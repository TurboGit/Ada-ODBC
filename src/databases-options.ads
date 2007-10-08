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
--     Set (DB, Option, Value)
--        modifie les options pour DB. (SQLSetConnectOption)
--        => SQL_Error
--
--     Set (Query, Option, Value)
--        modifie les options pour Query. (SQLSetStmtOption)
--        => SQL_Error
--
--     Set (Query, For_Update, No_Wait)
--        modifie les options du Select.
--        For_Update permet de mettre un verrou sur les donnees recuperees
--        ce verrou est leve lors de la fin de transaction lors d'un Commit
--        ou d'un Rollback.
--        No_Wait indique qu'il ne faut pas etre bloque au cas ou les
--        donnees sont deja verrouillees.

package Databases.Options is

   --  Connection options

   procedure Set (DB     : in Database;
                  Option : in Natural;
                  Value  : in Natural);


   --  Query options

   procedure Set (Query  : in Select_Statement;
                  Option : in Natural;
                  Value  : in Natural);

   procedure Set (Query      : in out Select_Statement;
                  For_Update : in     For_Update_Options);

end Databases.Options;
