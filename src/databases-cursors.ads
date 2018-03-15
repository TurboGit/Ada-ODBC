------------------------------------------------------------------------------
--                            Databases.Cursors                             --
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
--
--     *** Constructors
--
--     Create
--        cree un cursor.
--
--     *** Accessors
--
--     Get_Name
--        retourne le nom du cursor.
--
--     For_Where_Clause
--        retourne une chaine de caracteres representant le cursor, cette
--        chaine est a utiliser dans une clause where d'une requete SQL.
--        La chaine est de la forme : "CURRENT OF <nom cursor>"

package Databases.Cursors is

   procedure Create (Cursor : in out Databases.Cursor);

   function Get_Name (Cursor : in Databases.Cursor) return String;

   function For_Where_Clause (Cursor : in Databases.Cursor) return String;

end Databases.Cursors;
