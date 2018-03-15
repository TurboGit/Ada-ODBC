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
--
--     Ce module permet de generer des requetes SQL.
--
--     Build_Select (Fields, From, Where, For_Update)
--        retourne sous forme de chaine la requete SQL SELECT.
--        Fields     : les champs a retournes. Si Fields="" alors on retourne
--                     tous les champs "*".
--        From       : le nom de la table.
--        Where      : la clause Where.
--        For_Update : options For Update de la clause Select.
--                     (None, Yes, Yes_No_Wait).
--
--     Build_Update
--        retourne sous forme de chaine la requete SQL UPDATE.

package Databases.SQL is

   Empty_String : constant String := "";

   function Build_Select
     (Fields     : in String  := Empty_String;
      From       : in String;
      Where      : in String  := Empty_String;
      For_Update : in For_Update_Options := None) return String;

   function Build_Update
     (Table : in String;
      Set   : in String;
      Where : in String := Empty_String) return String;

end Databases.SQL;
