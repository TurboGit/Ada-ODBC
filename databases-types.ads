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
--     SQL_To_C
--        Table de relations entre donnees ODBC-SQL et C.

private package Databases.Types is

   type Data_Values is
      record
         C_Value   : ODBC.SWORD;
         SQL_Value : ODBC.SWORD;
      end record;

   SQL_To_C : array (Data_Type) of Data_Values :=
     (SQL_CHAR     => (Win32.SQL.SQL_C_CHAR,      Win32.SQL.SQL_CHAR),
      SQL_VARCHAR  => (Win32.SQL.SQL_C_CHAR,      Win32.SQL.SQL_VARCHAR),
      SQL_DATE     => (Win32.SQLEXT.SQL_C_DATE,   Win32.SQLEXT.SQL_DATE),
      SQL_NUMERIC  => (Win32.SQL.SQL_C_CHAR,      Win32.SQL.SQL_NUMERIC),
      SQL_DECIMAL  => (Win32.SQL.SQL_C_CHAR,      Win32.SQL.SQL_DECIMAL),
      SQL_INTEGER  => (Win32.SQL.SQL_C_LONG,      Win32.SQL.SQL_INTEGER),
      SQL_SMALLINT => (Win32.SQL.SQL_C_SHORT,     Win32.SQL.SQL_SMALLINT),
      SQL_FLOAT    => (Win32.SQL.SQL_C_DOUBLE,    Win32.SQL.SQL_FLOAT),
      SQL_DOUBLE   => (Win32.SQL.SQL_C_DOUBLE,    Win32.SQL.SQL_DOUBLE),
      SQL_REAL     => (Win32.SQL.SQL_C_FLOAT,     Win32.SQL.SQL_REAL));

end Databases.Types;
