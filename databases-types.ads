
--  ----------------------------------------------------------------------  --
--
--                Copyright (C) 1996 Electricite De France
--                   Direction des Etudes et Recherches
--
--  Author  : Pascal Obry
--  Address : EDF/DER 1, av du General de Gaulle 92141 Clamart CEDEX
--  E-Mail  : pascal.obry@der.edfgdf.fr
--
--  ----------------------------------------------------------------------  --
--
--  $Id$
--
--  ----------------------------------------------------------------------  --
--
--       Module Name : Databases-Types
--         File name : databases-types.ads
--
--       Created by  : Pascal Obry
--               on  : Tue May 14 15:45:00 1996
--
--  Last modified by : $Author$
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  ======================================= I D E N T I F I C A T I O N ==  --
--
--  Description
--     Description des relations entre types C et ODBC-SQL.
--
--  Mots-cles
--     ODBC, SQL, Types de donnees
--
--  Caracterisation
--     Unite    : Paquetage
--     Genre    :
--     Liaisons : Unite fille
--
--  Disponibilite
--     Systemes de compilation
--        GNAT, Pentium Pro, Windows-NT
--     Access
--        Sources
--
--  Historique
--
--  ======================================= S P E C I F I C A T I O N S ==  --
--
--  Elements generiques et ajustement de comportement
--     (Unite non generique)
--
--  Elements principaux
--
--     SQL_To_C
--        Table de relations entre donnees ODBC-SQL et C.
--
--  Elements annexes
--
--  ===================================== I M P L E M E N T A T I O N S ==  --
--
--  Elaboration
--     (neant - pas de pragma d'elaboration necessaire)
--
--  Algorithme
--     (neant)
--
--  Elements sensibles utilises
--     (neant)
--
--  Performances
--     (neant)
--
--  Autres informations
--     (neant)
--
--  ======================================================================  --
--

private package Databases.Types is

   type Data_Values is
      record
         C_Value   : ODBC.SWORD;
         SQL_Value : ODBC.SWORD;
      end record;

   SQL_To_C : array (Data_Type) of Data_Values :=
     (SQL_CHAR     => (Win32.SQL.SQL_C_CHAR,      Win32.SQL.SQL_CHAR),
      SQL_VARCHAR  => (Win32.SQL.SQL_C_CHAR,      Win32.SQL.SQL_VARCHAR),
      SQL_NUMERIC  => (Win32.SQL.SQL_C_CHAR,      Win32.SQL.SQL_NUMERIC),
      SQL_DECIMAL  => (Win32.SQL.SQL_C_CHAR,      Win32.SQL.SQL_DECIMAL),
      SQL_INTEGER  => (Win32.SQL.SQL_C_LONG,   Win32.SQL.SQL_INTEGER),
      SQL_SMALLINT => (Win32.SQL.SQL_C_SHORT,  Win32.SQL.SQL_SMALLINT),
--       SQL_INTEGER  => (Win32.SQLEXT.SQL_C_SLONG,  Win32.SQL.SQL_INTEGER),
--       SQL_SMALLINT => (Win32.SQLEXT.SQL_C_SSHORT, Win32.SQL.SQL_SMALLINT),
      SQL_FLOAT    => (Win32.SQL.SQL_C_DOUBLE,    Win32.SQL.SQL_FLOAT),
      SQL_DOUBLE   => (Win32.SQL.SQL_C_DOUBLE,    Win32.SQL.SQL_DOUBLE),
      SQL_REAL     => (Win32.SQL.SQL_C_FLOAT,     Win32.SQL.SQL_REAL));

end Databases.Types;
