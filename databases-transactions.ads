
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
--       Module Name : Databases-Transactions
--         File name : databases-transactions.ads
--
--       Created by  : Pascal Obry
--               on  : Wed Apr 17 11:45:33 1996
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
--
--  Mots-cles
--
--  Caracterisation
--     Unite    : Paquetage
--     Genre    :
--     Liaisons : Unite fille
--
--  Disponibilite
--     Systemes de compilation
--        GNAT, Pentium Pro, Windows NT
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

package Databases.Transactions is

   procedure Auto_Commit (DB       : in Database;
                          Position : in Switch);

   procedure Commit   (DB : in Database);
   procedure Rollback (DB : in Database);

end Databases.Transactions;
