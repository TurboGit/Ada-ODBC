
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
--         File name : databases-options.ads
--
--       Created by  : Pascal Obry
--               on  : Wed Apr 17 11:51:07 1996
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
--     Modification des options du driver ODBC.
--
--  Mots-cles
--     ODBC, SQL, Options
--
--  Caracterisation
--     Unite    : Paquetage
--     Genre    :
--     Liaisons : Unite fille.
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
