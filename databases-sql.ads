
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
--       Module Name : Databases-Sql
--         File name : databases-sql.ads
--
--       Created by  : Pascal Obry
--               on  : Wed Apr 24 10:12:28 1996
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
--     Ce module permet de generer des requetes SQL.
--
--  Mots-cles
--     SQL Request
--
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

package Databases.SQL is

   Empty_String : constant String := "";

   function Build_Select (Fields     : in String  := Empty_String;
                          From       : in String;
                          Where      : in String  := Empty_String;
                          For_Update : in For_Update_Options := None)
                          return String;

   function Build_Update (Table : in String;
                          Set   : in String;
                          Where : in String := Empty_String)
                          return String;

end Databases.SQL;

