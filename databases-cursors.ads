
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
--       Module Name : Databases-Cursors
--         File name : databases-cursors.ads
--
--       Created by  : Pascal Obry
--               on  : Wed Apr 17 11:40:04 1996
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
--     Genre    : Type de donnee abstrait
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

package Databases.Cursors is

   procedure Create (Cursor : in out Databases.Cursor);

   function Get_Name (Cursor : in Databases.Cursor)
                      return String;

   function For_Where_Clause (Cursor : in Databases.Cursor)
                              return String;

end Databases.Cursors;
