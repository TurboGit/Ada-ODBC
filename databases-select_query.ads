
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
--       Module Name : Databases-Select_Query
--         File name : databases-select_query.ads
--
--       Created by  : Pascal Obry
--               on  : Fri Apr 26 11:02:29 1996
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
--     Gestion des requetes SQL Select. Cette abstraction est de plus haut
--     niveau que celle dans Databases. Ici, il n'y a pas de notion de
--     "column binding", c'est le systeme qui se charge de cette
--     tache. Chaque requete comporte un Context qui permet de recuperer
--     par des Fetch successifs l'ensemble des reponses.
--
--     Les donnees des colonnes sont retournees sous forme de chaine de
--     caracteres.
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
--     Execute
--        execution d'une commande SQL Select. Il est possible d'executer
--        une autre commande SQL qu'un Select mais dans ce cas il est
--        recommande d'utiliser Databases.Execute.
--
--     Fetch
--        recuperation de la ligne suivante.
--
--     Number_Of_Columns
--        donne le nombre de colonnes dans le lot resultat.
--
--     Get_Value
--        recupere la valeur d'une colonne de la ligne resultat.
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

package Databases.Select_Query is

   type Select_Datas is private;

   procedure Execute (DB         : in     Database;
                      Statement  : in     String;
                      Context    :    out Select_Datas;
                      Parameters : in     Parameter_Set    := No_Parameter;
                      Cursor     : in     Databases.Cursor := No_Cursor);

   procedure Fetch (Context : in     Select_Datas;
                    Found   :    out Boolean);

   function Number_Of_Columns (Context : in Select_Datas)
                               return Positive;

   function Get_Value (Context : in Select_Datas;
                       Column  : in Positive)
                       return String;

private

   use Ada.Strings.Unbounded;

   type Column_Datas is
      record
         Name  : Unbounded_String;
         Value : Unbounded_String;
         Model : aliased ODBC.SWORD;
      end record;
   type Columns_Datas is array (Positive range <>) of Column_Datas;
   type Columns_Datas_Access is access Columns_Datas;

   type Select_Datas is
      record
         Base                 : Database;
         DBC_Statement_Handle : aliased ODBC.HSTMT;
         Columns              : Columns_Datas_Access;
         SQL                  : String_Access;
      end record;

end Databases.Select_Query;

