-- You must set up the "Clients_DB" ODBC string before running this demo.
-- See readme.txt for instructions.

--  Test des procedures

with Ada.Exceptions;
with Databases;
with Ada.Text_IO;

procedure Demo5 is

   use Ada;

   procedure Display (S : in String) is
   begin
     Text_IO.Put_Line (S);
     Text_IO.Flush;
   end Display;

   Clients : Databases.Database;

   Nom    : String := "Qwertsqdfsqdfqsdy";
   Prenom : String := "Ruandsffsdgsfd";
   Age    : Integer := 78;

   Clients_Parameters : Databases.Parameter_Set (3);

begin

   Display ("Open...");
   Databases.Connect (Clients, "Clients_DB", "guest", "guest");

   Display ("Insert...");
   Databases.Parameter (Clients_Parameters,
                        1,
                        Databases.SQL_PARAM_INPUT,
                        Nom (Nom'First)'Address,
                        Nom'Length,
                        Databases.SQL_CHAR);
   Databases.Parameter (Clients_Parameters,
                        2,
                        Databases.SQL_PARAM_INPUT,
                        Prenom (Prenom'First)'Address,
                        Prenom'Length,
                        Databases.SQL_CHAR);
   Databases.Parameter (Clients_Parameters,
                        3,
                        Databases.SQL_PARAM_INPUT,
                        Age'Address,
                        4,
                        Databases.SQL_INTEGER);
   Databases.Execute (Clients,
                      "{call Nouveau_Client (?, ?, ?)}",
                      Clients_Parameters);

   Display ("Close...");
   Databases.Close (Clients);

exception

   when E : others =>
      Display ("Probleme avec ODBC :-( ... : " &
               Ada.Exceptions.Exception_Message (E));
      Databases.Close (Clients);

end Demo5;
