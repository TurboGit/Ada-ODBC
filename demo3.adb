
--
-- Author : Pascal Obry
--
-- This is a sample program to show some ODBC features.
--
-- It compile fine under GNAT 3.04a. You must add the following
-- libraries : ODBC32.LIB
--

--  tests du Commit et Rollback

with Ada.Text_IO;
with Ada.Exceptions;

with Databases;
with Databases.Transactions;

procedure Demo3 is

   use Ada;

   procedure Display (S : in String) is
   begin
      Text_IO.Put_Line (S);
      Text_IO.Flush;
   end Display;

   Clients : Databases.Database;

begin

   Display ("Open...");
   Databases.Connect (Clients, "Clients", "guest", "guest");

   Display ("Auto commit off...");
   Databases.Transactions.Auto_Commit (Clients, Databases.Off);

   Display ("Insert...");
   Databases.Execute (Clients,
                      "insert into clients " &
                      "values ('N1111', 'P1111', 10)");
   Databases.Execute (Clients,
                      "insert into clients " &
                      "values ('N2222', 'P2222', 20)");
   Databases.Execute (Clients,
                      "insert into clients " &
                      "values ('N3333', 'P3333', 30)");
   Databases.Execute (Clients,
                      "insert into clients " &
                      "values ('N4444', 'P4444', 40)");


   Display ("Rollback...");
   Databases.Transactions.Rollback (Clients);

   Display ("Insert...");
   Databases.Execute (Clients,
                      "insert into clients " &
                      "values ('Ochon', 'Paul', 8)");

   Display ("Commit...");
   Databases.Transactions.Commit (Clients);

   Display ("Close...");
   Databases.Close (Clients);

exception

   when E : others =>
      Display ("Probleme avec ODBC :-( ... : " &
               Ada.Exceptions.Exception_Message (E));
      Databases.Close (Clients);
end Demo3;
