
--
-- Author : Pascal Obry
--
-- This is a sample program to show some ODBC features.
--
-- It compile fine under GNAT 3.04a. You must add the following
-- libraries : ODBC32.LIB
--

with Ada.Exceptions;
with Databases;
with Ada.Text_IO;

procedure Demo is

   use Ada;

   procedure Display (S : in String) is
   begin
     Text_IO.Put_Line (S);
     Text_IO.Flush;
   end Display;

   Clients : aliased Databases.Database;
   Query   : Databases.Select_Statement (3);

   Nom    : String (1 .. 20);
   Prenom : String (1 .. 20);
   Age    : Positive;

begin

   Display ("Open...");
   Databases.Connect (Clients, "Clients", "test", "test");

   Display ("Bind columns...");

   Databases.Bind (Query, 1, "nom",
                   Nom (Nom'First)'Address, Nom'Length,
                   Databases.SQL_CHAR);
   Databases.Bind (Query, 2, "prenom",
                   Prenom (Prenom'First)'Address, Prenom'Length,
                   Databases.SQL_CHAR);
   Databases.Bind (Query, 3, "age",
                   Age'Address, 0, Databases.SQL_INTEGER);
   Databases.Query (Query, 1, Databases.Equal, "Obry");

   Display ("Select...");
   Databases.SQL_Select (Clients, Query, Table => "clients");

   Display ("Get datas...");
   loop
      declare
        Found : Boolean;
      begin
         Display ("Fetch...");
         Databases.Fetch (Query, Found);
         exit when not Found;

         Display ("datas :");
         Display (Nom (1 .. Databases.Last (Query, 1)));
         Display (Prenom (1 .. Databases.Last (Query, 2)));
         Display (Positive'Image (Age));
         Display ("----------");
      end;
   end loop;

   Display ("Insert...");
   Databases.Execute (Clients,
                      "insert into clients " &
                      "values ('dupont', 'toto', 29)");

   Display ("Close...");
   Databases.Close (Clients);

exception

   when E : others =>
      Display ("Problem with ODBC ... : " &
               Ada.Exceptions.Exception_Message (E));
      Databases.Close (Clients);
end Demo;
