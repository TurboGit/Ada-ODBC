
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

   Nom    : String (1 .. 50);
   Prenom : String (1 .. 50);
   Age    : Positive;

begin

   Display ("Open...");
   Databases.Connect (Clients, "Clients_DB", "test", "test");

   Display ("Bind columns directly to Ada variables...");

   Databases.Bind (Query, 1, "nom",
                   Nom (Nom'First)'Address, Nom'Length,
                   Databases.SQL_CHAR);
   Databases.Bind (Query, 2, "prenom",
                   Prenom (Prenom'First)'Address, Prenom'Length,
                   Databases.SQL_CHAR);
   Databases.Bind (Query, 3, "age",
                   Age'Address, 0, Databases.SQL_INTEGER);

   Display ("Select...");
   -- This is equivalent to: select * from Clients where Nom = "Obry"
   Databases.Query (Query, 1, Databases.Equal, "Obry");
   Databases.SQL_Select (Clients, Query, Table => "clients");

   Display ("Getting the data...");
   loop
      declare
        Found : Boolean;
      begin
         Display ("Fetching one data row, if any...");
         Databases.Fetch (Query, Found);
         exit when not Found;

         Display ("data :");
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
