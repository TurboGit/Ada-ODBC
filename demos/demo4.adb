
--
-- Author : Pascal Obry
--
-- This is a sample program to show some ODBC features.
--
-- It compile fine under GNAT 3.04a. You must add the following
-- libraries : ODBC32.LIB
--

with Ada.Text_IO;
with Ada.Exceptions;

with Databases.Select_Query;
with Databases.SQL;

procedure Demo4 is

   use Ada;

   package SQL_Select renames Databases.Select_Query;

   procedure Display (S : in String) is
   begin
     Text_IO.Put_Line (S);
     Text_IO.Flush;
   end Display;

   Clients : aliased Databases.Database;
   Query   : SQL_Select.Select_Data;

begin

   Display ("Open...");
   Databases.Connect (Clients, "Clients_DB", "test", "test");

   Display ("Execute Select...");

   SQL_Select.Execute
     (Clients,
      Databases.SQL.Build_Select (From => "clients"),
      Query);

   Display ("Get data...");
   Get_Data :
   declare
      Found : Boolean;
   begin
      loop
         Display ("Fetch...");
         SQL_Select.Fetch (Query, Found);
         exit when not Found;

         Display ("data :");
         for Column in 1 .. SQL_Select.Number_Of_Columns (Query) loop
            Display (SQL_Select.Get_Value (Query, Column));
         end loop;
         Display ("----------");
      end loop;
   end Get_Data;

   Display ("Close...");
   Databases.Close (Clients);

exception

   when E : others =>
      Display ("Probleme avec ODBC :-( ... : " &
               Ada.Exceptions.Exception_Message (E));
      Databases.Close (Clients);

end Demo4;
