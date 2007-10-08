-- Modified from demo4

with Ada.Text_IO;
with Ada.Exceptions;

with Databases.Select_Query;
with Databases.SQL;

procedure Test_Select is

   package SQL_Select renames Databases.Select_Query;

   procedure Display (S : in String) is
   begin
     Ada.Text_IO.Put_Line (S);
     Ada.Text_IO.Flush;
   end Display;

   Clients : Databases.Database;
   Query   : SQL_Select.Select_Datas;

   Driver  : constant String:= "PM_test 678";

begin

   Display ("Open (connect) driver [" & driver & "]...");
   Databases.Connect (Clients, driver, "", "");

   Display ("Execute Select...");

   SQL_Select.Execute
     (Clients,
      "select * from history_run", -- fx_rate_id, type
      -- "select * from bo where bomonth='2007-07-10'",
      -- Databases.SQL.Build_Select (From => "clients"),
      Query);

   Display ("Get data...");
   Get_Data :
   declare
      Found : Boolean;
      Row : Natural:= 0;
   begin
      loop
         Display ("Fetch...");
         SQL_Select.Fetch (Query, Found);
         exit when not Found;

         Row := Row + 1;
         Display ("  data for row:" & Positive'Image(Row));
         for Column in 1 .. SQL_Select.Number_Of_Columns (Query) loop
            Display (
              "    " &
              SQL_Select.Get_Name (Query, Column) & ": " &
              SQL_Select.Get_Value (Query, Column)
            );
         end loop;
         Display ("----------");
      end loop;
      Display ("Total rows:"& Positive'Image(Row));
   end Get_Data;

   Display ("Close...");
   Databases.Close (Clients);

   Ada.Text_IO.Put("[Press enter]");
   Ada.Text_IO.Skip_Line;

exception

   when E : others =>
      Display ("Problem with ODBC :-( ... : " &
               Ada.Exceptions.Exception_Message (E));
      Databases.Close (Clients);
      Ada.Text_IO.Put("[Press enter]");
      Ada.Text_IO.Skip_Line;

end Test_select;
