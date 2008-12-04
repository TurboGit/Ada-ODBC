------------------------------------------------------------------------------
--                                Database                                  --
--                                                                          --
--                        Copyright (C) 1999-2007                           --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with Databases.Cursors;
with Databases.Types;

package body Databases.Select_Query is

   Internal_Error_Data_Type_Not_Yet_Implemented : exception;

   function To_PUCHAR is new Ada.Unchecked_Conversion (System.Address,
                                                       Win32.PUCHAR);

   -------------
   -- Execute --
   -------------

   procedure Execute (DB         : in      Database;
                      Statement  : in      String;
                      Context    :    out  Select_Datas;
                      Parameters : in      Parameter_Set    := No_Parameter;
                      Cursor     : in      Databases.Cursor := No_Cursor)
   is
      RC  : ODBC.RETCODE;
   begin --  Execute

      Context.Base := DB;

      --  allocate statement block
      RC := ODBC.SQLAllocStmt (DB.DBC_Handle,
                               Context.DBC_Statement_Handle'Access);
      Check_SQL_Error (DB, RC,
                       Procedure_Name   => "Execute",
                       Error_Message    => "Allocation Statement",
                       Statement_Handle => Context.DBC_Statement_Handle);

      --  set the cursor if needed
      if Cursor /= No_Cursor then
         declare
            Cursor_Name : constant String
                        := Databases.Cursors.Get_Name (Cursor);
         begin
            RC := ODBC.SQLSetCursorName
              (Context.DBC_Statement_Handle,
               To_PUCHAR (Cursor_Name (Cursor_Name'First)'Address),
               Cursor_Name'Length);
            Check_SQL_Error
              (DB, RC,
               Procedure_Name   => "Execute",
               Error_Message    => "Cursor creation",
               Statement_Handle => Context.DBC_Statement_Handle);
         end;
      end if;

      --  prepare query
      Context.SQL := new String'(Statement);

      if Parameters = No_Parameter then
         RC := ODBC.SQLExecDirect
           (Context.DBC_Statement_Handle,
            To_PUCHAR (Context.SQL (Context.SQL'First)'Address),
            Context.SQL'Length);
         Check_SQL_Error (DB, RC,
                          Procedure_Name   => "Execute",
                          Error_Message    => "Execute query",
                          Statement_Handle => Context.DBC_Statement_Handle);
      else
         RC := ODBC.SQLPrepare
           (Context.DBC_Statement_Handle,
            To_PUCHAR (Context.SQL (Context.SQL'First)'Address),
            Context.SQL'Length);
         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Preparation of SQL statement : " &
                                Context.SQL.all,
            Statement_Handle => Context.DBC_Statement_Handle);

         for Parameter_Number in 1 .. Parameters.N loop
            declare
               Parameter : Parameter_Datas
                 := Parameters.Parameters (Parameter_Number);
            begin
               RC := ODBC_EXT.SQLBindParameter
                 (Context.DBC_Statement_Handle,
                  ODBC.UWORD (Parameter_Number),
                  Parameter.Mode,
                  Types.SQL_To_C (Parameter.Data_Model).C_Value,
                  Types.SQL_To_C (Parameter.Data_Model).SQL_Value,
                  ODBC.UDWORD (Parameter.Size),
                  0,
                  Parameter.Address,
                  Parameter.Size,
                  Parameter.Last'Access);
               Check_SQL_Error
                 (DB, RC,
                  Procedure_Name   => "Execute",
                  Error_Message    => "Parameters of SQL statement : " &
                                      Context.SQL.all,
                  Statement_Handle => Context.DBC_Statement_Handle);
            end;
         end loop;

         RC := ODBC.SQLExecute (Context.DBC_Statement_Handle);
         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Execution of SQL statement : " &
                                 Context.SQL.all,
            Statement_Handle => Context.DBC_Statement_Handle);
      end if;


      Build_Context_Datas :
      declare
         Max_Name_Length : constant := 200;
         Number_Columns  : aliased ODBC.SWORD;
         Column_Name     : String (1 .. Max_Name_Length);
         Name_Length     : aliased ODBC.SWORD;
         Precision       : aliased ODBC.SDWORD;
         Scale, Nullable : aliased ODBC.SWORD;
      begin
         RC := ODBC.SQLNumResultCols (Context.DBC_Statement_Handle,
                                      Number_Columns'Access);
         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Get number of columns",
            Statement_Handle => Context.DBC_Statement_Handle);

         Context.Columns :=
          new Columns_Datas (1 .. Positive (Number_Columns));


         for Column in 1 .. Positive (Number_Columns) loop
            RC := ODBC.SQLDescribeCol
              (Context.DBC_Statement_Handle,
               ODBC.UWORD (Column),
               To_PUCHAR (Column_Name (Column_Name'First)'Address),
               Max_Name_Length,
               Name_Length'Access,
               Context.Columns (Column).Model'Access,
               Precision'Access,
               Scale'Access,
               Nullable'Access);

            Context.Columns (Column).Name := To_Unbounded_String
              (Column_Name (Column_Name'First .. Natural (Name_Length)));
         end loop;

      end Build_Context_Datas;

   end Execute;

   -----------
   -- Fetch --
   -----------

   procedure Fetch (Context : in     Select_Datas;
                    Found   :    out Boolean)
   is
      use type ODBC.RETCODE;
      RC : ODBC.RETCODE;

      procedure Get_Columns_Datas is

         Max_Data_Length : constant := 10_000;

         Tmp_String    : String (1 .. Max_Data_Length);
         Tmp_Integer   : Win32.Int;
         Tmp_Double    : Win32.Double;
         Tmp_Date      : ODBC_EXT.DATE_STRUCT;
         Tmp_Time      : ODBC_EXT.TIME_STRUCT;
         Tmp_TimeStamp : ODBC_EXT.TIMESTAMP_STRUCT;
         Data_Length   : aliased ODBC.SDWORD;

         function Date_Image
           (year : in ODBC.SWORD; month, day : in ODBC.UWORD) return String
         is
           use ODBC;
           -- + 100: trick for obtaining 0x
           sY : constant String:= SWORD'Image (year);
           sM : constant String:= UWORD'Image (month + 100);
           sD : constant String:= UWORD'Image (day + 100);
         begin
            return
               sY( sY'Last-3 .. sY'Last ) & '-' &
               sM( sM'Last-1 .. sM'Last ) & '-' &
               sD( sD'Last-1 .. sD'Last );
         end Date_image;

         function Time_Image
           (hour, minute, second : in ODBC.UWORD) return String
         is
           use ODBC;
           -- + 100: trick for obtaining 0x
           shr : constant String:= UWORD'Image (hour + 100);
           smn : constant String:= UWORD'Image (minute + 100);
           ssc : constant String:= UWORD'Image (second + 100);
         begin
            return
               shr (shr'Last - 1 .. shr'Last) & ':' &
               smn (smn'Last - 1 .. smn'Last) & ':' &
               ssc (ssc'Last - 1 .. ssc'Last);
         end Time_image;

         function Millisecond_Image
           (fraction : in ODBC.UDWORD) return String
         is
           use ODBC;
           -- + 1000: trick for obtaining 0x
           sfr : constant String := UDWORD'Image( fraction + 1000);
         begin
            return sfr (sfr'Last - 2 .. sfr'Last);
         end Millisecond_image;

         procedure Check_Error is
         begin
            Check_SQL_Error
              (Context.Base, RC,
               Procedure_Name   => "Get_Columns_Datas",
               Error_Message    => "Error on SQLGetData",
               Statement_Handle => Context.DBC_Statement_Handle);
         end Check_Error;

      begin
         for Column in Context.Columns'Range loop

            case Context.Columns (Column).Model is

               when ODBC.SQL_FLOAT =>
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_FLOAT).C_Value,
                     Tmp_Double'Address,
                     0,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    Trim (To_Unbounded_String
                            (Float'Image (Float (Tmp_Double))),
                          Ada.Strings.Left);

               when ODBC.SQL_INTEGER =>
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_INTEGER).C_Value,
                     Tmp_Integer'Address,
                     0,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    Trim (To_Unbounded_String
                            (Integer'Image (Integer (Tmp_Integer))),
                          Ada.Strings.Left);

               when ODBC.SQL_NUMERIC =>
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_NUMERIC).C_Value,
                     Tmp_Integer'Address,
                     0,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    Trim (To_Unbounded_String
                            (Integer'Image (Integer (Tmp_Integer))),
                          Ada.Strings.Left);

               when ODBC.SQL_SMALLINT =>
                  Tmp_Integer := 0;
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_SMALLINT).C_Value,
                     Tmp_Integer'Address,
                     0,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    Trim (To_Unbounded_String
                            (Integer'Image (Integer (Tmp_Integer))),
                          Ada.Strings.Left);

               when ODBC.SQL_CHAR =>
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_CHAR).C_Value,
                     Tmp_String'Address,
                     Max_Data_Length,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    To_Unbounded_String
                       (Tmp_String
                           (Tmp_String'First .. Integer (Data_Length)));

               when ODBC.SQL_VARCHAR =>
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_CHAR).C_Value,
                     Tmp_String'Address,
                     Max_Data_Length,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    To_Unbounded_String
                       (Tmp_String
                           (Tmp_String'First .. Integer (Data_Length)));

               when ODBC_EXT.SQL_DATE =>
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_DATE).C_Value,
                     Tmp_Date'Address,
                     0,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    To_Unbounded_String
                      (Date_image(Tmp_date.year,Tmp_date.month,Tmp_date.day));

               when ODBC_EXT.SQL_TIME =>
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_TIME).C_Value,
                     Tmp_Time'Address,
                     0,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    To_Unbounded_String
                      (Time_Image
                        (Tmp_time.hour, Tmp_time.minute, Tmp_time.second));

               when ODBC_EXT.SQL_TIMESTAMP =>
                  RC := ODBC_EXT.SQLGetData
                    (Context.DBC_Statement_Handle,
                     ODBC.UWORD (Column),
                     Types.SQL_To_C (SQL_TIMESTAMP).C_Value,
                     Tmp_Timestamp'Address,
                     0,
                     Data_Length'Access);
                  Check_Error;
                  Context.Columns (Column).Value :=
                    To_Unbounded_String
                      (Date_Image(
                          Tmp_timestamp.year,
                          Tmp_timestamp.month,
                          Tmp_timestamp.day)
                       & ' ' &
                       Time_image(
                          Tmp_timestamp.hour,
                          Tmp_timestamp.minute,
                          Tmp_timestamp.second)
                       & '.' &
                       Millisecond_image(Tmp_timestamp.fraction)
                      );

               when others =>
                  Ada.Exceptions.Raise_Exception
                    (Internal_Error_Data_Type_Not_Yet_Implemented'Identity,
                     Message =>
                       "Data type number" &
                       ODBC.SWORD'Image(Context.Columns (Column).Model) &
                       " not Yet Implemented."
                    );

            end case;
         end loop;
      end Get_Columns_Datas;

   begin --  Fetch
      RC := ODBC.SQLFetch (Context.DBC_Statement_Handle);

      Found := True;

      if RC = ODBC.SQL_NO_DATA_FOUND then
         Found := False;
      else
         Check_SQL_Error (Context.Base, RC,
                          Procedure_Name   => "Fetch",
                          Error_Message    => "Fetch error",
                          Statement_Handle => Context.DBC_Statement_Handle);
         Get_Columns_Datas;
      end if;
   end Fetch;

   -----------------------
   -- Number_Of_Columns --
   -----------------------

   function Number_Of_Columns (Context : in Select_Datas)
                               return Positive is
   begin
      return Context.Columns'Length;
   end Number_Of_Columns;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Context : in Select_Datas;
                      Column  : in Positive)
                      return String is
   begin
      return To_String (Context.Columns (Column).Name);
   end Get_Name;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Context : in Select_Datas;
                       Column  : in Positive)
                       return String is
   begin
      return To_String (Context.Columns (Column).Value);
   end Get_Value;

end Databases.Select_Query;
