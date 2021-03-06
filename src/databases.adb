------------------------------------------------------------------------------
--                                Databases                                 --
--                                                                          --
--                        Copyright (C) 1999-2018                           --
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

with Interfaces.C;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

with Databases.Cursors;
with Databases.SQL;
with Databases.Types;

package body Databases is

   pragma Linker_Options ("-lodbc32");

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   function To_PUCHAR is new Ada.Unchecked_Conversion (System.Address,
                                                       Win32.PUCHAR);

   -----------------------
   -- SQL_Error_Message --
   -----------------------

   function SQL_Error_Message
     (DB               : in Database;
      Statement_Handle : in ODBC.HSTMT) return String
   is
      use type System.Address;

      RC            : ODBC.RETCODE with Unreferenced;
      SQL_State     : Interfaces.C.char_array (1 .. 10);
      Error_Code    : aliased ODBC.SDWORD;
      Error_Message : String (1 .. 500);
      Last          : aliased ODBC.SWORD := 0;
   begin
      RC := ODBC.SQLError
        (DB.DBC_Environment_Handle,
         DB.DBC_Handle,
         Statement_Handle,
         To_PUCHAR (SQL_State (SQL_State'First)'Address),
         Error_Code'Access,
         To_PUCHAR (Error_Message (Error_Message'First)'Address),
         ODBC.SWORD (Error_Message'Length),
         Last'Access);

      return "(" & Interfaces.C.To_Ada (SQL_State) & ") "
        & Error_Message
            (Error_Message'First .. Natural (ODBC.SWORD'Max (0, Last)));
   end SQL_Error_Message;

   ---------------------
   -- Check_SQL_Error --
   ---------------------

   procedure Check_SQL_Error
     (DB               : in Database;
      RC               : in ODBC.RETCODE;
      Procedure_Name   : in String;
      Error_Message    : in String := "";
      Statement_Handle : in ODBC.HSTMT := System.Null_Address)
   is
      use Ada;
      use ODBC;
   begin
      if RC /= ODBC.SQL_SUCCESS and RC /= ODBC.SQL_SUCCESS_WITH_INFO then
         Exceptions.Raise_Exception
           (SQL_Error'Identity,
            "(" & Procedure_Name & ") "
            & Error_Message & " SQL Error : "
            & SQL_Error_Message (DB, Statement_Handle));
      end if;
   end Check_SQL_Error;

   -------------
   -- Connect --
   -------------

   procedure Connect (DB : in out Database; Driver, UID, PASSWD : in String) is
   begin
      --  is DB already open ?
      if DB.Driver /= null then
         raise SQL_Error with "(connect) Base already open";
      end if;

      DB.Driver := new String'(Driver);
      DB.UID    := new String'(UID);
      DB.PASSWD := new String'(PASSWD);

      declare
         RC : ODBC.RETCODE;
      begin
         --  environment initialisation
         RC := ODBC.SQLAllocEnv (DB.DBC_Environment_Handle'Access);

         Check_SQL_Error
           (DB, RC,
            Procedure_Name => "Connect",
            Error_Message  => "Allocation Environment");

         RC := ODBC.SQLAllocConnect
           (DB.DBC_Environment_Handle,
            DB.DBC_Handle'Access);

         Check_SQL_Error
           (DB, RC,
            Procedure_Name => "Connect",
            Error_Message  => "Allocation Connection");

         --  connection
         declare
            UID_Addr    : Win32.PUCHAR := null;
            PASSWD_Addr : Win32.PUCHAR := null;
         begin
            if DB.UID'Length /= 0 then
               UID_Addr := To_PUCHAR (DB.UID (DB.UID'First)'Address);
            end if;

            if DB.PASSWD'Length /= 0 then
               PASSWD_Addr := To_PUCHAR (DB.PASSWD (DB.PASSWD'First)'Address);
            end if;

            RC := ODBC.SQLConnect
              (DB.DBC_Handle,
               To_PUCHAR (DB.Driver (DB.Driver'First)'Address), Driver'Length,
               UID_Addr, UID'Length, PASSWD_Addr, PASSWD'Length);
         end;

         Check_SQL_Error
           (DB, RC,
            Procedure_Name => "Connect",
            Error_Message  => "Base connection");
      end ;
   end Connect;

   -----------
   -- Close --
   -----------

   procedure Close (DB : in out Database) is
      RC : ODBC.RETCODE with Unreferenced;
   begin
      RC := ODBC.SQLDisconnect  (DB.DBC_Handle);
      RC := ODBC.SQLFreeConnect (DB.DBC_Handle);
      RC := ODBC.SQLFreeEnv     (DB.DBC_Environment_Handle);

      Free (DB.Driver);
      Free (DB.UID);
      Free (DB.PASSWD);
   end Close;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined
     (Query  : in Select_Statement;
      Column : in Column_Number)
      return Boolean is
   begin
     return Query.Fields (Column).Name /= Null_Unbounded_String;
   end Is_Defined;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Query      : in out Select_Statement;
      Column     : in     Column_Number;
      Name       : in     String;
      Address    : in     System.Address;
      Size       : in     Natural;
      Data_Model : in     Data_Type) is
   begin
      Query.Fields (Column).Name       := To_Unbounded_String (Name);
      Query.Fields (Column).Data_Model := Data_Model;
      Query.Fields (Column).Size       := ODBC.SDWORD (Size);
      Query.Fields (Column).Address    := Address;

      --  check if Data_Model is ok

      case Data_Model is
         when SQL_DATE =>
            null;
         when SQL_CHAR | SQL_VARCHAR =>
            null;
         when SQL_NUMERIC | SQL_DECIMAL | SQL_INTEGER |
           SQL_SMALLINT | SQL_FLOAT | SQL_REAL | SQL_DOUBLE =>
            null;
         when others =>
            raise Data_Type_Error;
      end case;
   end Bind;

   -----------
   -- Query --
   -----------

   procedure Query
     (Query    : in out Select_Statement;
      Column   : in     Column_Number;
      Operator : in     Operators;
      Value    : in     String) is
   begin
      Query.Fields (Column).Operator    := Operator;
      Query.Fields (Column).Query_Value := To_Unbounded_String (Value);
   end Query;

   -----------
   -- Query --
   -----------

   procedure Query
     (Query        : in out Select_Statement;
      Where_Clause : in     String := "";
      SQL_Clause   : in     String := "") is
   begin
      if Where_Clause /= "" then
         Query.Where_Clause := To_Unbounded_String (Where_Clause);
      end if;

      if SQL_Clause /= "" then
         Query.SQL_Clause := To_Unbounded_String (SQL_Clause);
      end if;
   end Query;

   ------------------
   -- Reset_Select --
   ------------------

   procedure Reset_Select (Query : in out Select_Statement) is
   begin
      Free (Query.SQL);
   end Reset_Select;

   ----------
   -- Name --
   ----------

   function Name
     (Query  : in Select_Statement;
      Column : in Column_Number) return String is
   begin
      return To_String (Query.Fields (Column).Name);
   end Name;

   -----------------
   -- Query_Value --
   -----------------

   function Query_Value
     (Query  : in Select_Statement;
      Column : in Column_Number) return String is
   begin
      return To_String (Query.Fields (Column).Query_Value);
   end Query_Value;

   ----------
   -- Last --
   ----------

   function Last
     (Query  : in Select_Statement;
      Column : in Column_Number) return Natural is
   begin
      return Natural (Query.Fields (Column).Last);
   end Last;

   --------------------
   -- Get_SQL_Select --
   --------------------

   function Get_SQL_Select
     (Query   : in Select_Statement;
      Table   : in String) return String
   is

      Fields     : Unbounded_String;
      Conditions : Unbounded_String;

   begin
      --  columns

      for Column in Query.Fields'Range loop
         if Is_Defined (Query, Column) then
            if Column /= Query.Fields'First then
               Fields := Fields & ", ";
            end if;

            Fields := Fields & Name (Query, Column);
         end if;
      end loop;

      --  where

      Build_Where_Clause :
      declare
         N : Positive := 1;
      begin
         for Column in Query.Fields'Range loop
            if Query.Fields (Column).Query_Value /= Null_Unbounded_String then
               if N > 1 then
                  Append (Conditions, " AND ");
               end if;

               Append (Conditions, Name (Query, Column));

               case Query.Fields (Column).Operator is
                  when Equal =>
                     Append (Conditions, "=");
                  when Not_Equal =>
                     Append (Conditions, "!=");
                  when Like =>
                     Append (Conditions, " like ");
               end case;

               case Query.Fields (Column).Data_Model is
                  when SQL_CHAR | SQL_VARCHAR =>
                     Append (Conditions,
                             ''' &
                             To_String (Query.Fields (Column).Query_Value)
                             & ''');
                  when SQL_NUMERIC | SQL_DECIMAL | SQL_INTEGER |
                    SQL_SMALLINT | SQL_FLOAT | SQL_REAL | SQL_DOUBLE =>
                     Append (Conditions,
                             To_String (Query.Fields (Column).Query_Value));
                  when others =>
                     raise Data_Type_Error;
               end case;

               N := N + 1;
            end if;
         end loop;

         if Query.Where_Clause /= Null_Unbounded_String then
            if N > 1 then
               Append (Conditions, " AND ");
            end if;
            Append (Conditions, Query.Where_Clause);
         end if;
      end Build_Where_Clause;

      declare
         Select_Statement : constant String :=
                              Databases.SQL.Build_Select
                                (Fields     => To_String (Fields),
                                 From       => Table,
                                 Where      => To_String (Conditions),
                                 For_Update => Query.For_Update);
      begin
         if Query.SQL_Clause /= Null_Unbounded_String then
            return Select_Statement & ' ' & To_String (Query.SQL_Clause);
         else
            return Select_Statement;
         end if;
      end;
   end Get_SQL_Select;

   ----------------
   -- SQL_Select --
   ----------------

   procedure SQL_Select
     (DB     : in     Database;
      Query  : in out Select_Statement;
      Table  : in     String;
      Cursor : in     Databases.Cursor := No_Cursor)
   is
      RC : ODBC.RETCODE;
   begin

      Query.Base := DB;

      --  allocate statement block

      RC := ODBC.SQLAllocStmt
        (DB.DBC_Handle, Query.DBC_Statement_Handle'Access);

      Check_SQL_Error
        (DB, RC,
         Procedure_Name   => "SQL_Select",
         Error_Message    => "Allocation Statement",
         Statement_Handle => Query.DBC_Statement_Handle);

      --  set the cursor if needed

      if Cursor /= No_Cursor then
         declare
            Cursor_Name : constant String :=
                            Databases.Cursors.Get_Name (Cursor);
         begin
            RC := ODBC.SQLSetCursorName
              (Query.DBC_Statement_Handle,
               To_PUCHAR (Cursor_Name (Cursor_Name'First)'Address),
               Cursor_Name'Length);

            Check_SQL_Error
              (DB, RC,
               Procedure_Name   => "SQL_Select",
               Error_Message    => "Cursor creation",
               Statement_Handle => Query.DBC_Statement_Handle);
         end;
      end if;

      --  prepare query and bind column

      Query.SQL := new String'(Get_SQL_Select (Query, Table));

      RC := ODBC.SQLPrepare
        (Query.DBC_Statement_Handle,
         To_PUCHAR (Query.SQL (Query.SQL'First)'Address),
         Query.SQL'Length);

      Check_SQL_Error
        (DB, RC,
         Procedure_Name   => "SQL_Select",
         Error_Message    => "Preparing query",
         Statement_Handle => Query.DBC_Statement_Handle);

      for Column in Query.Fields'Range loop
         if Is_Defined (Query, Column) then
            RC := ODBC.SQLBindCol
              (Query.DBC_Statement_Handle,
               ODBC.UWORD (Column),
               Types.SQL_To_C (Query.Fields (Column).Data_Model).SQL_Value,
               Query.Fields (Column).Address,
               Query.Fields (Column).Size,
               Query.Fields (Column).Last'Access);

            Check_SQL_Error
              (DB, RC,
               Procedure_Name => "SQL_Select",
               Error_Message  => "Binding column : " & Name (Query, Column));
         end if;
      end loop;

      RC := ODBC.SQLExecute (Query.DBC_Statement_Handle);

      Check_SQL_Error
        (DB, RC,
         Procedure_Name   => "SQL_Select",
         Error_Message    => "Execute SQL statement : " & Query.SQL.all,
         Statement_Handle => Query.DBC_Statement_Handle);
   end SQL_Select;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (Query : in     Select_Statement;
      Found :    out Boolean)
   is
      use ODBC;
      RC : ODBC.RETCODE;
   begin
      RC := ODBC.SQLFetch (Query.DBC_Statement_Handle);

      Found := True;

      if RC = ODBC.SQL_NO_DATA_FOUND then
         Found := False;
      else
         Check_SQL_Error
           (Query.Base, RC,
            Procedure_Name   => "Fetch",
            Error_Message    => "Fetch error",
            Statement_Handle => Query.DBC_Statement_Handle);
      end if;
   end Fetch;

   ---------------
   -- Parameter --
   ---------------

   procedure Parameter
     (Parameters : in out Parameter_Set;
      Column     : in     Column_Number;
      Mode       : in     Natural;
      Address    : in     System.Address;
      Size       : in     Natural;
      Data_Model : in     Data_Type) is
   begin
      Parameters.Parameters (Column) :=
        (Mode       => ODBC.SWORD (Mode),
         Data_Model => Data_Model,
         Address    => Address,
         Size       => ODBC.SDWORD (Size),
         Last       => ODBC.SDWORD (Size));
   end Parameter;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (DB         : in Database;
      Command    : in String;
      Parameters : in Parameter_Set := No_Parameter)
   is
      DBC_Statement_Handle : aliased ODBC.HSTMT;
      RC                   : ODBC.RETCODE;
      RC2                  : ODBC.RETCODE with Unreferenced;
      Tmp_Params           : Parameter_Array := Parameters.Parameters;
   begin
      RC := ODBC.SQLAllocStmt (DB.DBC_Handle, DBC_Statement_Handle'Access);

      Check_SQL_Error
        (DB, RC,
         Procedure_Name   => "Execute",
         Error_Message    => "Allocation statement",
         Statement_Handle => DBC_Statement_Handle);

      if Parameters = No_Parameter then
         RC := ODBC.SQLExecDirect
           (DBC_Statement_Handle,
            To_PUCHAR (Command (Command'First)'Address),
            Command'Length);

         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Direct execution of SQL statement : "
                                 & Command,
            Statement_Handle => DBC_Statement_Handle);

      else
         RC := ODBC.SQLPrepare
           (DBC_Statement_Handle,
            To_PUCHAR (Command (Command'First)'Address),
            Command'Length);

         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Preparation of SQL statement : "
                                & Command,
            Statement_Handle => DBC_Statement_Handle);

         for Parameter_Number in 1 .. Parameters.N loop
            RC := ODBC_EXT.SQLBindParameter
              (DBC_Statement_Handle,
               ODBC.UWORD (Parameter_Number),
               Tmp_Params (Parameter_Number).Mode,
               Types.SQL_To_C
                  (Tmp_Params (Parameter_Number).Data_Model).C_Value,
               Types.SQL_To_C
                  (Tmp_Params (Parameter_Number).Data_Model).SQL_Value,
               ODBC.UDWORD (Tmp_Params (Parameter_Number).Size),
               0,
               Tmp_Params (Parameter_Number).Address,
               Tmp_Params (Parameter_Number).Size,
               Tmp_Params (Parameter_Number).Last'Access);

            Check_SQL_Error
              (DB, RC,
               Procedure_Name   => "Execute",
               Error_Message    => "Parameters of SQL statement : " & Command,
               Statement_Handle => DBC_Statement_Handle);
         end loop;

         RC := ODBC.SQLExecute (DBC_Statement_Handle);
         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Execution of SQL statement : " & Command,
            Statement_Handle => DBC_Statement_Handle);
      end if;

      RC2 := ODBC.SQLFreeStmt (DBC_Statement_Handle, 0);
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Query : in out Select_Statement) is
      RC  : ODBC.RETCODE;
   begin
      --  release  memory used by this Query

      Free (Query.SQL);

      --  free statement block

      RC := ODBC.SQLFreeStmt (Query.DBC_Statement_Handle, ODBC.SQL_DROP);

      Check_SQL_Error
        (Query.Base, RC,
         Procedure_Name   => "Free",
         Error_Message    => "Free Statement Block",
         Statement_Handle => Query.DBC_Statement_Handle);
   end Free;

end Databases;
