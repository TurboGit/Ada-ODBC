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
--       Module Name : Databases
--         File name : databases.ads
--
--       Created by  : Pascal Obry
--               on  : Thu Mar 21 09:07:43 1996
--
--  Last modified by : $Author$
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  ======================================================================  --
--

with System.Address_To_Access_Conversions;
with Interfaces.C;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

with Databases.Cursors;
with Databases.SQL;
with Databases.Types;

package body Databases is

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   function To_PUCHAR is new Ada.Unchecked_Conversion (System.Address,
                                                       Win32.PUCHAR);

   -------------------------------------------------------------------------

   --  Error handling

   function SQL_Error_Message
     (DB               : in Database;
      Statement_Handle : in ODBC.HSTMT)
      return String
   is
      RC            : ODBC.RETCODE;
      SQL_State     : Interfaces.C.Char_Array (1 .. 10);
      Error_Code    : aliased ODBC.SDWORD;
      Error_Message : String (1 .. 500);
      Last          : aliased ODBC.SWORD;
      use type System.Address;
   begin
      RC := ODBC.SQLError (DB.DBC_Environment_Handle,
                           DB.DBC_Handle,
                           Statement_Handle,
                           To_PUCHAR (SQL_State (SQL_State'First)'Address),
                           Error_Code'Access,
                           To_PUCHAR (Error_Message
                                      (Error_Message'First)'Address),
                           ODBC.SWORD (Error_Message'Length),
                           Last'Access);
      return "(" & Interfaces.C.To_Ada (SQL_State) & ") " &
        Error_Message (Error_Message'First .. Natural (Last));
   end SQL_Error_Message;

   -------------------------------------------------------------------------

   procedure Check_SQL_Error (DB               : in Database;
                              RC               : in ODBC.RETCODE;
                              Procedure_Name   : in String;
                              Error_Message    : in String := "";
                              Statement_Handle : in ODBC.HSTMT
                                               := System.Null_Address)
   is
      use Ada;
      use ODBC;
   begin
      if RC /= ODBC.SQL_SUCCESS and RC /= ODBC.SQL_SUCCESS_WITH_INFO then
         Exceptions.Raise_Exception
           (SQL_Error'Identity,
            "(" & Procedure_Name & ") " &
            Error_Message & " SQL Error : " &
            SQL_Error_Message (DB, Statement_Handle));
      end if;
   end Check_SQL_Error;


   -------------------------------------------------------------------------

   --  Connect-Open / Close

   procedure Connect (DB : in out Database; Driver, UID, PASSWD : in String)
   is
   begin --  Connect
      --  is DB already open ?
      if DB.Driver /= null then
         Ada.Exceptions.Raise_Exception (SQL_Error'Identity,
                                         "(connect) Base already open");
      end if;

      DB.Driver := new String'(Driver);
      DB.UID    := new String'(UID);
      DB.PASSWD := new String'(PASSWD);

      declare
         RC : ODBC.RETCODE;
      begin
         --  environment initialisation
         RC := ODBC.SQLAllocEnv (DB.DBC_Environment_Handle'Access);
         Check_SQL_Error (DB, RC,
                          Procedure_Name => "Connect",
                          Error_Message  => "Allocation Environement");
         RC := ODBC.SQLAllocConnect (DB.DBC_Environment_Handle,
                                     DB.DBC_Handle'Access);
         Check_SQL_Error (DB, RC,
                          Procedure_Name => "Connect",
                          Error_Message  => "Allocation Connection");

         --  connection
         RC := ODBC.SQLConnect
           (DB.DBC_Handle,
            To_PUCHAR (DB.Driver (DB.Driver'First)'Address), Driver'Length,
            To_PUCHAR (DB.UID (DB.UID'First)'Address), UID'Length,
            To_PUCHAR (DB.PASSWD (DB.PASSWD'First)'Address), PASSWD'Length);

         Check_SQL_Error (DB, RC,
                          Procedure_Name => "Connect",
                          Error_Message  => "Connection a la base");
      end ;
   end Connect;

   -------------------------------------------------------------------------

   procedure Close (DB : in out Database)
   is
      RC : ODBC.RETCODE;
   begin --  Close
      RC := ODBC.SQLDisconnect  (DB.DBC_Handle);
      RC := ODBC.SQLFreeConnect (DB.DBC_Handle);
      RC := ODBC.SQLFreeEnv     (DB.DBC_Environment_Handle);

      Free (DB.Driver);
      Free (DB.UID);
      Free (DB.PASSWD);
   end Close;



   -------------------------------------------------------------------------

   --  Columns binding

   function Is_Defined (Query  : in Select_Statement;
                        Column : in Column_Number)
                        return Boolean is
   begin
     return Query.Fields (Column).Name /= Null_Unbounded_String;
   end Is_Defined;

   -------------------------------------------------------------------------

   procedure Bind (Query      : in out Select_Statement;
                   Column     : in     Column_Number;
                   Name       : in     String;
                   Address    : in     System.Address;
                   Size       : in     Natural;
                   Data_Model : in     Data_Type)
   is
   begin --  Bind
      Query.Fields (Column).Name       := To_Unbounded_String (Name);
      Query.Fields (Column).Data_Model := Data_Model;
      Query.Fields (Column).Size       := ODBC.SDWORD (Size);
      Query.Fields (Column).Address    := Address;

      --  check if Data_Model is ok.
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

   -------------------------------------------------------------------------

   procedure Query (Query    : in out Select_Statement;
                    Column   : in     Column_Number;
                    Operator : in     Operators;
                    Value    : in     String)
   is
   begin --  Query
      Query.Fields (Column).Operator    := Operator;
      Query.Fields (Column).Query_Value := To_Unbounded_String (Value);
   end Query;

   -------------------------------------------------------------------------

   procedure Reset_Select (Query : in out Select_Statement) is
   begin
      Free (Query.SQL);
   end Reset_Select;

   -------------------------------------------------------------------------

   function Name (Query  : in Select_Statement;
                  Column : in Column_Number)
                  return String
   is
   begin --  Name
      return To_String (Query.Fields (Column).Name);
   end Name;

   -------------------------------------------------------------------------

   function Query_Value (Query  : in Select_Statement;
                         Column : in Column_Number)
                         return String
   is
   begin --  Query_Value
      return To_String (Query.Fields (Column).Query_Value);
   end Query_Value;

   -------------------------------------------------------------------------

   function Last (Query  : in Select_Statement;
                  Column : in Column_Number)
                  return Natural
   is
   begin --  Value
      return Natural (Query.Fields (Column).Last);
   end Last;



   -------------------------------------------------------------------------

   --  Actions

   function Get_SQL_Select (Query   : in Select_Statement;
                            Table   : in String)
                            return String
   is

      use Ada.Strings.Unbounded;

      Fields     : Unbounded_String;
      Conditions : Unbounded_String;

   begin --  Get_SQL_Select

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
               end case;

               case Query.Fields (Column).Data_Model is
                  when SQL_CHAR | SQL_VARCHAR =>
                     Append (Conditions,
                             ''' &
                             To_String (Query.Fields (Column).Query_Value) &
                             ''');
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
      end Build_Where_Clause;

      return Databases.SQL.Build_Select (Fields     => To_String (Fields),
                                         From       => Table,
                                         Where      => To_String (Conditions),
                                         For_Update => Query.For_Update);
   end Get_SQL_Select;

   -------------------------------------------------------------------------

   procedure SQL_Select (DB     : in     Database;
                         Query  : in out Select_Statement;
                         Table  : in     String;
                         Cursor : in     Databases.Cursor := No_Cursor)
   is
      RC  : ODBC.RETCODE;
   begin --  SQL_Select

      Query.Base := DB;

      --  allocate statement block
      RC := ODBC.SQLAllocStmt (DB.DBC_Handle,
                               Query.DBC_Statement_Handle'Access);
      Check_SQL_Error (DB, RC,
                       Procedure_Name   => "SQL_Select",
                       Error_Message    => "Allocation Statement",
                       Statement_Handle => Query.DBC_Statement_Handle);

      --  set the cursor if needed
      if Cursor /= No_Cursor then
         declare
            Cursor_Name : constant String
                        := Databases.Cursors.Get_Name (Cursor);
         begin
            RC := ODBC.SQLSetCursorName
              (Query.DBC_Statement_Handle,
               To_PUCHAR (Cursor_Name (Cursor_Name'First)'Address),
               Cursor_Name'Length);
            Check_SQL_Error (DB, RC,
                             Procedure_Name   => "SQL_Select",
                             Error_Message    => "Creation du curseur",
                             Statement_Handle => Query.DBC_Statement_Handle);
         end;
      end if;

      --  prepare query and bind column
      Query.SQL := new String'(Get_SQL_Select (Query, Table));

      RC := ODBC.SQLPrepare (Query.DBC_Statement_Handle,
                             To_PUCHAR (Query.SQL (Query.SQL'First)'Address),
                             Query.SQL'Length);
      Check_SQL_Error (DB, RC,
                       Procedure_Name   => "SQL_Select",
                       Error_Message    => "Preparation de la requete",
                       Statement_Handle => Query.DBC_Statement_Handle);

      for Column in Query.Fields'Range loop
         if Is_Defined (Query, Column) then
            RC := ODBC.SQLBindCol
              (Query.DBC_Statement_Handle,
               ODBC.UWORD (Column),
               ODBC.SWORD (Types.SQL_TO_C (Query.Fields (Column).Data_Model)
                           .SQL_Value),
               Query.Fields (Column).Address,
               Query.Fields (Column).Size,
               Query.Fields (Column).Last'Access);
            Check_SQL_Error (DB, RC,
                             Procedure_Name => "SQL_Select",
                             Error_Message  => "Binding de la colonne " &
                             Name (Query, Column));
         end if;
      end loop;

      RC := ODBC.SQLExecute (Query.DBC_Statement_Handle);
      Check_SQL_Error (DB, RC,
                       Procedure_Name   => "SQL_Select",
                       Error_Message    => "Execution de la commande SQL : " &
                                           Query.SQL.all,
                       Statement_Handle => Query.DBC_Statement_Handle);
   end SQL_Select;

   -------------------------------------------------------------------------

   procedure Fetch  (Query : in     Select_Statement;
                     Found :    out Boolean)
   is
      use ODBC;
      RC : ODBC.RETCODE;
   begin --  Fetch
      RC := ODBC.SQLFetch (Query.DBC_Statement_Handle);

      Found := True;

      if RC = ODBC.SQL_NO_DATA_FOUND then
         Found := False;
      else
         Check_SQL_Error (Query.Base, RC,
                          Procedure_Name   => "Fetch",
                          Error_Message    => "Erreur sur le fetch",
                          Statement_Handle => Query.DBC_Statement_Handle);
      end if;
   end Fetch;

   -------------------------------------------------------------------------

   procedure Parameter (Parameters :    out Parameter_Set;
                        Column     : in     Column_Number;
                        Mode       : in     Natural;
                        Address    : in     System.Address;
                        Size       : in     Natural;
                        Data_Model : in     Data_Type)
   is
   begin
      Parameters.Parameters (Column) :=
        (Mode       => ODBC.SWORD (Mode),
         Data_Model => Data_Model,
         Address    => Address,
         Size       => ODBC.SDWORD (Size),
         Last       => ODBC.SDWORD (Size));
   end Parameter;

   -------------------------------------------------------------------------

   procedure Execute (DB         : in Database;
                      Command    : in String;
                      Parameters : in Parameter_Set := No_Parameter)
   is
      DBC_Statement_Handle : aliased ODBC.HSTMT;
      RC                   : ODBC.RETCODE;
   begin
      RC := ODBC.SQLAllocStmt (DB.DBC_Handle,
                               DBC_Statement_Handle'Access);
      Check_SQL_Error (DB, RC,
                       Procedure_Name   => "Exec",
                       Error_Message    => "Allocation Statement",
                       Statement_Handle => DBC_Statement_Handle);

      if Parameters = No_Parameter then
         RC := ODBC.SQLExecDirect
           (DBC_Statement_Handle,
            To_PUCHAR (Command (Command'First)'Address),
            Command'Length);
         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Execution directe de la commande SQL : " &
                                Command,
            Statement_Handle => DBC_Statement_Handle);
      else
         RC := ODBC.SQLPrepare
           (DBC_Statement_Handle,
            To_PUCHAR (Command (Command'First)'Address),
            Command'Length);
         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Preparation de la commande SQL : " &
                                Command,
            Statement_Handle => DBC_Statement_Handle);

         for Parameter_Number in 1 .. Parameters.N loop
            declare
               Parameter : Parameter_Datas
                         := Parameters.Parameters (Parameter_Number);
            begin
               RC := ODBC_EXT.SQLBindParameter
                 (DBC_Statement_Handle,
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
                  Error_Message    => "Lien des parametres de la " &
                                      "commande SQL : " & Command,
                  Statement_Handle => DBC_Statement_Handle);
            end;
         end loop;

         RC := ODBC.SQLExecute (DBC_Statement_Handle);
         Check_SQL_Error
           (DB, RC,
            Procedure_Name   => "Execute",
            Error_Message    => "Execution de la commande SQL : " &
                                Command,
            Statement_Handle => DBC_Statement_Handle);
      end if;

      RC := ODBC.SQLFreeStmt (DBC_Statement_Handle, 0);
   end Execute;

end Databases;
