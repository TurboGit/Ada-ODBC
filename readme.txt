
		   ODBC Samples and a Database abstraction
		   ---------------------------------------

				  release 2


This directory contain some ODBC samples. You must have the Win32Ada binding
installed to your computer to use it.

The database used by these samples is very simple but you must create it
to run the samples. I have used Microsoft ACCESS, Excel and a Text file
to test all these samples. It has also been tested on
 - Windows: XP, Server 2003, Vista
 - SQL Server: 2000 and 2005; Access: 2002, 2007

Here is the database structure :

Database name : Clients
3 fields

	Nom    : Text				(lastname)
	Prenom : Text				(firstname)
	Age    : Numeric - Integer		(age)

After creating the database you must connect it with the ODBC driver under
the "Control Panel" (Panneau de Configuration), then "Administrative Tools".
I have included an ACCESS (clients.mdb) and an Excel (clients.xls) databases.
In the tab "User DSN" (could be also System of File DSN), click the "Add"
button, select the "Microsoft Access Driver (*.mdb)" choice,
then "Finish", then give "Clients_DB" to the data source
name (this is the name which is used in the demos, could be different of
course), then select ("Select...") the provided database file "clients.mdb".

The Database packages are provided as-is. This is a demonstration of an
implementation of an Database abstraction on top of Win32 Ada binding
(Win32Ada). This abstraction has been done to learn more about ODBC
and Win32Ada. Note that not all the ODBC functionalities are available from
this abstraction, so if you plan to use it, you'll need sometimes to call
a Win32Ada function.

These samples have been tested with GNAT GPL 2008.

To compile the binding run:

   $ make

What's New
----------

   since release 1

      Handle SQL "Like" operator
      Add mode flexibility to the SQL build function, it is now possible
        - to add a specific Where_Clause
        - to add a some specific option to add to the SQL query
	(see Databases.Query)

Demos
-----

demo:  demo of SELECT statement, with retrieval of data, column numbers
       aliased to names.

demo3: demo of INSERT statement (model for any statement without output
       from the database)

demo4: another demo of SELECT statement, with retrieval of data

demo5: another demo


Tools
-----

record_generator: generates an Ada source snippet with a record type
                  corresponding to a table's model (column names and types)

Acknowledgment
--------------

Thanks to Rob Veenker for his help to fix some bugs.

Thanks to Anders Wirzenius for his contribution and ideas about new features.

Thanks to Gautier de Montmollin for testing it.

Pascal Obry
Team-Ada member.
e-mail : pascal@obry.net
