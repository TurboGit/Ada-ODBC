
		   ODBC Samples and a Database abstraction
		   ---------------------------------------

				  release 2


$Id$


This directory contain some ODBC samples. You must have the Win32Ada binding
installed to your computer to use it.

The database used by these samples is very simple but you must create it
to run the samples. I have use Microsoft ACCESS, Excel and a Text file
to test all these samples.

Here is the database structure :

Database name : Clients
3 fields

	Nom    : Text				(lastname)
	Prenom : Text				(firstname)
	Age    : Numeric - Integer		(age)

After creating the database you must connect it with the ODBC driver under
the "Control Panel" (Panneau de Configuration). I have included an ACCESS
(clients.mdb) and an Excel (clients.xls) databases.

The Database packages are provided as-is. This is a demonstration of an
implementation of an Database abstraction on top of Win32 Ada binding
(Win32Ada). This abstraction has been done to learn more about ODBC
and Win32Ada. Note that not all the ODBC functionalities are available from
this abstraction, so if you plan to use it, you'll need sometimes to call
a Win32Ada function.

These samples have been tested with GNAT 3.11 and GNAT 3.12.

To compile the binding run the compile.bat batch.

What New
--------

   since release 1

      Handle SQL Like operator
      Add mode flexibility to the SQL build function, it is now possible
        - to add a specific Where_Clause
        - to add a some specific option to add to the SQL query
	(see Databases.Query)

Acknoledgment
-------------

Thanks to Rob Veenker for his help to fix some bugs.

Thanks to Anders Wirzenius for his contribution and ideas about new features.


Pascal Obry
Team-Ada member.
e-mail : pascal_obry@csi.com
