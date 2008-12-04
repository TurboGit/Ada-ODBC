
INSTALL_DIR	=	$(HOME)/opt/SQL

all:
	gnat make -p -Pdatabases

demos: all

install:
	mkdir -p $(INSTALL_DIR)/src
	mkdir -p $(INSTALL_DIR)/obj
	cp -p src/* $(INSTALL_DIR)/src
	cp -p obj/data* $(INSTALL_DIR)/obj
	cp -p databases.gpr $(INSTALL_DIR)

release:
	-rm databases.tar*
	tar cvf databases.tar databases.gpr src/databases*.ad[sb] \
		demos/clients.mdb demos/clients.xls makefile \
		demos/demo*.adb demos/test_select.adb readme.txt
	gzip -9 databases.tar

clean:
	gnat clean -Pdatabases
