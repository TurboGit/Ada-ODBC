
# $Id$

INSTALL_DIR	=	/usr/ada.libraries/SQL

all:
	for file in `ls da*.adb`; \
		do gnatmake -i -c $$file; \
	done;

demos:
	gnatmake -i demo -largs -lodbc32
	gnatmake -i demo3 -largs -lodbc32
	gnatmake -i demo4 -largs -lodbc32
	gnatmake -i demo5 -largs -lodbc32

install:
	cp da*.ad[sb]	$(INSTALL_DIR)
	cp da*.{o,ali}	$(INSTALL_DIR)

release:
	-rm databases.tar*
	tar cvf databases.tar databases*.ad[sb] clients.mdb clients.xls \
		compile.bat demo*.adb readme.txt
	gzip -9 databases.tar

clean:
	rm *.o *.ali *.exe
