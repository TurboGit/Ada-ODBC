
# $Id$

INSTALL_DIR	=	/usr/ada.libraries/SQL

all:
	for file in `ls da*.adb`; \
		do gnatmake -c $$file; \
	done;

demos:
	gnatmake demo -largs -lodbc32
	gnatmake demo3 -largs -lodbc32
	gnatmake demo4 -largs -lodbc32
	gnatmake demo5 -largs -lodbc32

install:
	cp da*.ad[sb]	$(INSTALL_DIR)
	cp da*.{o,ali}	$(INSTALL_DIR)
