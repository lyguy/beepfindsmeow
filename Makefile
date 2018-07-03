INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	jbuilder build --dev @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	jbuilder clean

.PHONY: default test clean install uninstall reinstall
