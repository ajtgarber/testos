mkisofs -U -D -floppy-boot -no-emul-boot -boot-load-size 4 -b test -c boot.catalog -hide test -hide boot.catalog -V "TestOS" -iso-level 3 -L -o test.iso .
