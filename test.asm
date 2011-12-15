; This "bootloader" simply loads its second section and executes it
; it runs soley in real mode and uses the BIOS as a crutch

; references being Wikipedia and the all mighty Google

; to assemble this file (using NASM) use:
; nasm -f bin test.asm -o test.bin
; to generate a floppy emulated iso file use this command (assuming you keep test.asm as the name)
;  mkisofs -U -D -floppy-boot -no-emul-boot -boot-load-size 4 -b test.bin -c boot.catalog -hide test.bin -hide boot.catalog -V "TestOS" -iso-level 3 -L -o test.iso . 
; if you don't specify the -no-emul-boot you may have to pad the test.bin with zeroes to make it (this is a guess
; 1.44 MB in size).

[BITS 16]			; The code is running in 16 bit real mode
[ORG 0x7C00]			; BIOS loads us here

bootload_main:
	mov [DeviceLoadedFrom], dl	; BIOS places what drive we're loaded from into dl, store this for later use
	; setup segments
	mov ax, 0
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov fs, ax
	mov gs, ax
	
	; should be fairly self-explanitory (print "Hello World!")
	mov si, HelloWorld
	call printk
	
	; tell the user we're loading a sector
	mov si, LoadingSector
	call printk

	; actually load the sector
	call loadSector

	; jmp to it (execute the code there, careful on doing this on non-code areas)
	jmp 0x7E00

	; just in case something goes horribly wrong and we're still here say this
	mov si, StillRunning
	call printk
	cli ; then disable interrupts and halt the processor
	hlt

printk:
	lodsb		; load a byte from si into al
	or al, al	; used to set processor flags (could replace with cmp al, al)
	jz .done	; check if al was zero if so, then leave
	mov ah, 0x0E	; BIOS call for write character
	int 0x10	; interrupt the BIOS
	jmp printk	; loop
.done			; we're done
	ret

loadSector:
	; I've found running these next three lines results in an infinite loop on my netbook
	; so I remove it when testing on baremetal, it works in emulators like qemu and virtualbox
	; I should've probably passed in a dl though
	mov ah, 0x01 	; BIOS function check error
	int 0x13	; interrupt the BIOS
	jnz loadSector  ; loop if an error occured (or not ready, shouldn't happen under qemu)
	
	mov ah, 0x02 			; read sector
	mov al, 1    			; num sectors to read
	mov ch, 0    			; track (0 based)
	mov cl, 2    			; sector (1 based)
	mov dh, 0    			; head (0 based)
	mov dl, [DeviceLoadedFrom] 	; read from the device we we're loaded from
	mov bx, 0x7E00			; save the sector at 0x7E00, the next 512 bytes in RAM next to us
	int 0x13			; BIOS Drive Interupt
	cmp ah, 0x00			; Check if we were successful, if 0 we were (some emulators screw this up)
	je .successful
	mov si, LoadSectorUnsuccessful
	call printk
	ret
.successful
	mov si, LoadSectorSuccessful
	call printk
	ret

HelloWorld db 'Hello, World!', 13, 10, 0
LoadingSector db 'Loading Second Sector...', 13, 10, 0
LoadSectorSuccessful db 'Loaded Sector Successfully!', 13, 10, 0
LoadSectorUnsuccessful db 'Unsuccessful Sector load :(', 13, 10, 0
StillRunning	db 'Still Running... :(', 13, 10, 0
DeviceLoadedFrom db 0

times 510-($-$$) db 0 		; pad with zeroes to insure our boot sector is 512 bytes exactly
dw 0xAA55			; Boot signature

; START OF LOADED SECTOR
; this is what is loaded by loadSector

; print out a test string to make sure this was loaded
mov si, testing
call printk
cli
hlt

testing db 'Testing Second Sector! Now Halting! YAY!!!!', 13, 10, 0
times 1024-($-$$) db 0 ; make sure we're taking up 512 bytes for this sector and 1024 bytes total (2 sectors)
