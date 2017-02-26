; This is currently an attempt to play with OS development in real
; mode (yes I'm aware abusing the BIOS and real mode in general
; is frowned upon). Right now it just plays with multitasking

[BITS 16]			; The code is running in 16 bit real mode
[ORG 0x7C00]			; BIOS loads us here

bootload_main:
	; setup segments
	mov ax, 0
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov fs, ax
	mov gs, ax
	
	mov [DeviceLoadedFrom], dl	; BIOS places what drive we're loaded from into dl, store this for later use

	; should be fairly self-explanatory (print "Hello World!")
	mov si, HelloWorld
	call printk
	
	; Determine available low memory
	call showMem

	; tell the user we're loading a sector
	mov si, LoadingSector
	call printk

	; actually load the sector
	call loadSector

	; jmp to it (execute the code there)
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
	int 0x13
	cmp al, 1			; BIOS Drive Interupt
	je .successful			; Check if we were successful, if 0 we were (some emulators screw this up)
	call dump_regs
	mov si, LoadSectorUnsuccessful
	call printk

	mov ah, 0x01
	mov al, [DeviceLoadedFrom]
	int 0x13

	call dump_regs
	cli
	hlt
.successful
	mov si, LoadSectorSuccessful
	call printk
	ret

showMem:
	pusha
	pushf

	mov si, showMemPrefix
	call printk

	clc
	int 0x12
	jc .error
	call print_hex
	
	mov si, showMemSuffix
	call printk	

	jmp .end

.error
	mov si, showMemErr
	call printk
	jmp .end

.end
	popf
	popa
	ret

showMemErr db 'BIOS does not support int 0x12', 13, 10, 0
showMemPrefix db '0x', 0
showMemSuffix db ' KB are available', 13, 10, 0
	
; Input comes in from ax
print_hex: ; Mostly taken from osdev wiki
	pusha
	pushf ; because I'm being lazy for now, this function clobbers flags
	      ; and I haven't completed the simple task of digging through
	      ; to figure out which ones, just going to save what we have
	      ; and restore back to it

	push ax	; store this for later
	mov [.temp], ah
	mov al, ah
	shr al, 4
	cmp al, 10
	sbb al, 0x69
	das

	mov ah, 0x0E
	int 0x10

	mov al, [.temp]
	ror al, 4
	shr al, 4
	cmp al, 10
	sbb al, 0x69
	das

	mov ah, 0x0E 
	int 0x10

	pop ax ; begin work on the upper half of ax
	mov [.temp], al
	shr al, 4
	cmp al, 10
	sbb al, 0x69
	das

	mov ah, 0x0E
	int 0x10

	mov al, [.temp]
	ror al, 4
	shr al, 4
	cmp al, 10
	sbb al, 0x69
	das

	mov ah, 0x0E
	int 0x10

	popf
	popa
	ret
.temp db 0

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
jmp second_sector

first_task:
	mov si, task1_msg
	call printk

	int 0x80
	
	hlt
	jmp first_task
	
cli
hlt
task1_msg db 'A', 0

task2_msg db 'B', 0
second_task:
	mov si, task2_msg
	call printk

	int 0x80

	hlt
	jmp second_task
cli
hlt
ts_msg db 'C', 0

second_sector:
	pusha

	mov [task1_sp], sp
	; setup second 'thread' information
	mov [task2_sp],  word 0x600 ; arbitrary stack selection
	mov [curr_task], word 0x0

	; Pretty sure there's a cleaner obvious way
	; to do this
	; ... don't judge me
	mov sp, [task2_sp]
	pushf
	push word 0x0 ; cs
	push word second_task ;ip
	pusha
	mov [task2_sp], sp
	mov sp, [task1_sp]

	cli ; hook software interrupt 0x80
	mov [0x200], word handle_task
	mov [0x202], word 0x0
	sti
	jmp first_task

handle_task:	
	pusha ; push everything from the calling task

	mov ax, [curr_task]
	inc ax
	cmp ax, 2
	jl .nowrap
	xor ax, ax
.nowrap	
	mov [curr_task], ax	
	cmp ax, 1
	je .second_task
	mov [task2_sp], word sp
	mov sp, [task1_sp]
	jmp .task
.second_task
	mov [task1_sp], word sp
	mov sp, [task2_sp]
.task
	
	popa
	iret
; rudimentary task switching information
curr_task dw 0
task1_sp  dw 0
task2_sp  dw 0

strcmp:
	.loop
		mov al, [si] ; grab from si
		mov bl, [di]
		cmp al, bl
		jne .notequal

		cmp al, 0
		je .done

		inc di
		inc si
		jmp .loop

	.notequal
		clc
		ret

	.done
		stc
		ret

dump_regs:
	pusha
	pusha

	mov si, .description
	call printk
	pop ax
	call print_hex
	mov si, .endl
	call printk

	pop ax
	call print_hex
	mov si, .endl
	call printk

	pop ax
	call print_hex
	mov si, .endl
	call printk	

	pop ax
	call print_hex
	mov si, .endl
	call printk

	pop ax
	call print_hex
	mov si, .endl
	call printk

	pop ax
	call print_hex
	mov si, .endl
	call printk

	pop ax
	call print_hex
	mov si, .endl
	call printk	

	pop ax
	call print_hex
	mov si, .endl
	call printk

	popa
	ret
.description db 'DI, SI, BP, SP, BX, DX, CX, AX', 13, 10, 0
.endl db 13, 10, 0


testing db 'Testing Second Sector!', 13, 10, 0
prompt db '> ', 0
cmd_help db 'help', 0
cmd_dump db 'dump', 0
cmd_mem  db 'mem', 0
helpstr db 'What help?', 13, 10, 0
badcommand db 'Bad command entered.', 0x0D, 0x0A, 0


buffer times 64 db 0
times 1024-($-$$) db 0 ; make sure we're taking up 512 bytes for this sector and 1024 bytes total (2 sectors)
