# this is a comment

main:
    # initialize stack pointerx
	movil sp, 0xff
	movih sp, 0x00

	movil r0, 0x05
	movih r0, 0x01

    movih r1, 0x00
	movil r1, 0x06

	movil r2, 0x00
	movih r2, 0x00

	movil r3, 0x01
	movih r3, 0x00

	movil r4, 0x0c
	movih r4, 0x00

loop_start:
	sub r1, r1, r3
	add r2, r2, r0
	cmp r1, r5
	jmpc ne, r4

	movil r5, 0x02

	str r5, r2
	push r2

	sht r2, r2, l, 3
	sht r2, r2, r, 2

	ldr r1, r5
	pop r2

	movil r0, 0x00
	movih r0, 0x80
    movil r10, 0x04
	movih r10, 0x80
	movih r1, 0x00
	movil r1, 0x01
	str r0, r1
	movil r0, 0x02
	str r0, r1

    movil r3, 0x00
    movih r3, 0x00
    movil r4, 0xff
    movih r4, 0xff
    movil r6, 0x00
    movih r6, 0x00
    movil r7, 0x03
    movih r7, 0x00
    movil r8, 0x01
    movih r8, 0x00
    # save pc
    mov r5, pc
    add r6, r6, r8
    cmp r6, r7
    jmpc ne, r5
    movil r6, 0x00
    movih r6, 0x00
    xor r3, r3, r4
    # r6 is 0x00 at this point
    cmp r3, r6
    movil r9, 0x37
    movih r9, 0x00
    jmpc eq, r9
    str r10, r1
    jmpc al, r5
else:
    str r0, r1
    jmpc al, r5

	jmpri 0x00
