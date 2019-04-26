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
	movih r1, 0x00
	movil r1, 0x01
	str r0, r1
	movil r0, 0x02
	str r0, r1

	jmpri 0xfff
