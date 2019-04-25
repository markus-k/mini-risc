main:
	movil sp, 0xff
	movih sp, 0x00

	movil r0, 0x05
	movih r0, 0x01

	movih r1, 0x06

	movil r2, 0x00
	movih r2, 0x00

	movil r3, 0x01
	movih r3, 0x00

	movil r4, 0x0b
	movih r4, 0x00

	sub r1, r1, r3
	add r2, r2, r0
	cmp r1, r5
	jmpne r4

	movil r5, 0x02

	str r5, r2
	push r2

	sht r2, r2, l, 3
	sht r2, r2, r, 2

	ldr r1, r5
	pop r2

	movil r0, 0x00
	movih r0, 0x80
	movih r1, 0x01
	movil r1, 0x00
	str r0, r1
	movil r0, 0x02
	str r0, r1

	jmp 0xfff
