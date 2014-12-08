min_caml_print:
        li  $t0, 0xFFFF0000
        sw  $v0, 12($t0)
        jr  $ra
min_caml_fprint:
        mfc1  $at, $f0
        li  $t0, 0xFFFF0000
        sw  $at, 12($t0)
        jr  $ra
min_caml_print_byte:    
        li  $t0, 0xffff0000
wr_poll:
        lw  $t1, 8($t0)
        andi  $t1, $t1, 0x01
        beq $t1, $zero, wr_poll
        sw  $v0, 12($t0)
        jr  $ra
min_caml_read_byte:    
        li  $t0, 0xffff0000
rd_poll:
        lw  $t1, 0($t0)
        andi  $t1, $t1, 0x01
        beq $t1, $zero, rd_poll
        lw  $v0, 4($t0)
        andi  $v0, $v0, 0xff
        jr  $ra
min_caml_create_array:
	sll	$t1, $v0, 2
	addu	$v0, $gp, $zero
	addu	$gp, $gp, $t1
	addiu	$t2, $zero, 0
min_caml_create_array.1:
	addu	$t3, $v0, $t2
	sw	$v1, 0($t3)
	addiu	$t2, $t2, 4
	bne	$t1, $t2, min_caml_create_array.1
	jr	$ra
