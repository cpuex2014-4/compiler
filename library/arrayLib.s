min_caml_print:
	rsb	$v0
	jr	$ra
min_caml_fprint:
	mfc1	$at, $f0
	rsb	$at
	jr	$ra
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
