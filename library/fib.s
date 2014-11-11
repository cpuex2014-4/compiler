fib.10:
	addiu	$at, $zero, 2
	slt	$at, $v0, $at
	bne	$at, $zero, branch.24
	addiu	$v1, $v0, -1
	sw	$v0, 0($sp)
	addu	$v0, $v1, $zero
	sw	$ra, 8($sp)
	addiu	$sp, $sp, -12
	jal	fib.10
	addiu	$sp, $sp, 12
	lw	$ra, 8($sp)
	lw	$v1, 0($sp)
	addiu	$v1, $v1, -2
	sw	$v0, 4($sp)
	addu	$v0, $v1, $zero
	sw	$ra, 8($sp)
	addiu	$sp, $sp, -12
	jal	fib.10
	addiu	$sp, $sp, 12
	lw	$ra, 8($sp)
	lw	$v1, 4($sp)
	addu	$v0, $v1, $v0
	jr	$ra
branch.24:
	addiu	$v0, $zero, 1
	jr	$ra
main:
	addiu	$v0, $zero, 10
	jal	fib.10
	jal	min_caml_print
