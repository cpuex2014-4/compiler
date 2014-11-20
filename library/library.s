min_caml_print:
	rsb	$v0
	jr	$ra
min_caml_fprint:
	mfc1	$at, $f0
	rsb	$at
	jr	$ra
