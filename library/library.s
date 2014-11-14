min_caml_print:
        rsb     $v0
        jr      $ra



# floor(x)
min_caml_floor:
        mtc1    $zero, $f1                # $f1 <-  0.0
        li      $s0, 0x4b000000           #
        mtc1    $s0, $f3                  # $f3 <-  8388608.0
        li      $s0, 0x3f800000           #
        mtc1    $s0, $f4                  # $f4 <-  1.0
        li      $s0, 0xcb000000           #
        mtc1    $s0, $f5                  # $f5 <- -8388608.0
        c.eq.s  $f0, $f1                  # if (isDenormal($f0))
        bc1t    floor.1                   #   goto floor.2
        c.olt.s $f0, $f1                  # if ($f0 < $f1) //$f1 == 0
        bc1t    floor.3                   #   goto floor.1
        c.olt.s $f3, $f0                  # if (8388608.0 < $f0)
        bc1t    floor.2                   #   goto floor.3
        add.s   $f2, $f0, $f3             # $f2 <- $f0 + 8388608.0
        sub.s   $f2, $f2, $f3             # $f2 <- $f2 - 8388608.0
        c.ole.s $f2, $f0                  # if ($f2 <= $f0) //OK
        bc1t    floor.4                   #   goto floor.4
        sub.s   $f0, $f2, $f4             # $f0 <- $f2 - 1.0
        jr      $ra                       # return $f0


floor.1:                                  # // $f0 < 0.0
        c.olt.s $f0, $f5                  # if ($f0 < -8388608.0)
        bc1t    floor.2                   #   goto floor.3
        sub.s   $f2, $f0, $f3             # $f2 <- $f0 - 8388608.0
        add.s   $f2, $f2, $f3             # $f2 <- $f2 + 8388608.0
        c.ole.s $f2, $f0                  # if ($f2 <= $f0)
        bc1t    floor.4                   #   goto floor.4
        sub.s   $f0, $f2, $f4             # $f0 <- $f2 - 1.0
        jr      $ra                       # return $f0

floor.2:
        mov.s   $f0, $f1                  # $f0 <- $f1 //$f1 == 0
floor.3:
        jr      $ra                       # return $f0
floor.4:
        mov.s   $f0, $f2                  # $f0 <- $f2
        jr      $ra                       # return $f0
