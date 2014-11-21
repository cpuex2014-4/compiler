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
        bc1t    floor.2                   #   goto floor.2
        c.olt.s $f0, $f1                  # if ($f0 < $f1) //$f1 == 0
        bc1t    floor.1                   #   goto floor.1
        c.olt.s $f3, $f0                  # if (8388608.0 < $f0)
        bc1t    floor.3                   #   goto floor.3
        add.s   $f2, $f0, $f3             # $f2 <- $f0 + 8388608.0
        sub.s   $f2, $f2, $f3             # $f2 <- $f2 - 8388608.0
        c.ole.s $f2, $f0                  # if ($f2 <= $f0) //OK
        bc1t    floor.4                   #   goto floor.4
        sub.s   $f0, $f2, $f4             # $f0 <- $f2 - 1.0
        jr      $ra                       # return $f0


floor.1:                                  # // $f0 < 0.0
        c.olt.s $f0, $f5                  # if ($f0 < -8388608.0)
        bc1t    floor.3                   #   goto floor.3
        sub.s   $f2, $f0, $f3             # $f2 <- $f0 - 8388608.0
        add.s   $f2, $f2, $f3             # $f2 <- $f2 + 8388608.0
        c.ole.s $f2, $f0                  # if ($f2 <= $f0)
        bc1t    floor.4                   #   goto floor.4
        sub.s   $f0, $f2, $f4             # $f0 <- $f2 - 1.0
        jr      $ra                       # return $f0

floor.2:                                  # // Denormal
        mov.s   $f0, $f1                  # $f0 <- $f1 //$f1 == 0
floor.3:                                  # // return
        jr      $ra                       # return $f0
floor.4:
        mov.s   $f0, $f2                  # $f0 <- $f2
        jr      $ra                       # return $f0

# reduction2Pi
min_caml_reduction2Pi:
        li      $s0, 0x40490fda           #
        mtc1    $s0, $f1                  # $f1 <- PI
        li      $s0, 0x40c90fdb
        mtc1    $s0, $f2                  # $f2 <- 2*PI
        li      $s0, 0x40000000
        mtc1    $s0, $f3                  # $f3 <- 2.0
        li      $s0, 0x3f000000
        mtc1    $s0, $f4                  # $f4 <- 0.5
reduction2Pi_while.1:                     # do {
        mul.s   $f1, $f1, $f3             #   $f1 <- $f1 * 2.0
        c.ole.s $f1, $f0                  # } while ($f1 <= $f0)
        bc1t    reduction2Pi_while.1
reduction2Pi_while.2:                     # do {
        c.ole.s $f1, $f0                  #   if ($f1 <= $f0) {
        bc1f    reduction2Pi_endif.1      #
        sub.s   $f0, $f0, $f1             #     $f0 <- $f0 - $f1
reduction2Pi_endif.1:                     #   }
        mul.s   $f1, $f1, $f4             #   $f1 <- $f1 * 0.5
        c.ole.s $f2, $f0                  # } while (2*PI <= $f1)
        bc1t    reduction2Pi_while.2      #
        jr      $ra


# kernel_sin
# A + 0xbe2aaaac * A^3 + 0x3c088666 * A^5 + 0xb94d64b6 * A^7
min_caml_kernel_sin:
        li      $s0, 0xbe2aaaac
        mtc1    $s0, $f3                  # $f3 <- -0.16666668
        li      $s0, 0x3c088666
        mtc1    $s0, $f4                  # $f4 <-  0.008332824
        li      $s0, 0xb94d64b6
        mtc1    $s0, $f5                  # $f5 <- -0.00019587841
        mul.s   $f1, $f0, $f0             # $f1 <- A^2
        mul.s   $f2, $f1, $f0             # $f2 <- A^3
        mul.s   $f3, $f3, $f2             # $f3 <- -0.16666668 * A^3
        add.s   $f0, $f0, $f3             # $f0 <- $f0 + $f3
        mul.s   $f2, $f2, $f1             # $f2 <- A^5
        mul.s   $f4, $f4, $f2             # $f4 <-  0.00833282 * A^5
        add.s   $f0, $f0, $f4             # $f0 <- $f0 + $f4
        mul.s   $f2, $f2, $f1             # $f2 <- A^7
        mul.s   $f5, $f5, $f2             # $f5 <- -0.00019587 * A^7
        add.s   $f0, $f0, $f5             # $f0 <- $f0 + $f5
        jr      $ra

# kernel_cos
# 1.0 + 0xbf000000 * A^2 + 0x3d2aa789 * A^4 + 0xbab38106 * A^6
min_caml_kernel_cos:
        li      $s0, 0xbf000000
        mtc1    $s0, $f4                  # $f4 <- 0.5
        li      $s0, 0x3d2aa789
        mtc1    $s0, $f5                  # $f5 <-  0.04166368
        li      $s0, 0xbab38106
        mtc1    $s0, $f6                  # $f6 <- -0.0013695068
        mul.s   $f1, $f0, $f0             # $f1 <- A^2
        li      $s0, 0x3f800000
        mtc1    $s0, $f0                  # $f0 <- 1.0
        mul.s   $f2, $f4, $f1             # $f2 <- 0.5 * A^2
        add.s   $f0, $f0, $f2             # $f0 <- $f0 + $f2
        mul.s   $f3, $f2, $f2             # $f3 <- A^4
        mul.s   $f2, $f5, $f3             # $f2 <- 0.04166368 * A^4
        add.s   $f0, $f0, $f2             # $f0 <- $f0 + $f2
        mul.s   $f2, $f1, $f3             # $f2 <- A^6
        mul.s   $f1, $f6, $f2             # $f1 <-  -0.0013695, $f2
        add.s   $f0, $f0, $f1             # $f0 <- $f0 + $f1
        jr      $ra

# sin
# $f0=A, $s1=signFlag (even or odd), $f2 = 0.0
min_caml_sin:
        mfc1    $s2, $f0
        srl     $s1, $s2, 0x1f            # $s1 <- sgn($f0)
        sll     $s2, $s2, 1
        srl     $s2, $s2, 1
        mtc1    $s2, $f0                  # $f0 <- abs($f0)


        addiu   $sp, $sp, -8              #
        sw      $ra, 4($sp)               #
        sw      $s1, 0($sp)               # store signFlag
        jal     min_caml_reduction2Pi     # $f0 <- reduction(abs(A))
        lw      $s1, 0($sp)               # load signFlag
        mtc1    $zero, $f2                # $f2 <-  0.0
        li      $s0, 0x40490fda           #
        mtc1    $s0, $f3                  # $f3 <-  PI
        c.ole.s $f3, $f0                  # if (PI <= $f0) {
        bc1f    sin_endif.2               #
        sub.s   $f0, $f0, $f3             #   $f0 <- $f0 - PI
        addiu    $s1, $s1, 0x1            #   reverse(flg)
sin_endif.2:                              # }
        li      $s0, 0x3fc90fdb           #
        mtc1    $s0, $f4                  # $f4 <-  PI/2
        c.ole.s $f4, $f0                  # if (PI/2 <= $f0) {
        bc1f    sin_endif.3               #
        sub.s   $f0, $f3, $f0             #   $f0 <- PI - $f0
sin_endif.3:                              # }
        li      $s0, 0x3f490fdb           #
        mtc1    $s0, $f5                  # $f5 <-  PI/4
        sw      $s1, 0($sp)               # store signFlag
        c.ole.s $f0, $f5                  # if ($f0 <= PI/4) {
        bc1f    sin_else.1                #
        jal     min_caml_kernel_sin       #   $f0 <- kernel_sin($f0)
        j       sin_endif.4               #
sin_else.1:                               # } else {
        sub.s   $f0, $f4, $f0             #   $f0 <- PI/2 - $f0
        jal     min_caml_kernel_cos       #   $f0 <- kernel_cos($f0)
sin_endif.4:                              # }
        lw      $s1, 0($sp)               # load signFlag
        sll     $s1, $s1, 31
        li      $s2, 0x80000000           # $s2 <- 1<<31
        mfc1    $s2, $f0                  #
        addu    $s2, $s1, $s2             # add Flag
        mtc1    $s2, $f0                  #

        lw      $ra, 4($sp)
        addiu   $sp, $sp, 8
        jr      $ra


# cos
# $f0=A, $s1=signFlag (even or odd), $f2 = 0.0
min_caml_cos:
        mfc1    $s2, $f0
        sll     $s2, $s2, 1
        srl     $s2, $s2, 1
        mtc1    $s2, $f0                  # $f0 <- abs($f0)

        addiu   $sp, $sp, -8
        sw      $ra, 4($sp)
        jal     min_caml_reduction2Pi     # $f0 <- reduction(abs(A))

        mov     $s1, $zero                # $s1 <- '+'
        mtc1    $zero, $f2                # $f2 <-  0.0
        li      $s0, 0x40490fda
        mtc1    $s0, $f3                  # $f3 <-  PI
        c.ole.s $f3, $f0                  # if (PI <= $f0) {
        bc1f    cos_endif.2               #
        sub.s   $f0, $f0, $f3             #   $f0 <- $f0 - PI
        addiu   $s1, $s1, 0x1             #  reverse(flg)
cos_endif.2:                              # }
        li      $s0, 0x3fc90fdb
        mtc1    $s0, $f4                  # $f4 <-  PI/2
        c.ole.s $f4, $f0                  # if (PI/2 <= $f0) {
        bc1f    cos_endif.3               #
        sub.s   $f0, $f3, $f0             #   $f0 <- PI - $f0
        addiu   $s1, $s1, 0x1             #   reverse(flg)
cos_endif.3:                              # }
        li      $s0, 0x3f490fdb
        mtc1    $s0, $f5                  # $f5 <-  PI/4
        sw      $s1, 0($sp)               # store signFlag
        c.ole.s $f0, $f5                  # if ($f0 <= PI/4) {
        bc1f    cos_else.1                #
        jal     min_caml_kernel_cos       #   $f0 <- kernel_cos($f0)
        j       cos_endif.4               #
cos_else.1:                               # } else {
        sub.s   $f0, $f4, $f0             #   $f0 <- PI/2 - $f0
        jal     min_caml_kernel_sin       #   $f0 <- kernel_sin($f0)
cos_endif.4:                              # }
        lw      $s1, 0($sp)               # load signFlag
        sll     $s1, $s1, 31
        li      $s2, 0x80000000           # $s2 <- 1<<31
        mfc1    $s2, $f0                  #
        addu    $s2, $s1, $s2             # add Flag
        mtc1    $s2, $f0                  #
        lw      $ra, 4($sp)
        addiu   $sp, $sp, 8
        jr      $ra
