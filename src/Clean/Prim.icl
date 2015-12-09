implementation module Clean.Prim

prim_andBool :: !Bool Bool -> Bool
prim_andBool a b = code {
        push_b 0
        jmp_false l1
        pop_b 1
        jsr_eval 0
        pushB_a 0
        pop_a 1
    .d 0 1 b
        rtn
    :l1
        pop_a 1
    .d 0 1 b
        rtn
}

prim_orBool :: !Bool Bool -> Bool
prim_orBool a b = code {
        push_b 0
        jmp_true l2
        pop_b 1
        jsr_eval 0
        pushB_a 0
        pop_a 1
    .d 0 1 b
        rtn
    :l2
        pop_a 1
    .d 0 1 b
        rtn
}

