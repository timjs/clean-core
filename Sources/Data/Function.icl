implementation module Data.Function

abort :: !String -> .a
abort s = code inline {
    .d 1 0
        jsr print_string_
    .o 0 0
        halt
}

undefined :: .a
undefined = abort "Run-time error! Program evaluated an undefined value!"

unreachable :: .a
unreachable = abort "Run-time error! Program got to an unreachable branch!"
