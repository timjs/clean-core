implementation module _DataArrayUnboxedInternal

import _DataArrayInternal

/// # Overloading

instance Array {#} Bool where
	array size el = code inline {
		create_array BOOL 0 1
	}
	_array size = code inline {
		create_array_ BOOL 0 1
	}
	select arr index = code inline {
		select BOOL 0 1
	}
	uselect arr index = code inline {
		push_a 0
		select BOOL 0 1
	}
	size arr = code inline {
		push_arraysize BOOL 0 1
	}
	usize arr = code inline {
		push_a 0
		push_arraysize BOOL 0 1
	}
	update arr index el = code inline {
		update BOOL 0 1
	}
	replace arr index el = code inline {
		replace BOOL 0 1
	}

instance Array {#} Char where
	array size el = code inline {
		create_array CHAR 0 1
	}
	_array size = code inline {
		create_array_ CHAR 0 1
	}
	select arr index = code inline {
		select CHAR 0 1
	}
	uselect arr index = code inline {
		push_a 0
		select CHAR 0 1
	}
    slice arr lower upper = code inline {
        .d 1 2 i i
            jsr sliceAC
        .o 1 0
    }
	size arr = code inline {
		push_arraysize CHAR 0 1
	}
	usize arr = code inline {
		push_a 0
		push_arraysize CHAR 0 1
	}
	update arr index el = code inline {
		update CHAR 0 1
	}
	replace arr index el = code inline {
		replace CHAR 0 1
	}
    concat arr1 arr2 = code inline {
        .d 2 0
            jsr catAC
        .o 1 0
    }
    compare arr1 arr2 = code inline {
        .d 2 0
            jsr cmpAC
        .o 0 1 i
    }

instance Array {#} Int where
	array size el = code inline {
		create_array INT 0 1
	}
	_array size = code inline {
		create_array_ INT 0 1
	}
	select arr index = code inline {
		select INT 0 1
	}
	uselect arr index = code inline {
		push_a 0
		select INT 0 1
	}
	size arr = code inline {
		push_arraysize INT 0 1
	}
	usize arr = code inline {
		push_a 0
		push_arraysize INT 0 1
	}
	update arr index el = code inline {
		update INT 0 1
	}
	replace arr index el = code inline {
		replace INT 0 1
	}

instance Array {#} Real where
	array size el = code inline {
		create_array REAL 0 1
	}
	_array size = code inline {
		create_array_ REAL 0 1
	}
	select arr index = code inline {
		select REAL 0 1
	}
	uselect arr index = code inline {
		push_a 0
		select REAL 0 1
	}
	size arr = code inline {
		push_arraysize REAL 0 1
	}
	usize arr = code inline {
		push_a 0
		push_arraysize REAL 0 1
	}
	update arr index el = code inline {
		update REAL 0 1
	}
	replace arr index el = code inline {
		replace REAL 0 1
	}

instance Array {#} {#.a} where
	array size el = code inline {
		create_array _ 1 0
	}
	_array size = code inline {
		create_array_ _ 1 0
	}
	select arr index = code inline {
		select _ 1 0
	}
	uselect arr index = code inline {
		push_a 0
		select _ 1 0
	}
	size arr = code inline {
		push_arraysize _ 1 0
	}
	usize arr = code inline {
		push_a 0
		push_arraysize _ 1 0
	}
	update arr index el = code inline {
		update _ 1 0
	}
	replace arr index el = code inline {
		replace _ 1 0
	}

instance Array {#} {!.a} where
	array size el = code inline {
		create_array _ 1 0
	}
	_array size = code inline {
		create_array_ _ 1 0
	}
	select arr index = code inline {
		select _ 1 0
	}
	uselect arr index = code inline {
		push_a 0
		select _ 1 0
	}
	size arr = code inline {
		push_arraysize _ 1 0
	}
	usize arr = code inline {
		push_a 0
		push_arraysize _ 1 0
	}
	update arr index el = code inline {
		update _ 1 0
	}
	replace arr index el = code inline {
		replace _ 1 0
	}

instance Array {#} {.a} where
	array size el = code inline {
		create_array _ 1 0
	}
	_array size = code inline {
		create_array_ _ 1 0
	}
	select arr index = code inline {
		select _ 1 0
	}
	uselect arr index = code inline {
		push_a 0
		select _ 1 0
	}
	size arr = code inline {
		push_arraysize _ 1 0
	}
	usize arr = code inline {
		push_a 0
		push_arraysize _ 1 0
	}
	update arr index el = code inline {
		update _ 1 0
	}
	replace arr index el = code inline {
		replace _ 1 0
	}

instance Array {#} a where
	array size el = code inline {
    		buildAC "Data.Array.Unboxed.array: should not be called for unknown basic type"
		.d 1 0
    		jsr print_string_
		.o 0 0
    		halt
	}
	_array size = code inline {
    		buildAC "Data.Array.Unboxed._array: should not be called for unknown basic type"
		.d 1 0
    		jsr print_string_
		.o 0 0
    		halt
	}
	select arr index = code inline {
    		buildAC "Data.Array.Unboxed.select: should not be called for unknown basic type"
		.d 1 0
    		jsr print_string_
		.o 0 0
    		halt
	}
	uselect arr index = code inline {
    		buildAC "Data.Array.Unboxed.uselect: should not be called for unknown basic type"
		.d 1 0
    		jsr print_string_
		.o 0 0
    		halt
	}
	size arr = code inline {
    		buildAC "Data.Array.Unboxed.size: should not be called for unknown basic type"
		.d 1 0
    		jsr print_string_
		.o 0 0
    		halt
	}
	usize arr = code inline {
    		buildAC "Data.Array.Unboxed.usize: should not be called for unknown basic type"
		.d 1 0
    		jsr print_string_
		.o 0 0
    		halt
	}
	update arr index el = code inline {
    		buildAC "Data.Array.Unboxed.update: should not be called for unknown basic type"
		.d 1 0
    		jsr print_string_
		.o 0 0
    		halt
	}
	replace arr index el = code inline {
    		buildAC "Data.Array.Unboxed.replace: should not be called for unknown basic type"
		.d 1 0
    		jsr print_string_
		.o 0 0
    		halt
	}
