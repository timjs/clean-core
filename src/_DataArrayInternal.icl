implementation module _DataArrayInternal

/// # Overloading

instance Array {} a where
	array size el = code inline {
		create_array _ 1 0
	}
	_array size = code inline {
		create_array_ _ 1 0
	}

	size arr = code inline {
		push_arraysize _ 1 0
	}
	usize arr = code inline {
		push_a 0
		push_arraysize _ 1 0
	}

	select arr index = code inline {
		select _ 1 0
		jsr_eval 0
	}
	uselect arr index = code inline {
		push_a 0
		select _ 1 0
	}

    // slice arr lower upper = undefined
    // uslice arr lower upper = undefined

	update arr index el = code inline {
		update _ 1 0
	}

	replace arr index el = code inline {
		replace _ 1 0
	}
