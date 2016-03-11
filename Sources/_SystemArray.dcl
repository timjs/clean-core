definition module _SystemArray

/// Legacy module used to satisfy the Clean Compiler.

/// # Array class

class Array .a e where
	select				:: !.(a .e) !Int	-> .e
	uselect				:: !u:(a e) !Int	-> *(e, !u:(a e))
	size				:: !.(a .e)			-> Int
	usize				:: !u:(a .e)		-> *(!Int, !u:(a .e))
	update				:: !*(a .e) !Int .e -> *(a .e)
	createArray			:: !Int e			-> *(a e)
	_createArray		:: !Int				-> *(a .e)
	replace				:: !*(a .e) !Int .e -> *(.e, !*(a .e))

/// # Instances

instance Array {!} a

instance Array {#} Int
instance Array {#} Char
instance Array {#} Real
instance Array {#} Bool

instance Array {#} {#.a}
instance Array {#} {!.a}
instance Array {#} {.a}

instance Array {#} a

//instance Array {#} File

instance Array {} a

/// # Changes

/// ## Renames of class methods

array size init :== createArray size init
unsafeArray size :== _createArray size

/// ## Additions to class

// selectRange :: !.(a .e) !(!Int,!Int) -> .(a .e)

/// ## Workaround for Strings

sliceString :: !{#Char} !(!Int,!Int) -> {#Char}
