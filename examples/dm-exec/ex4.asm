device foo = {name = "foo"}

.start
	list "devs"
	jmp-fail start

.two
	create foo
	jmp two
	remove foo
	jmp start
	
	
