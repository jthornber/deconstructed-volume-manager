device foo = {name = "foo"}
table t1 = [
	linear 1024 "/dev/loop/0 0",
	linear 4096 "/dev/loop/1 1024"
]

.start
	create foo
	jmp-fail .label0

	load foo t1
	jmp-fail .label2
	jmp .label3

.label2
	remove foo

.label3
	jmp .label0

.label0


