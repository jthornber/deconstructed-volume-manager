# dm-exec files begin with declarations of devices, and tables
device bar = {name = "bar", uuid = "LKJDSFLS"}
device foo = {name = "foo"}

table t1 = [
  error 1024 "",
  error 4096 ""
]

# Then the special label .start annouces the begining of the
# instructions
.start
	remove-all
	begin-object "bar"
	create bar
	jmp-fail .create-fail

	load bar t1
	jmp-fail .bar-fail

	resume bar
	jmp-fail .bar-fail

	table bar "table"
	jmp-fail .bar-fail

	lit "result" "success"
	jmp .bar-out

.bar-fail
	remove bar
.create-fail
	lit "result" "fail"
	end-object
	exit 2

.bar-out
	end-object

	# 'foo' doesn't exist, so we expect this to fail.
	begin-object "foo"
	table foo "table"
	jmp-fail .foo-fail

	lit "result" "success (unexpected)"
	end-object
	exit 3

.foo-fail
	lit "result" "fail (expected)"
	end-object
	exit 0
