device bar = {name = "bar", uuid = "skdjfslkjs"}

table table1 = [
   error 1024 "",
   error 4096 ""
]

# This label must be present
.start
	remove-all
	create bar
	load bar table1
	list "list"
	suspend bar
	resume bar
	
	begin "bar"
  	table bar "table"
  	info bar "info"
	end
	
	remove bar
	exit 17
