--open Format

module Error = struct

--exception Exit of int

data info = FI string int int | UNKNOWN
--type 'a withinfo = {i: info; v: 'a}

dummyinfo = UNKNOWN
createInfo f l c = FI f l c

errf f = do print_flush() 
  	    open_vbox 0
  	    open_hvbox 0 
	    f()  
	    print_cut() 
	    close_box()  
	    print_newline()
            raise Exit 1

printInfo = case
  function
    FI(f,l,c) ->
      print_string f; 
      print_string ":"; 
      print_int l; print_string "."; 
      print_int c; print_string ":"
  | UNKNOWN ->
      print_string "<Unknown file and line>: "
