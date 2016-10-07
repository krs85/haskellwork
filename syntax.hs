--open Format
--open Support.Error
--open Support.Pervasive 

--Datatypes

data term =
	TmTrue info
	| TmFalse info
	| TmIf info term term term
	| TmZero info
	| TmSucc info term
	| TmPred info term
	| TmIsZero info term
	

--info is a defined in lexer.mll in a let expression
--why does lexer.mll have a .mll extension?

-- not sure where Eval is defined 
data command = 
	| Eval info term

--fi = file info
extractFi t =   case t of
	 	TmTrue fi	-> fi
		TmFalse fi	-> fi
		TmIf fi _ _ _ 	-> fi
		TmZero fi	-> fi
		TmSucc fi _	-> fi
		TmPred fi _	-> fi
		TmIsZero fi _ 	-> fi 

obox0() = open_hvbox 0
obox() = open_hvbox 2
cbox() = close_box()
break() = print_break 0 0

printtm_Term outer t = 	case t of 
    			TmIf fi t1 t2 t3 -> do
       					    obox0()
       					    putStrLn "if "
       					    printtm_Term false t1
       					    print_space()
       					    putStrLn "then "
       				            printtm_Term false t2
       					    print_space()
       					    putStrLn "else "
       					    printtm_Term false t3
       				  	    cbox()
  			 t 		 -> printtm_AppTerm outer t

			where printtm_AppTerm outer t = case t of 
							TmPred _ t1  	-> do
       									   putStrLn "pred "
									   printtm_ATerm false t1
  							TmIsZero _ t1 	-> do
									   putStrLn "iszero "
									   printtm_ATerm false t1													   t -> printtm_ATerm outer t

							where printtm_ATerm outer t = case t of
    										      TmTrue _    -> putStrLn "true"
  										      TmFalse _   -> putStrLn "false"
  										      TmZero fi   -> putStrLn "0"
										      TmSucc _ t1 -> let f n t = case t of 
													     TmZero _   -> putStrLn string_of_int n
												             TmSucc _ s -> f (n+1) s
												             _          -> do
															   putStrLn "(succ "
															   printtm_ATerm false t1
														           putStrLn ")"
												     in f 1 t1
										      t 	  -> do 
												     putStrLn "("			
												     printtm_Term outer t
												     putStrLn ")"							     

printtm t = printtm_Term true t

