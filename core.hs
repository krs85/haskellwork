--open Format
--open Syntax
--open Support.Error
--open Support.Pervasive


--exception NoRuleApplies

isnumericval t = case t of
	TmZero _ -> true
	TmSucc _ t1 -> isnumericval t1
	         _  -> false
	
isval t = case t of
	TmTrue _  -> true
	TmFalse _ -> true
	t -> true | isnumericval t -- ??
	_ -> false

eval1 t = case t of 
    TmIf(_ TmTrue _ t2 t3) -> t2
    TmIf(_ TmFalse _ t2 t3) -> t3
    TmIf fi t1 t2 t3  -> let t1' = eval1 t1 in TmIf fi t1' t2 t3
    TmSucc fi t1  -> let t1' = eval1 t1 in TmSucc fi t1'
    TmPred(_ TmZero _ ) -> TmZero dummyinfo
    TmPred(_ TmSucc(_ nv1))  -> nv1 | isnumericval nv1
    TmPred fi t1  -> let t1' = eval1 t1 in TmPred fi  t1'
    TmIsZero(_ TmZero _ ) -> TmTrue dummyinfo
    TmIsZero(_ TmSucc(_ nv1)) -> TmFalse dummyinfo | isnumericval nv1
    TmIsZero fi t1 -> let t1' = eval1 t1 in TmIsZero fi  t1'
    -- _ -> raise NoRuleApplies

let eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t
