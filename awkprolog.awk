################################################
#
# awkprolog: a Prolog interpreter in awk
# 
# Version 1.2 (2002)
#
# Willow Fung (prolog8@gmail.com)
#
#
################################################

 

BEGIN {
  ### Cell Structure #################
  null_type	= 0
  atom_type	= 1
  int_type 	= 2
  list_type 	= 3
  pred_type	= 4
  var_type	= 5
  dlist_type	= 6
  str_type	= 7
  unv_type	= 8
  
  struct_size 	= 5
  
  # Basic
  tag 		= 0
  
  # Atom
  atom_val 	= 1
  
  # Integer
  int_val 	= 1

  # String
  str_val 	= 1
  
  # List
  hd 		= 1
  tl 		= 2
  
  # Predicate
  pred 		= 1
  args 		= 2
  argn 		= 3
  
  # Variable
  var_nm	= 1
  var_val	= 2
  
  ################################
  
  init_prove()
  init_sys()
  initLexer()
}

function install_token() { return total_token++ }

function initLexer() {
  total_token = 0
  
  tok_unv = install_token()
  
  tok_atom = install_token()
  tok_str = install_token()
  tok_var = install_token()
  tok_num = install_token()
  
  tok_add = install_token()
  tok_sub = install_token()
  tok_mul = install_token()
  
  tok_eq = install_token()
  tok_lt = install_token()
  tok_gt = install_token()
  tok_le = install_token()
  tok_ge = install_token()
  tok_ne = install_token()
  
  tok_sql = install_token()
  tok_sqr = install_token()
  tok_lp = install_token()
  tok_rp = install_token()
  tok_com = install_token()
  tok_cut = install_token()
  tok_dot = install_token()
  tok_lst = install_token()
  tok_ru = install_token()
  tok_quo = install_token()
  tok_eof = install_token()
}

function init_sys() {
  nil = newCell(null_type)
  unv = newCell(unv_type)
  
  bu = install("builtIn")
  bu_rule = install(":-")
  bu_qry = install("?-")
  bu_cut = install("!")
  bu_cut_atom = install("cut")

  bu_op = install("op")
  yfx		= install("yfx")
  xfy		= install("xfy")
  xfx		= install("xfx")
  fy		= install("fy")
  fx		= install("fx")
  yf		= install("yf")
  xf		= install("xf")

  bu_addo = install("+")
  bu_subo = install("-")
  bu_mulo = install("*")
  
  bu_add = install("add")
  bu_sub = install("sub")
  bu_mul = install("mul")
  
  bu_univ = install("univ")
  bu_var = install("var")
  bu_nonvar = install("nonvar")
  bu_atomic = install("atomic")
  bu_number = install("number")
  bu_bound = install("bound")
  bu_compound = install("compound")
  bu_copyterm = install("copy_term")
  
  bu_assertz = install("assertz")
  bu_retract = install("retract")
  
  bu_eq = install("==")
  bu_gt = install(">")
  bu_lt = install("<")
  bu_le = install("=<")
  bu_ge = install(">=")
  bu_ne = install("=\\=")
  
  bu_is = install("is")
  bu_assign = install("=")
  bu_and = install("and")
  bu_or = install("or")
  bu_write = install("write")
  bu_writeln = install("writeln")
  bu_gput = install("gput")
  bu_gget = install("gget")
  bu_concat = install("concat")
  bu_chr = install("chr")
}

function init_prove() {
  max_heap 	= 30000
  heap 	= 0

  max_heap2 	= max_heap+10000
  heap2 	= max_heap
  
  heap_flag = 0

  sp 		= 0

  use_subt = 0
  bound_val = 0
  
  total_token = 0
  curr_token = 0
  cp = 0
  
  end_prove	= 0
  fail			= 0
  
  total_string	= 0
  total_clause	= 0
  total_op		= 0
  total_trail	= 0  
}

function newCell(theType, newAddr, tmp) {
	#heap_flag=0;
	
	if(heap_flag==0) tmp=heap; else tmp=heap2;

		newAddr = tmp
		if(theType==atom_type || theType==int_type || theType==str_type)
			tmp += 2
		else if(theType==var_type || theType==dlist_type || theType==list_type)
			tmp += 3
		else if(theType==pred_type)
			tmp += 4
		else
			tmp += struct_size
			
		if(heap_flag==0) {
			heap=tmp; 
			if(heap >= max_heap) error("Out of memory!")
		} else {
			heap2=tmp;
			if(heap2 >= max_heap2) error("Out of memory for rule space!")
		}
	
		cell[newAddr + tag] = theType
		return newAddr
}

function install(s, i) {
	for(i=0; i<total_string; i++) {
		if(s==str_arr[i]) return i
	}
	
	i = total_string++
	str_arr[i] = s
	return i
}

function install_operator(prec, typ, s, i, o) {
	o = install(s);
	for(i=0; i<total_op; i++) {
		if(o==op_arr[i] && op_typ[i]==typ) {
			op_prec[i] = prec;
			return;
		}
	}
	
	i = total_op++
	op_arr[i] = o;
	op_typ[i] = typ;
	op_prec[i] = prec;
	return;
}

function mkAtom(newAtom, i) {
	newAddr = newCell(atom_type)

	cell[newAddr + atom_val] = install(newAtom)
	return newAddr
}

function mkString(newStr, i) {
	newAddr = newCell(str_type)
	
	cell[newAddr + str_val] = install(newStr)
	return newAddr
}


function mkInteger(newInt, newAddr) {
	newAddr = newCell(int_type)
	cell[newAddr + int_val] = newInt
	return newAddr
}

function mkPred(PD, AG, arg_num) {
	newAddr = newCell(pred_type)

	cell[newAddr + pred] = install(PD)
	cell[newAddr + args] = AG
	cell[newAddr + argn] = arg_num
	return newAddr
}

function mkVar(newVarName, newIdx, newAddr) {
	newAddr = newCell(var_type)

	cell[newAddr + var_nm] = install(newVarName)
	install_local(newAddr)
	#cell[newAddr + var_idx] = newIdx
	return newAddr
}

function cons(a, b, newAddr) {
	newAddr = newCell(list_type)
	cell[newAddr + hd] = a
	cell[newAddr + tl] = b
	return newAddr
}

function append(L1, L2) {
	if(cell[L1+tag]==null_type) 
		return L2
	else
		return cons(cell[L1 + hd], append(cell[L1 + tl], L2))
}

function deref(tr, v) {
	if(cell[tr+tag]==var_type && cell[tr+var_val] != tr) {
		return deref(cell[tr+var_val])
	} else {
		return tr
	}
}

function trail(tr) {
	trail_stk[total_trail] = tr
	total_trail++
}

function unbind(stop_point, v) {
	while(total_trail > stop_point) {
		v = trail_stk[total_trail-1]
		cell[v+var_val] = v
		total_trail--
	}
}

function d_trail(i) {
    	print "Print Trail"
	for(i=0; i<total_trail; i+=3) {
		printTree(trail_stk[i])
		print ""
	}
	print "###"
}

function bind(theVar, theValue) {
	theVar = deref(theVar)
	cell[theVar+var_val] = theValue
	trail(theVar)
}

function bound(theVar, i, v) {
	v = deref(cell[theVar+var_val])
	if(cell[v+tag]==var_type) {
		return 0
	} else {
		bound_val = v
		return 1
	}
}

function printStrHeap() {
	for(i=0; i<total_string; i++) {
		print i, ":", str_arr[i]
	}
}

function unify(a, b) {
	a_t = cell[a+tag]
	b_t = cell[b+tag]
	
	#printf " UNIFY   "
	#printTree(a)
	#printf " ==  "
	#printTree(b)
	#printf "\n"
	
	if(a_t==b_t) {
		if(a_t==null_type) {
			return 1
		} else if(a_t==unv_type) {
			return 1
		} else if(a_t==var_type) {
			if(bound(a)) 
				return unify(bound_val, b)
			else if(bound(b))
				return unify(a, bound_val)
			#else if((cell[a+var_nm]==cell[b+var_nm]) && (cell[a+var_idx]==cell[b+var_idx]))
			else if(cell[a+var_val]==cell[b+var_val])
				return 1
			else {
				bind(a, b)
				return 1
			}
		} else if(a_t==atom_type) {
			if(cell[a+atom_val]==cell[b+atom_val]) return 1; else return 0
		} else if(a_t==str_type) {
			if(cell[a+str_val]==cell[b+str_val]) return 1; else return 0
		} else if(a_t==int_type) {
			if(cell[a+int_val]==cell[b+int_val]) return 1; else return 0
		} else if(a_t==pred_type) {
			if(cell[a+pred]==cell[b+pred]) 
				return unify(cell[a+args],cell[b+args])
			else
				return 0
		} else if(a_t==list_type) {
			if(unify(cell[a+hd],cell[b+hd])) return unify(cell[a+tl],cell[b+tl]); else return 0
		} else if(a_t==dlist_type) {
			if(unify(cell[a+hd],cell[b+hd])) return unify(cell[a+tl],cell[b+tl]); else return 0
		} else
			return 0
	} else if(a_t==var_type) {
		if(bound(a)) {
			v = bound_val
			return unify(v, b)
		} else {
			bind(a, b)
			return 1
		}
	} else if(b_t==var_type) {
		return unify(b, a)
	} else if(a_t==unv_type || b_t==unv_type) {
		return 1		
	} else {
		return 0
	}
}

function rename(tr, theSP, tmp, t2, t) {
	t = cell[tr+tag]
	if(t==null_type) {
		return nil
	} else if(t==unv_type) {
		return unv
	} else if(t==atom_type) {
		tmp = newCell(atom_type)
		cell[tmp+atom_val] = cell[tr+atom_val]
		return tmp
	} else if(t==int_type) {
		return mkInteger(cell[tr+int_val])
	} else if(t==str_type) {
		tmp = newCell(str_type)
		cell[tmp+str_val] = cell[tr+str_val]
		return tmp
	} else if(t==list_type) {
		return cons(rename(cell[tr+hd], theSP), 
				rename(cell[tr+tl], theSP))
	} else if(t==dlist_type) {
		tmp =  cons(rename(cell[tr+hd], theSP), 
				rename(cell[tr+tl], theSP))
		cell[tmp+tag] = dlist_type
		return tmp
	} else if(t==pred_type) {
		tmp = newCell(pred_type)
		
		if(cell[tr+pred]==bu_cut_atom) {
			cell[tmp+pred] = bu_cut_atom
			cell[tmp+args] = theSP
			return tmp
		}
		
		cell[tmp+pred] = cell[tr+pred]
		cell[tmp+args] = rename(cell[tr+args], theSP)
		cell[tmp+argn] = cell[tr+argn]
		return tmp
	} else if(t==var_type) {
		if(copy_term_flag==1 && bound(tr)) {
			tmp = bound_val
			return rename(tmp, 0)
		} else {
			tmp = newCell(var_type)
			cell[tmp+var_nm] = cell[tr+var_nm]
			install_local(tmp)
			return tmp
		}
	}
}

function copy_term(tr, tmp) {
	total_local = 0
	copy_term_flag = 1
	tmp = rename(tr, 0)
	copy_term_flag = 0
	return tmp
}

function install_local(tr, i) {
	i = 0
	while(i<total_local) {
		if(local_nm[i]==cell[tr+var_nm]) {
			cell[tr+var_val] = local_val[i]
			return
		}
		i++
	}
	
	cell[tr+var_val] = tr
	
	local_nm[total_local] = cell[tr+var_nm]
	local_val[total_local] = tr
	
	total_local++
}

function printTree(tr, tmp, v, t) {
	t = cell[tr+tag]
	if(t==null_type) {
		printf "[]"
	} else if(t==unv_type) {
		printf "_"
	} else if(t==atom_type) {
		printf "%s", str_arr[cell[tr+atom_val]]
	} else if(t==int_type) {
		printf "%d", cell[tr+int_val]
	} else if(t==str_type) {
		printf "%s", str_arr[cell[tr+str_val]]
	} else if(t==list_type) {
		printTree(cell[tr+hd])
		if(cell[tr+tl] !=nil) {
			printf ", "
			printTree(cell[tr+tl])
		}
	} else if(t==dlist_type) {
		printf "["
		
		tmp = deref(tr)
		while(1) {
			printTree(cell[tmp+hd])
			tmp = deref(cell[tmp+tl])
			
			if(cell[tmp+tag] == dlist_type) {
				printf ", "
			} else if(tmp == nil) {
				printf "]"
				break
			} else {
				printf "|"
				printTree(tmp)
				printf "]"
				break
			}
		}
		
	} else if(t==pred_type) {
		if(cell[tr+pred]==bu_cut_atom) {
			printf "!-%d", cell[tr+args]
			#break
		} else {
			infix_operator(str_arr[cell[tr+pred]]);
			if(found_op) {
				printTree(cell[cell[tr+args]+hd])
				if(cell[tr+pred]==bu_and)
					printf ", "
				else
					printf " %s ", str_arr[cell[tr+pred]]
				printTree(cell[cell[cell[tr+args]+tl]+hd])
			} else {
				# common
				printf "%s", str_arr[cell[tr+pred]]
				if(cell[tr+args]==nil) {
					printf ""
				} else {
					printf"("
					printTree(cell[tr+args])
					printf")"
				}
			}
		}
		
	} else if(t==var_type) {
		if(use_subt==1) {
			if(bound(tr)) {
				v = bound_val
				printTree(v)
			} else  {
				#printf "%s_%d", str_arr[cell[tr+var_nm]], cell[tr+var_idx]
				#printf "%s(_%d)", str_arr[cell[tr+var_nm]], cell[tr+var_val]
				#printf "%s_%d", str_arr[cell[deref(tr)+var_nm]], cell[deref(tr)+var_val]
				printf "_%d", cell[deref(tr)+var_val]
			}
		} else {
			#printf "%s[%d]", cell[tr+var_nm], cell[tr+var_idx]
			#printf "%s_%d", str_arr[cell[tr+var_nm]], cell[tr+var_idx]
			#printf "%s(_%d)", str_arr[cell[tr+var_nm]], cell[tr+var_val]
			printf "%s_%d", str_arr[cell[tr+var_nm]], cell[tr+var_val]
		}
	}
}

function evalCompare(opr, arg1, arg2, result) {
		#arg1 = cell[deref(arg1)+int_val]
		#arg2 = cell[deref(arg2)+int_val]
		
		arg1 = cell[evalA(arg1)+int_val]
		arg2 = cell[evalA(arg2)+int_val]

		if(opr==bu_gt) result = (arg1>arg2)
		else if(opr==bu_lt) result = (arg1<arg2)
		else if(opr==bu_ne) result = (arg1 != arg2)
		else if(opr==bu_eq) result = (arg1==arg2)
		else if(opr==bu_ge) result = (arg1 >= arg2)
		else if(opr==bu_le) result = (arg1 <= arg2)
		else {
			error("Error in operator!")
		}
		
		#print arg1, arg2, result

		if(result) 
			next_goals()
		else
			backtrack()
}

function evalA(tr, t, a1, a2, opr, tmp, r) {
	tr = deref(tr); t = cell[tr+tag]
	
	if(t==pred_type) {
		if(cell[tr+pred] >= bu_addo && cell[tr+pred] <= bu_mulo) {
			opr = cell[tr+pred]

			a1 = cell[evalA(cell[cell[tr+args]+hd])+int_val]
			a2 = cell[evalA(cell[cell[cell[tr+args]+tl]+hd])+int_val]

			if(opr==bu_addo) {
				return mkInteger(a1+a2)
			} else if(opr==bu_subo) {
				return mkInteger(a1-a2)
			} else if(opr==bu_mulo) {
				return mkInteger(a1*a2)
			} else {
				error("Error in operator!")
			}
		} else if(cell[tr+pred] == bu_chr) {
			a1 = deref( cell[cell[tr+args]+hd] )
			r = mkString( sprintf("%c", cell[a1+int_val]) )

			return r
		}
	} else if(t==int_type || t==str_type) {
		return tr
	} else {
		#return tr
		printTree(tr)
		error("Error in operand!")
	}
}

function is_predicate(varLhs, expRhs, r_val) {
	expRhs = deref(expRhs)
	if(cell[expRhs+tag]==pred_type) {
		if((cell[expRhs+pred] >= bu_addo && cell[expRhs+pred] <= bu_mulo) || cell[expRhs+pred]==bu_chr) {
			r_val = evalA(expRhs)
			unification(varLhs, r_val)
		} else {
			unification(varLhs, expRhs)
		}
	} else {
		unification(varLhs, expRhs)
	}
}

function conv_list_type(AG, typ, tmp1, tmp2) {
	AG = deref(AG)
	
	if(cell[AG+tag]==null_type) {
		return nil;
	} else {
		tmp1 = conv_list_type(cell[AG+tl], typ);
		tmp2 = cons(cell[AG+hd], tmp1);
		cell[tmp2 + tag] = typ;
		return tmp2;
	}
}

function pred_to_list(pred_addr, newAddr, tmp) {
	newAddr = mkAtom(str_arr[cell[pred_addr + pred]])
	tmp =  cons(newAddr, conv_list_type(cell[pred_addr + args], dlist_type))
	cell[tmp + tag] = dlist_type;
	return tmp;
}

function list_to_pred(list_addr, newAddr, listHead) {
	listHead = str_arr[cell[deref(cell[list_addr + hd])+atom_val]]
	tmp = conv_list_type(deref(cell[list_addr + tl]), list_type)
	return mkPred(listHead, conv_list_type(cell[list_addr + tl], list_type))
}

function pushFrame(GL, n) {
	frame[sp] = GL
	frame[sp+1] = n
	frame[sp+2] = total_trail
	frame[sp+3] = heap
	sp += 4
	
	return
}

function backtrack() {
	if(sp <= 0)  {
		fail = 1
	} else {
		sp -= 4
    		goals = frame[sp]
    		num = frame[sp+1]
    		unbind(frame[sp+2])
    		heap = frame[sp+3]
    	}
}

function getHeadTail() {
    	theHead = cell[goals+hd]
    	theTail = cell[goals+tl]
}

#******************************************************************
#	Prolog Parser
#******************************************************************
function error(msg) {
		print msg
		exit
}

function and_to_list(A, t1, t2) {
	if(cell[A+tag]==pred_type && cell[A+pred]==bu_and) {
		t1 = cell[cell[A+args]+hd];
		t2 = cell[cell[cell[A+args]+tl]+hd];
		return append(and_to_list(t1), and_to_list(t2));
	} else if(A != nil) {
		return cons(A, nil);
	} else {
		return nil;
	}
}

function findFromTail(pd, i) {
	i = total_clause-1
	while(i > 0) {
		if(db[i]==pd) return i+1
		i--
	}
	return total_clause
}

function appendRule(pd, lhs, rhs, i, ii) {
	ii = findFromTail(pd)
	for(i=total_clause; i > ii; i--) {
		db[i] = db[i-1]
		db_lhs[i] = db_lhs[i-1]
		db_rhs[i] = db_rhs[i-1]
	}
	db[ii] = pd
	db_lhs[ii] = lhs
	db_rhs[ii] = rhs
	total_clause++
}

function assertz(t, lhs, rhs, pd, tmp, i) {
	heap_flag = 1;
	
	if(cell[t+tag]==pred_type && cell[t+pred]==bu_rule) {
		lhs = cell[cell[t+args]+hd]
		rhs = and_to_list( cell[cell[cell[t+args]+tl]+hd] )
		pd = cell[lhs+pred]
	} else {
		lhs = t
		rhs = nil
		pd = cell[t+pred]
	}
	
	appendRule(pd, lhs, rhs)
	
	heap_flag = 0;
}

function retract(t, i, t_num, bak_sp, bak_env) {
	#printTree(t)
	
	t_num = 0
	while( findClause(t, t_num) ) {

		bak_sp = sp
		bak_env = total_trail
    		
    		total_local=0
    		copy_term_flag=0
    		cl_lhs = rename(curr_clause_lhs, 0)
    		
    		if(unify(t, cl_lhs)) {
    			#
    			total_clause--
    			i = clause_index
    			while(i < total_clause) {
    				db[i] = db[i+1]
    				db_lhs[i] = db_lhs[i+1]
    				db_rhs[i] = db_rhs[i+1]
    				i++
    			}
    		
    			#unbind(bak_env)
    			#sp = bak_sp
    			
    			return
    		}
    		
    		unbind(bak_env)
    		sp = bak_sp
    		t_num++
	}
	
	return
	
	i = 0
	while(i<total_clause) {
		the_rule = cell[tmp+hd];
		
		bak_sp = sp
		bak_env_sp = env_sp
		bak_idx = rename_idx
		
    		total_local=0
    		copy_term_flag=0
    		cl_lhs = rename(cell[the_rule+ru_lhs], sp)
    		
    		#printTree(t)
    		#printf " <==> "
    		#printTree(cl_lhs)
    		#print ""
    		if(unify(t, cl_lhs)) {
			if(tmp==db_hd) {
				db_hd = cell[db_hd+tl];
			} else {
				cell[prev_rule+tl] = cell[tmp+tl];
			}
			
			#removeIndex(t)
			
			#d_rule()
			return;
		}
		
		
		sp = bak_sp
		env_sp = bak_env_sp
		rename_idx = bak_idx
		
		prev_rule = tmp;
		tmp = cell[tmp+tl];
	}
}

function d_info() {
	print "heap = ", heap
	print "heap2 = ", heap2
	print "string heap = ", total_string
	print "sp = ", sp
}

function ParseStream(lhs, rhs, L, a1, a2, a3, t, tmp) {
	cp = 1
	lineno=1
	nextToken()
	
 while(getToken() != tok_eof) {
  	t = read_term();
	if(cell[t+tag]==pred_type && cell[t+pred]==bu_op) {
		tmp = cell[t+args]; 
		a1 = cell[tmp+hd]; tmp = cell[tmp+tl];
		a2 = cell[tmp+hd]; tmp = cell[tmp+tl];
		a3 = cell[tmp+hd];
		
		install_operator(cell[a1+int_val], cell[a2+atom_val], str_arr[cell[a3+atom_val]]);
	} else if(cell[t+tag]==pred_type && cell[t+pred]==bu_qry) {
		tmp = cell[cell[t+args]+hd];

		query = and_to_list(tmp);
		goals = query;
		#set_local_var(query)
		
		prove();
		
		heap = last_heap
		end_prove = 0
		fail = 0
		sp = 0
		total_trail = 0
	} else {
		#printTree(t)
		#print ""
		#set_local_var(t)
		assertz(t);
		
		last_heap = heap
	}
 } 
 
 #d_info()
 
}

function findClause(H, cl_num, i, PD, the_rule, tmp, tmp2, tmp3) {
	contain_next_clause = 0;
	
	if(cell[H+tag]==pred_type) {
		PD = cell[H+pred];
	} else if(cell[H+tag]==atom_type) {
		PD = cell[H+atom_val];
	} else if(cell[H+tag]==var_type) {
		# deref(H) may is an atom
		PD = cell[deref(H)+pred];
	}
	#printTree(H)
	#print cell[H+tag], PD
	###############################
	i = 0
	tmp = cl_num
	
	while(i<total_clause) {
		# find same name
		if(db[i]==PD) {
		
			# find n'th rule
			while(i<total_clause && tmp>0 && db[i]==PD) {
				i++
				tmp--
			}
			
			if(i<total_clause && tmp==0 && db[i]==PD) {
				curr_clause_lhs = db_lhs[i];
				curr_clause_rhs = db_rhs[i];
				
				clause_index = i;
				
				if((i+1) < total_clause && db[i+1]==PD) {
					next_clause_lhs = db_lhs[i+1];
					next_clause_rhs = db_rhs[i+1];
					contain_next_clause = 1
				}
				return 1
			}
		}
		i++
	}
	return 0
}

#*********************************************
function isalpha(c) { return ((c >= "a" && c <= "z") || (c >= "A" && c <= "Z")) }
function isdigit(c) {return (c >= "0" && c <= "9") }
function isgraph(c) {return (c=="+" || c=="-" || c=="*" || c==":"  || c==";" || c=="=" || c==">" || c=="<" || 
	c=="!" || c=="." || c=="/" || c=="\\" || c=="^" || c=="&" || c=="~" || c=="$" || c=="@" || c=="?") }
#*********************************************

function matchs(S) { return substr(ss, cp, length(S))==S; }
function getChar() { return substr(ss, cp, 1); }

function isEmpty() { return (tok==tok_eof); }
function getToken() { return tok; }

function nextToken() {
	ms = ""
	if(getChar()=="\0") {
		tok = tok_eof;
		ms=""
	} else if(getChar()=="%") {
		while(getChar()!="\0" && getChar()!="\n") cp++;
		nextToken()
	} else if(getChar()=="\n") {
		cp++
		lineno++
		nextToken()
	} else if(getChar()=="\t" || getChar()==" ") {
		while(getChar()=="\t" || getChar()==" ") cp++;
		nextToken();
	} else if(getChar() >= "a" && getChar() <= "z") {
			tok = tok_atom;

			while(isalpha(getChar()) || isdigit(getChar()) || getChar()=="_") {
				ms = ms getChar()
				cp++;
			}
	} else if(getChar() >= "A" && getChar() <= "Z" || getChar()=="_") {
			tok = tok_var;
			
			while(isalpha(getChar()) || isdigit(getChar()) || getChar()=="_") {
				ms = ms getChar()
				cp++;
			}
			
			if(ms=="_") {
				tok = tok_unv;
			}
	} else if(isdigit(getChar())) {
		tok = tok_num;
		while(isdigit(getChar())) {
			ms = ms getChar()
			cp++;
		}

		mi = int(ms);
	} else if(matchs("\"")) {
		tok = tok_str;
		cp++
		while(getChar()!="\"") {
			ms = ms getChar()
			cp++;
		}
		cp++
	} else if(matchs("'")) {
		tok = tok_str;
		cp++
		while(getChar()!="'") {
			ms = ms getChar()
			cp++;
		}
		cp++
	} else if(matchs("!")) { tok = tok_atom; cp += 1; ms = "!";
	} else if(matchs(",")) { tok = tok_atom; cp += 1; ms = ",";
	} else if(matchs("(")) { tok = tok_lp; cp += 1; ms = "(";
	} else if(matchs(")")) { tok = tok_rp; cp += 1; ms = ")";
	} else if(matchs("[")) { tok = tok_sql; cp += 1; ms = "[";
	} else if(matchs("]")) { tok = tok_sqr; cp += 1; ms = "]";
	} else if(matchs("|")) { tok = tok_lst; cp += 1; ms = "|";
	} else if(isgraph(getChar())) { 
			tok = tok_atom;

			while(isgraph(getChar())) {
				ms = ms getChar()
				cp++;
			}
	} else {
		error("Error in eat token!")
		tok = tok_eof;
		cp++
	}
}

#******************************************************************
#	Prolog Tokenier
#******************************************************************
{
#############################################
	ss = ss "\n" $0
#############################################
}


#******************************************************************
function checkGoals() {
	if(fail) {
		#d_info()
		print "fail"
		end_prove = 1
	} else if(goals==nil)	{
		#d_info()
		print "yes"
		use_subt=1
		#printTree(query)
		print ""
	
		use_subt=0
		#if(more) backtrack(); else exit;
		end_prove = 1
	}
}

function unification(T1, T2) {
	if(unify(T1, T2)) {
		next_goals()
	} else {
		backtrack()
	}
}

function next_goals() {
	goals = theTail
	num = 0
}

function prove(a1, a2, a3, tmp1, tmp2, c_op) {

num = 0

for(;;) {
	use_subt=1
	#printf "> "
	#printTree(goals)
	#print ""
	
	checkGoals()
	if(end_prove) break
	
	getHeadTail()
	if(cell[theHead+tag]==var_type) {
		theHead = deref(theHead)
	}
	
	theArgNum = cell[theHead+argn]
	
	if(cell[theHead+tag]==pred_type && cell[theHead+pred]==bu_cut_atom) {
		sp = cell[theHead+args]
		goals = theTail
		num = 0
	} else if(cell[theHead+tag]==pred_type && cell[theHead+pred]==bu) {
		tmp1 = cell[theHead+args]
		c_op = cell[cell[tmp1+hd]+atom_val];
		
		if(c_op==bu_is || c_op==bu_assign) {
			tmp1 = cell[tmp1+tl]; a1 = cell[tmp1+hd];
			tmp1 = cell[tmp1+tl]; a2 = cell[tmp1+hd];
			
			is_predicate(a1, a2)
		} else if(c_op >= bu_eq && c_op <= bu_ne) {
			tmp1 = cell[tmp1+tl]; a1 = cell[tmp1+hd];
			tmp1 = cell[tmp1+tl]; a2 = cell[tmp1+hd];

			evalCompare(c_op, a1, a2)
		} else if(c_op==bu_univ) {
			tmp1 = cell[tmp1+tl]; a1 = cell[tmp1+hd];
			tmp1 = cell[tmp1+tl]; a2 = cell[tmp1+hd];
			
			a1 = deref(a1)
			a2 = deref(a2)
			
			if(cell[a1+tag]==pred_type) {
				unification(a2, pred_to_list(a1));
			} else if(cell[a2+tag]==dlist_type) {
				unification(a1, list_to_pred(a2));
			}
		} else if(c_op==bu_var) {
			tmp1 = cell[tmp1+tl]; a1 = cell[tmp1+hd];
			if(! bound(a1)) {
				next_goals()
			} else {
				backtrack()
			}
		} else if(c_op==bu_atomic) {
			a1 = cell[deref(cell[cell[tmp1+tl]+hd])+tag];
			
			if(a1==atom_type || a1==int_type || a1==str_type) {
				next_goals()
			} else {
				backtrack()
			}
		} else if(c_op==bu_number) {
			a1 = cell[deref(cell[cell[tmp1+tl]+hd])+tag];
			
			if(a1==int_type) {
				next_goals()
			} else {
				backtrack()
			}
		} else if(c_op==bu_bound || c_op==bu_nonvar) {
			if(cell[ deref(cell[cell[tmp1+tl]+hd]) + tag ] != var_type) {
				next_goals()
			} else {
				backtrack()
			}
		} else if(c_op==bu_compound) {
			tmp1 = cell[tmp1+tl]; 
			a1 = deref(cell[tmp1+hd]);
			if(cell[a1+tag]==pred_type && cell[a1+pred] != bu_cut_atom) {
				next_goals()
			} else {
				backtrack()
			}
		} else if(c_op==bu_assertz) {
			tmp1 = cell[tmp1+tl]; a1 = cell[tmp1+hd];
			
			heap_flag = 1
			tmp2 = copy_term(deref(a1))
			heap_flag = 0
			
			assertz(tmp2)
			
			next_goals()
		} else if(c_op==bu_retract) {
			tmp1 = cell[tmp1+tl]; a1 = cell[tmp1+hd];
			
			retract(deref(a1))
			next_goals()
		} else if(c_op==bu_copyterm) {
			tmp1 = cell[tmp1+tl]; a1 = cell[tmp1+hd];
			tmp1 = cell[tmp1+tl]; a2 = cell[tmp1+hd];
			
			#total_local=0
			#tmp2 = rename(deref(a1), parent_sp)
			#set_local_var(tmp2)
			tmp2 = copy_term(deref(a1))
    			unification(tmp2, a2)
		}
	} else if(cell[theHead+tag]==pred_type && cell[theHead+pred]==bu_concat) {
		tmp1 = cell[theHead+args]
		a1 = cell[tmp1+hd];	tmp1 = cell[tmp1+tl]; 
		a2 = cell[tmp1+hd];	tmp1 = cell[tmp1+tl]; 
		a3 = cell[tmp1+hd];
		
		a1 = deref(a1)
		a2 = deref(a2)
		unification( mkString(str_arr[cell[a1+str_val]] str_arr[cell[a2+str_val]]) , a3)
		
	} else if(cell[theHead+tag]==pred_type && (cell[theHead+pred]==bu_writeln || cell[theHead+pred]==bu_write)) {
		a1 = cell[cell[theHead+args]+hd];
		
		use_subt=1
		printTree(a1);
		use_subt=0
			
		if(cell[theHead+pred]==bu_writeln) printf "\n";
		
		next_goals()
	} else if(findClause(theHead, num)) {
		### Backup parent SP for cut
		parent_sp = sp

  		### Get alternative choice point
    		if(contain_next_clause) {
			pushFrame(goals, num+1)
    		}
    		#printFrame()

    		### Rewrite the goal list
    		# curr_clause from 'findClause'
    		
    		total_local=0
    		copy_term_flag=0
    		cl_lhs = rename(curr_clause_lhs, parent_sp)
    		
    		#total_local=0
		#set_local(cl_lhs)

    		
    		if(unify(theHead, cl_lhs)) {
    			cl_rhs = rename(curr_clause_rhs, parent_sp)
    			#set_local(cl_rhs)
    			
    			goals = append(cl_rhs, theTail)
    			num = 0
    		} else {
    			backtrack()
    		}
	} else {
		### Backtracking
		backtrack()
	}
} # for-loop
}

#******************************************************************
#	Prolog Parser II
#******************************************************************
function prefix_operator(s) {
	for(i=0; i<total_op; i++) {
		if(str_arr[op_arr[i]]==s) {
			#print str_arr[op_arr[i]], op_typ[i]
			if(op_typ[i]==fy) {
				OpPrec = op_prec[i];
				RAP = OpPrec;
				found_op = 1;
				return;
			} else if(op_typ[i]==fx) {
				OpPrec = op_prec[i];
				RAP = OpPrec - 1;
				found_op = 1;
				return;
			}
		}
	}
	found_op = 0;
}

function postfix_operator(s) {
	for(i=0; i<total_op; i++) {
		if(str_arr[op_arr[i]]==s) {
			if(op_typ[i]==yf) {
				OpPrec = op_prec[i];
				LAP = OpPrec;
				found_op = 1;
				return;
			} else if(op_typ[i]==xf) {
				OpPrec = op_prec[i];
				LAP = OpPrec - 1;
				found_op = 1;
				return;
			}
		}
	}
	found_op = 0;
}

function infix_operator(s, i) {
	for(i=0; i<total_op; i++) {
		if(str_arr[op_arr[i]]==s) {
			if(op_typ[i]==xfy) {
				OpPrec = op_prec[i];
				LAP = OpPrec - 1;
				RAP = OpPrec;
				found_op = 1;
				return;
			} else if(op_typ[i]==yfx) {
				OpPrec = op_prec[i];
				LAP = OpPrec;
				RAP = OpPrec - 1;
				found_op = 1;
				return;
			} else if(op_typ[i]==xfx) {
				OpPrec = op_prec[i];
				LAP = OpPrec - 1;
				RAP = OpPrec - 1;
				found_op = 1;
				return;
			}
		}
	}
	found_op = 0;
}

function term(P, r, tOpPrec, tArgPrec, tmp1, ta) {

	# Number
	if(getToken()==tok_num) {
		r = mkInteger(mi);
		
		nextToken();
		r = rest_term(r, 0, P);
		return r;
	}
	
	# Variable
	if(getToken()==tok_var) {
		r = mkVar(ms);
		
		nextToken();
		r = rest_term(r, 0, P);
		return r;
	}

	# String
	if(getToken()==tok_str) {
		r = mkString(ms);
		
		nextToken();
		r = rest_term(r, 0, P);
		return r;
	}

	# Univer
	if(getToken()==tok_unv) {
		r = unv;
		
		nextToken();
		r = rest_term(r, 0, P);
		return r;
	}

	# Cut
	if(getToken()==tok_atom && ms=="!") {
		r = mkPred("cut", cons(sp, nil), 2);

		nextToken();
		r = rest_term(r, 0, P);
		return r;
	}

	# prefix OR basic prefix atom
	if(getToken()==tok_atom) {
		
		prefix_operator(ms);
		if(found_op) {
			tmp1 = ms
			
			tOpPrec = OpPrec;
			tArgPrec = RAP;
			
			nextToken();
			
			if(getToken()==tok_rp || getToken()==tok_sqr || (getToken()==tok_atom && (ms=="." || ms==","))) {
				r = mkAtom(tmp1);
				return r
			}
			
			r = term(tArgPrec);
			if(P >= tOpPrec) {
				r = mkPred(tmp1, cons(r, nil), 1);
				r = rest_term(r, tOpPrec, P);
				return r;
			}
		} 
	}
	
	ta = ms;
	
	# Compound term in functional notation OR basic atom
	if(getToken()==tok_atom) {
		nextToken();
		
		if(getToken()==tok_lp) {
			nextToken();
			
			r = term(999);
			
			tmp1 = arg_list();
			
			r = mkPred(ta, cons(r, tmp1), 2);
			r = rest_term(r, 0, P);
			return r;
		} else {
			# Otherwise, Atom
			r = mkAtom(ta);
			r = rest_term(r, 0, P);
			return r;
		}
		
	}

	
	# Compound term in list notation
	if(getToken()==tok_sql) {
		nextToken();
		
		# [] empty list
		if(getToken()==tok_sqr) {
			nextToken();
			return nil;
		}
		
		r = term(999);
		
		if(r==nil) return nil;
		
		tmp1 = items();
		
		r = cons(r, tmp1);
		cell[r+tag] = dlist_type;
		
		r = rest_term(r, 0, P);
		return r;
	}

	# Bracketed term
	if(getToken()==tok_lp) {
		nextToken();
		
		r = term(1200);
		
		if(getToken()==tok_rp) {
			nextToken();
			
			r = rest_term(r, 0, P);
			return r;
		}
	}
	
	print ta
	error("Read term error!");	
}

function items(r) {
	if(getToken()==tok_atom && ms== ",") {
		nextToken();
		
		r = term(999);
		r = cons(r, items());
		cell[r+tag] = dlist_type;
		
		return r;
	}

	if(getToken()==tok_lst) {
		nextToken();
		
		r = term(999);
		
		if(getToken()==tok_sqr) {
			nextToken();
			return r;
		}
		
		error("parse list error!")
	}
	
	if(getToken()==tok_sqr) {
		nextToken();
		return nil;
	}
}

function arg_list(r) {
	if(getToken()==tok_atom && ms== ",") {
		nextToken();
		
		r = term(999);
		r = cons(r, arg_list());
		return r;
	}
	
	if(getToken()==tok_rp) {
		nextToken();
		return nil;
	}
}

function rest_term(T, LeftPrec, P, r, tOpPrec, tArgPrec, t1, t2, ta) {
	if(getToken()==tok_atom) {
		ta = ms;
		
		# infix
		infix_operator(ta);
		
		if(found_op) {
			tOpPrec = OpPrec;
			t1 = LAP;
			t2 = RAP;
			
			#print P, tOpPrec, LeftPrec, t1
			
			if(P >= tOpPrec && LeftPrec <= t1) {
				nextToken();
				r = term(t2);
				r = mkPred(ta, cons(T, cons(r, nil)), 2);
				r = rest_term(r, tOpPrec, P);
				
				return r;
			}
		}
		
		# postfix
		postfix_operator(ta);
		if(found_op) {
			tOpPrec = OpPrec;
			t1 = LAP;
			
			if(P >= tOpPrec && LeftPrec <= t1) {
				r = mkPred(ta, cons(T, nil), 1);
				nextToken();
				r = rest_term(r, tOpPrec, P);
				return r;
			}
		} 
		
		# comma 'and'
		if(ta=="," && P >= 1000 && LeftPrec < 1000) {
			nextToken();
			r = term(1000);
			r = mkPred("and", cons(T, cons(r, nil)), 2);
			r = rest_term(r, 1000, P);
			return r;
		}
	
	}
	
	# otherwise
	return T;
	
}

function read_term(r, ta) {
	#####
	total_local = 0
	heap_flag = 1
	#####

	r = term(1200);

	#####
	heap_flag = 0
	#####

	if(getToken()==tok_atom) {
		ta = ms;
		
		# end dot
		if(ta==".") {
			nextToken();
			return r;
		}
	}
	
	print "Error in line# : ", lineno
	error("Expect '.'");
	
}

function d_rule(tmp, the_rule) {
	tmp = db_hd;
	while(tmp != db_last) {
		the_rule = cell[tmp+hd];
		
		printTree(cell[the_rule+ru_lhs])
		printf " :- "
		printTree(cell[the_rule+ru_rhs])
		print "."
		
		tmp = cell[tmp+tl];
	}
}

function install_built() {
 install_operator(200, fy, "-");
 install_operator(300, xfy, "^");
 install_operator(400, yfx, "*");
 install_operator(500, yfx, "or");
 install_operator(500, yfx, "-");
 install_operator(500, yfx, "+");
 install_operator(400, yfx, "*");
 install_operator(700, xfx, "is");
 install_operator(700, xfx, "=");
 install_operator(700, xfx, "=..");
 install_operator(900, fx, "~");
 install_operator(1000, xfy, "and");
 install_operator(1100, xfx, ";");
 install_operator(1200, xfx, ":-");
 install_operator(1200, fx, "?-");
 
 install_operator(700, xfx, "..");
 install_operator(1050, xfy, "->");
 install_operator(500, xfx, ">");
 install_operator(500, xfx, "<");
 install_operator(500, xfx, "=<");
 install_operator(500, xfx, ">=");
 
 install_operator(500, yfx, "/");
 
 install_operator(600, xfx, "=\\=");

ssb = ""
ssb = ssb "var(X) :- builtIn(var, X)."
ssb = ssb "var(X) :- builtIn(var, X)."
ssb = ssb "nonvar(X) :- builtIn(nonvar, X)."
ssb = ssb "atomic(X) :- builtIn(atomic, X)."
ssb = ssb "number(X) :- builtIn(number, X)."
ssb = ssb "bound(X) :- builtIn(bound, X)."
ssb = ssb "compound(X) :- builtIn(compound, X)."
ssb = ssb "assertz(X) :- builtIn(assertz, X)."
ssb = ssb "retract(X) :- builtIn(retract, X)."
ssb = ssb "copy_term(X, Y) :- builtIn(copy_term, X, Y)."
ssb = ssb "copy_term2(X, Y) :- assertz(cp_term(X)), retract(cp_term(Y))."

ssb = ssb "X =.. Y :- builtIn(univ, X, Y)."
ssb = ssb "X is Y :- builtIn(is, X, Y)."
ssb = ssb "X = Y :- builtIn( = , X, Y)."
ssb = ssb "X > Y :- builtIn( > , X, Y)."
ssb = ssb "X < Y :- builtIn( < , X, Y)."
ssb = ssb "X =< Y :- builtIn( =< , X, Y)."
ssb = ssb "X >= Y :- builtIn( >= , X, Y)."
ssb = ssb "X =\\= Y :- builtIn( =\\= , X, Y)."

ssb = ssb "X ; _ :- X."
ssb = ssb "_ ; Y :- Y."

ssb = ssb "and(X, Y) :- X, Y."

}

END {
 
 install_built()
 
 ss = ssb ss "\0"

 ParseStream()


#ss = "grandfather(sam, WHO)."
#ss ="append(A, B, [a, b, c, d]), write(A), write(', '), writeln(B), fail."
#ss = "expr(T, [lparn, num(3), add, num(12), rparn, mul, num(13)], R)."
#ss = "fac(4, R)."
#ss = "program(T, [begin, ok, sep, id(abc), equ, expr, sep, id(qwe), equ, expr, sep, ok, sep, end], R)."
#ss = "if_stm(T, [if, cond_expr, then, begin, ok, sep, ok, sep, end, else, begin, id(a), equ, expr, sep, ok, sep, end], R)."
#ss = "for_stm(T, [for, id(i), equ, expr, to, expr, begin, ok, sep, ok, sep, end], R)."
#ss = "program(T, [ok, sep, id(abc), equ, expr, sep, if, cond_expr, then, begin, ok, sep, ok, sep, end, else, begin, id(a), equ, expr, sep, ok, sep, end, sep, id(qwe), equ, expr, sep, ok, sep], R)."
#ss="param(T, [lp, id(abc), com, id(qwe), com, id(asd), rp], R)."
#ss="param(T, [lp, id(abc), rp], R)."
#ss = "func_list(T, [func, id(fac), lp, id(a), com, id(b), rp, begin, ok, sep, end, sep, func, id(main), lp, id(a), com, id(b), rp, begin, ok, sep, end, sep], R)."
#ss = "stm_list(T, [lineno(1), id(n), equ, num(10), sep, lineno(2), id(b), equ, num(1), sep, lineno(3), while, lp, id(a), s_lt, id(n), rp, lineno(4), begin, lineno(5), id(a), equ, id(a), add, num(2), mul, id(b), sep, lineno(6), end, sep, lineno(7), while, lp, id(b), s_lt, id(n), rp, lineno(8), begin, lineno(9), id(b), equ, id(b), add, num(1), sep, lineno(10), end, sep], R, 1, L)."
#ss = "cg(ss(assign_stm(id(n), num(10)), ss(assign_stm(id(b), num(1)), ss(while_stm(cmp(s_lt, id(a), id(n)), ss(assign_stm(id(a), add(id(a), mul(num(2), id(b)))), void)), ss(while_stm(cmp(s_lt, id(b), id(n)), ss(assign_stm(id(b), add(id(b), num(1))), void)), void)))), L)."
#ss = "hanoi(3)."
#ss = "max(12, 10)."
ss ="sort([45, 30, 21, 23, 20, 8, 7, 32, 1], S)."
ss ="sort([6, 45, 30, 21, 23, 20, 8, 32, 1], S)."
#ss = "stm_list(T, [lineno(1), id(n), equ, num(10), sep, lineno(2), id(b), equ, num(1), sep, lineno(3), while, lp, id(a), s_lt, id(n), rp, lineno(4), begin, lineno(5), id(a), equ, id(a), add, num(2), mul, id(b), sep, lineno(6), end, sep, lineno(7), while, lp, id(b), s_lt, id(n), rp, lineno(8), begin, lineno(9), id(b), equ, id(b), add, num(1), sep, lineno(10), end, sep], R)."
#ss = "add_sym(c, [c, b, a], L)."
#ss = "relocate([push(a), pop(b), push(n)], [sym(n, 10), sym(a, 11), sym(b, 12)], L)."
#ss = "func_list(T, [lineno(1), func, id(fac), lp, id(n), rp, lineno(2), begin, lineno(3), id(a), equ, num(1), sep, lineno(4), while, lp, id(a), s_lt, id(n), rp, lineno(5), begin, lineno(6), id(a), equ, id(a), add, num(1), sep, lineno(7), end, sep, lineno(8), end, sep, lineno(9), lineno(10), func, id(main), lp, rp, lineno(11), begin, lineno(12), id(a), equ, num(10), sep, lineno(13), end, sep], R)."
#ss="gen(func_list(func(fac, n, ss(assign_stm(id(a), num(1)), ss(while_stm(cmp(s_lt, id(a), id(n)), ss(assign_stm(id(a), add(id(a), num(1))), void)), void))), func(main, void, ss(assign_stm(id(a), num(10)), void))))."
#ss = "func_list(T, [lineno(1), func, id(fac), lp, id(n), rp, lineno(2), begin, lineno(3), id(a), equ, num(1), sep, lineno(4), while, lp, id(a), s_lt, id(n), rp, lineno(5), begin, lineno(6), id(a), equ, id(a), add, num(1), sep, lineno(7), end, sep, lineno(8), end, sep, lineno(9), lineno(10), func, id(main), lp, rp, lineno(11), begin, lineno(12), id(a), equ, num(10), sep, lineno(13), end, sep], R)."
#ss="eval(func_list(func(fac, [n, m], ss(assign_stm(id(a), num(1)), ss(while_stm(cmp(s_lt, id(a), id(n)), ss(assign_stm(id(a), add(id(a), num(1))), void)), void))), func(main, [], ss(assign_stm(id(a), num(10)), void)))  )"
#ss="tst('qwe', Q)."

#ss = "eq(john, Z), and(father(X, Y), father(Y, Z))."
#ss = "Z is 2 + 3 * 5, writeln(Z)."

#ss = "like(tom, mary) =.. T, writeln(T)."
#ss = "T =.. [like, tom, mary], writeln(T)."
#ss = "like(tom, mary) =.. [like, tom, mary]."
#ss = "Q is fai, like(tom, mary) =.. T1, append(T1, [new_ele, Q], T2), T =.. T2, writeln(T)."

#ss = "X is Y, var(X), writeln('OK is var')."
#ss = "X is qwe, nonvar(X), writeln('OK is nonvar')."
#ss = "atomic(X), writeln('OK is atomic')."

#ss = "A is f(X, X, Y), copy_term(A, Z), writeln(A), writeln(Z)."

#ss = "each(X, g(X), writeln(X))."
#ss = "m2(Z, 1 .. 5), writeln(Z), fail."
#ss = "X is 1, Y is 1, X is Y -> writeln('True') ; writeln('False')."
#ss = "p_case(3)."
#ss = "if 1 then writeln('True') else writeln('False')."

#ss = "X is like(peter, mary), assertz(X), like(peter, Who), writeln(Who)."
#ss = "assertz(like(peter, mary)), assertz(like(peter, susan)), like(peter, Who), writeln(Who), fail."
#ss = "retract(like(john, may)), like(john, may)."
#ss = "assertz(like(peter, mary)), assertz(like(peter, susan)), retract(like(peter, mary)), like(peter, Who), writeln(Who), fail."
#ss = "A is tom, B is sally, copy_term2(like(A, B), Z), writeln(Z)."
#ss = "writeln(abc(A, B, B)), copy_term2(like(A, B, B), Z), writeln(Z)."

#ss = "template(S), sol(S), writeln(S)."
#ss = "3 =\\= 2, writeln('OK')."

#ss = "member(X, [1, 2, 3]), writeln(X), fail."

#############################################
}
