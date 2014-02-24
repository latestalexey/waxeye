;; Waxeye Parser Generator
;; www.waxeye.org
;; Copyright (C) 2008-2010 Orlando Hill
;; Licensed under the MIT license. See 'LICENSE' for details.

(module
    1C
  mzscheme
(require (lib "ast.ss" "waxeye")
         (lib "fa.ss" "waxeye")
         (only (lib "list.ss" "mzlib") filter)
         "code.scm" "dfa.scm" "gen.scm" "util.scm")
  
  
  (provide gen-1C)
  
  
  
  
  
  (define *1C-prefix* "")
  (define *1C-parser-name* "")
  (define *1C-source-name* ".1c")
  
  
  ;; have we generated a character class
  ;; used to avoid declaring variables we don't need
  (define *done-cc* #f)
  
  
  (define (gen-c-names)
    (set! *1C-prefix* (if *name-prefix*
                          (string-append (camel-case-lower *name-prefix*) "_")
                          ""))
    (set! *1C-parser-name* (string-append *1C-prefix* "parser"))
    (set! *1C-source-name* (string-append *1C-parser-name* ".1c")))
  
  
  (define (gen-1C grammar path)
    (indent-unit! 4)
    (gen-c-names)
    (let (
          (source-path (string-append path *1C-source-name*)))
      (dump-string (gen-parser grammar) source-path)
      (list  source-path)
      ))
  
  
  (define (c-comment lines)
    (comment-bookend "/*" " *" " */" lines))
  
  
  (define (c-header-comment)
    (if *file-header*
        (c-comment *file-header*)
        (c-comment *default-header*)))
  
  
  
  
  (define (gen-parser grammar)
    (let* ((automata (make-automata grammar))
           (non-terms (get-non-terms grammar)))
      (format "
var modes;
var astDefines;

function set_within_set(s, ch) 
	if s.rangeLen > 0 then
		for i = 0 to s.rangeLen-1 do
			if (ch >= s.min[i] and ch <= s.max[i]) then
				return true;
			endif;
		enddo;
	endif;
	
	if s.stringLen > 0 then
		for i = 0 to  s.stringLen-1 do
			if (ch = s.String[i]) then
				return true;
			endif;
		enddo;
	endif;
	return false;
endfunction

function mv(ip)
	ch = input_peek(ip);
	ip.input_pos=ip.input_pos+1;
	if (ch = chars.LF) then
		ip.line=ip.line+1 ;
		ip.column = 0;
		ip.last_cr = true;
	else 
		if (ch = chars.cr) then
			if (ip.last_cr = false) then
				ip.line=ip.line+1;
				ip.column = 0;
			endif;
		else 
			ip.column=ip.column+1;
		endif;
		ip.last_cr = false;
	endif;
	return ast_new(astDefines.AST_CHAR, ch);
	
endfunction

function input_peek(ip)
	return mid(ip.input,ip.input_pos,1);
endfunction

function input_eof(ip)
	return ip.input_pos > ip.input_size;
endfunction

function match_edge(ip, edge) 
	start_pos = ip.input_pos;
	start_line = ip.line;
	start_col = ip.column;
	start_cr = ip.last_cr;
	t = edge.t;
	
	if t.TransType = \"Wild\" then
		res = ?(input_eof(ip),update_error(ip), mv(ip));
		
	elsif t.TransType = \"Char\" then
		if input_eof(ip) then 
			return undefined;
		else
			if t.value <> input_peek(ip) then
				return  update_error(ip);
			else
				res = mv(ip);
			endif;
		endif;
	elsif t.TransType = \"Set\" then
		if input_eof(ip) then 
			return undefined;
		else
			if not set_within_set(t.value, input_peek(ip)) then
				return update_error(ip) 
			else 
				res = mv(ip);
			endif;
		endif;
		
	elsif t.TransType = \"FA\" then
		res = match_automaton(ip, t.value);
	else
		return undefined;
	endif;
	
	if (res = undefined) then
		return undefined;
	else 
		tran_res = match_state(ip, edge.s);
		if (tran_res <> undefined) then
			if (edge.v or res.type = astDefines.AST_EMPTY) then
				return tran_res;
			else 
				tran_res.add(res);
				return tran_res;
			endif;
		else 
			restore_pos(ip, start_pos, start_line, start_col, start_cr);
			return undefined;
		endif;
	endif;
endfunction


function match_edges(ip, edges, index) 
    if (index = edges.count()) then
        return undefined;
    else 
        res = match_edge(ip, edges[index]);
        if (res <> undefined) then
            return res;
        else 
            return match_edges(ip, edges,  index + 1);
        endif;
    endif;
endfunction


function match_state(ip, index) 
	automaton = ip.fa_stack[ip.fa_stack.UBound()];
	state = automaton.state[index];
	res = match_edges(ip, state.edges,  0);
	
	if (res <> undefined) then
		return res;
	else 
		if (state.match) then
			return new Array;
		else 
			return undefined;
		endif;
	endif;
endfunction


procedure vector_push(stack, value)
	
	stack.add(value);
	
endprocedure

procedure vector_pop(stack)
	stack.Delete(stack.UBound());
endprocedure




procedure restore_pos(ip, pos, line,col, last_cr) 
    ip.input_pos = pos;
    ip.line = line;
    ip.column = col;
    ip.last_cr = last_cr;
endprocedure


function update_error(ip) 
	if (ip.error_pos < ip.input_pos) then
		ip.error_pos = ip.input_pos;
		ip.error_line = ip.line;
		ip.error_col = ip.column;
		ip.error_nt = ip.fa_stack[ip.fa_stack.UBound()].type;
	endif;
	return undefined;
endfunction



function vector_reverse(vector)
	
	ret = new Array;
	for i = -vector.UBound() to 0 do
		ret.add(vector[-i]);
	enddo;
	return ret;
endfunction

function ast_tree_new(type,c,s,p)
	
	return new Structure(\"type,children,start,pos\",type,c,s,p);
	
endfunction
function match_automaton(ip, index) 
start_pos = ip.input_pos;

vkey = \"(\"+index+\",\"+start_pos + \")\";

cache_value = ip.cache[vkey];

if (cache_value <> undefined) then
	restore_pos(ip, cache_value.pos.input_pos, cache_value.pos.line,cache_value.pos.column, cache_value.pos.last_cr);
	return cache_value.result;
endif;

start_line = ip.line;
start_col = ip.column;
start_cr = ip.last_cr;
automaton = ip.automata[index];

vector_push(ip.fa_stack, automaton);
res = match_state(ip, 0);
vector_pop(ip.fa_stack);

value = undefined;

if (automaton.mode = modes.POS) then
	restore_pos(ip, start_pos, start_line, start_col, start_cr);
	if (res = undefined) then
		value = update_error(ip);
	else 
		value = ast_new(astDefines.AST_EMPTY, \"Empty\");
	endif;
elsif (automaton.mode = modes.NEG) then
	restore_pos(ip, start_pos, start_line, start_col, start_cr);
	if (res = undefined) then
		value = ast_new(astDefines.AST_EMPTY, \"Empty\");
	else 
		value = update_error(ip);
	endif;
elsif (automaton.mode = modes.VOID) then
	if (res = undefined) then
		value = update_error(ip);
	else 
		value = ast_new(astDefines.AST_EMPTY, \"Empty\");
	endif;
elsif (automaton.mode = modes.PRUNE) then
	if (res = undefined) then
		value = update_error(ip);
	else 
		if (res.size = 0) then
			value = ast_new(astDefines.AST_EMPTY, \"Empty\");
		elsif (res.size = 1) then
			value = res[0];
			
		else
			t = ast_tree_new(automaton.type, vector_reverse(res), start_pos, ip.input_pos);
			value = ast_new(astDefines.AST_TREE, t);
		endif;
	endif;
	
elsif (automaton.mode = modes.LEFT) then
	if (res = undefined) then
		value = update_error(ip);
	else 
		t = ast_tree_new(automaton.type, vector_reverse(res), start_pos, ip.input_pos);
		value = ast_new(astDefines.AST_TREE, t);
	endif;
	
else
	raise \"Unknown mode\";
endif;

	ip.cache[vkey] = new Structure(\"result,pos\",value,new structure(\"input_pos,line,column,last_cr\",ip.input_pos,ip.line,ip.column,ip.last_cr));
	return value;
endfunction



function ast_new(type, value)
	return new Structure(\"type,value\",type,value);
endfunction

function create_parse_error(ip) 
    e = new Structure;
	e.insert(\"pos\",ip.error_pos);
    e.insert(\"line\",ip.error_line);
    e.insert(\"col\",ip.error_col);
    e.insert(\"nt\",ip.error_nt);

    return ast_new(astDefines.AST_ERROR, e);
endfunction


function eof_check(ip, res) 
	if (res <> undefined) then
		if (ip.eof_check and ip.input_pos < ip.input_size) then
			return create_parse_error(ip);
		else 
			return res;
		endif
	else 
		return create_parse_error(ip);
	endif;
endfunction


function inner_parser_parse(ip) 
    res = eof_check(ip, match_automaton(ip, ip.start));
    return res;
endfunction


function inner_parser_init(start,
                       automata, num_automata,
                       eof_check, input,
                       line,  column, last_cr,
                        error_pos,  error_line,  error_col,  error_nt,
                        cache,  cache_contents,
                        fa_stack,  to_free)
	a = new structure;					
	a.insert(\"start\"			,start);
    a.insert(\"automata\"			,automata);
    a.insert(\"num_automata\"		,num_automata);
    a.insert(\"eof_check\"		,eof_check);
    a.insert(\"input\"			,input);
    a.insert(\"input_size\"			,StrLen(input));
    a.insert(\"input_pos\"			,1);
    a.insert(\"line\"				,line);
    a.insert(\"column\"			,column);
    a.insert(\"last_cr\"			,last_cr);
    a.insert(\"error_pos\"		,error_pos);
    a.insert(\"error_line\"		,error_line);
    a.insert(\"error_col\"		,error_col);
    a.insert(\"error_nt\"			,error_nt);
    a.insert(\"cache\"			,cache);
    a.insert(\"cache_contents\"	,cache_contents);
    a.insert(\"fa_stack\"			,fa_stack);
    a.insert(\"to_free\"			,to_free);
	return a;
endfunction


function parse(parser, input) export
	cache = new Map;
	cache_contents = new Array;
	to_free = new Array;
	fa_stack = new Array;
	inner_parser =inner_parser_init(parser.start, parser.automata,
		parser.automata.count(), parser.eof_check,
		input, 1, 0, false, 0, 1, 0,
		parser.automata[parser.start].type,
		cache, cache_contents, fa_stack, to_free);
	
	res = inner_parser_parse(inner_parser);
	return res;
endfunction



function wparser_new(start, automata, eof_check)
	return new Structure(\"start, automata, eof_check,num_automate\",start, automata, eof_check,automata.Count());
endfunction

function trans_init_wild()
	return new Structure(\"TransType,value\",\"Wild\",-1);
endfunction


function trans_init_FA(value)
	return new Structure(\"TransType,value\",\"FA\",value);
endfunction

function trans_init_set(value)
	return new Structure(\"TransType,value\",\"Set\",value);
endfunction


function trans_init_Char(value)
	return new Structure(\"TransType,value\",\"Char\",value);
endfunction

function fa_init(where,index, mode, name, states)
	where[index] = new Structure(\"mode,type,state\",mode,name,states);
endfunction

function edge_init(where,index, t, s, v)
	where[index] = new Structure(\"t,s,v\", t, s, v);
endfunction

function state_init(where,index, e,  m)
	where[index] = new Structure(\"edges,num_edges,match\", e, e.count(), m);
endfunction

function  init_set(single, num_single, min, max, num_range)
	return new Structure(\"String,Min,Max,stringLen,rangeLen\",single,min,max,num_single,num_range);
endfunction


// Parser - ~a

function ~a_new() export
~a
endfunction


modes = new structure(\"VOID,PRUNE,LEFT,POS,NEG\",0,1,2,3,4);
astDefines = new structure(\"AST_CHAR,AST_EMPTY,AST_ERROR,AST_TREE\",0,1,2,3);

"
              *1C-parser-name*
              *1C-parser-name*
              (indent
               ;; force the automaton to be generated first so we know
               ;; if the declarations for character classes are needed
               (let ((fas (mapi->s gen-fa (vector->list automata)))
                     (cc-decl ""))
                 (format "~astart = ~a;
~aeof_check = ~a;
~aautomata = new Array(~a);
// Parser states begin
~a
// Parser states end
~areturn wparser_new(start, automata, eof_check);"
                         (ind)
                         (number->string *start-index*)
                         (ind)
                         (bool->s *eof-check*)
                         (ind)
                         (number->string (vector-length automata))
                         fas
                         (ind)))))))
  
  
  (define (mapi fn l)
    (let ((i -1))
      (map (lambda (a)
             (set! i (+ i 1))
             (fn i a))
           l)))
  
  
  (define (mapi->s fn l)
    (string-concat (mapi fn l)))
  
  
  (define (gen-mode a)
    (let ((type (fa-type a)))
      (cond
        ((equal? type '&) "POS")
        ((equal? type '!) "NEG")
        (else
         (case (fa-mode a)
           ((voidArrow) "VOID")
           ((pruneArrow) "PRUNE")
           ((leftArrow) "LEFT"))))))
  
  
  (define (gen-fa i a)
    (let ((cc (let ((type (fa-type a)))
                (if (or (equal? type '&) (equal? type '!))
                    "undefined"
                    (format "\"~a\"" type))))
          (cc1  (fa-type a)))
      (format "// count =  ~a name ~a
~astates=new Array(~a);
~a~afa_init(automata,~a, modes.~a, ~a, states);\n\n"
              (vector-length (fa-states a))
              cc1
              (ind) (vector-length (fa-states a))
              (mapi->s gen-state (vector->list (fa-states a)))
              (ind)
              i
              (gen-mode a)
              cc
              )))
  
  
  (define (gen-state i s)
    (format "
~aedges=new Array(~a);
~a~astate_init(states,~a, edges,  ~a);\n"
            (ind) (if (= 0 (length (state-edges s))) "" (length (state-edges s)))
            (mapi->s gen-edge (state-edges s))
            (ind)
            i
            (bool->s (state-match s))))
  
  
  (define (gen-edge i e)
    (format "~a~aedge_init(edges,~a, trans, ~a, ~a);\n"
            (gen-trans (edge-t e))
            (ind)
            i
            (edge-s e)
            (bool->s (edge-v e))))
  
  
  (define (gen-trans t)
    (cond
      ((equal? t 'wild) (gen-wild-card-trans))
      ((integer? t) (gen-automaton-trans t))
      ((char? t) (gen-char-trans t))
      ((pair? t) (gen-char-class-trans t))))
  
  
  (define (gen-automaton-trans t)
    (format "
~atrans = trans_init_FA( ~a);\n"
            (ind) t ))
  
  
  (define (gen-char-trans t)
    (format "\n~atrans = trans_init_Char(~a);\n"
            (ind) (gen-char t) ))
  
  
  (define (gen-char-class-trans t)
    (let* ((single (filter char? t))
           (ranges (filter pair? t))
           (min (map car ranges))
           (max (map cdr ranges)))
      (set! *done-cc* #t)
      (format "~anum_single = ~a;
~anum_range = ~a;
~a
~a
~a
~aset = init_set(single, num_single, min, max, num_range);
~atrans = trans_init_Set( set);\n"
              (ind) (length single)
              (ind) (length ranges)
              (gen-char-list "single" "single" single)
              (gen-char-list "min" "range" min)
              (gen-char-list "max" "range" max)
              (ind) (ind))))
  
  
  (define (gen-char-list name size l)
    (define (ass-char i c)
      (format "\n~a~a.Add(~a);" (ind) name  (gen-char c)))
    (format "~a~a = new Array;
~a"
            (ind) name
            (if (null? l)
                ""
                (mapi->s ass-char l))))
  
  
  (define (gen-char t)
    (format "~a"
            (cond
              ((equal? t #\") "\"\"\"\"")
              ((equal? t #\linefeed) "Chars.LF")
              ((equal? t #\tab) "Chars.Tab")
              ((equal? t #\return) "Chars.CR")
              (else (format "\"~a\"" t)))))
  
  
  (define (gen-wild-card-trans)
    (format "~atrans = trans_init_wild();\n"
            (ind) ))
  
  
  )
