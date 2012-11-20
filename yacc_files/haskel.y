%token
 vocurly         
 vccurly         
 SIMPLEQUOTE         

 VARID                    
 CONID          
 VARSYM         
 CONSYM         
 QVARID         
 QCONID         
 QVARSYM        
 QCONSYM        
 PREFIXQVARSYM  
 PREFIXQCONSYM  

 IPDUPVARID                   

 CHAR           
 STRING         
 INTEGER        
 RATIONAL       

 PRIMCHAR       
 PRIMSTRING     
 PRIMINTEGER    
 PRIMWORD       
 PRIMFLOAT      
 PRIMDOUBLE     

 DOCNEXT        
 DOCPREV        
 DOCNAMED       
 DOCSECTION     

TH_ID_SPLICE         
TH_TY_QUOTE           
TH_QUASIQUOTE   
TH_QQUASIQUOTE  

%%

identifier
        : qvar                          
        | qcon                          
        | qvarop                        
        | qconop                        
    | '(' "->" ')'      
;

module 
        : maybedocheader "module" modid maybemodwarning maybeexports "where" body

        | body2
;

maybedocheader
        : moduleheader            
        |              
;

missing_module_keyword
        :                            
;

maybemodwarning
    : "{-# DEPRECATED" strings "#-}" 
    | "{-# WARNING" strings "#-}"    
    |                    
;

body   
        :  '{'            top '}'               
        |      vocurly    top close  
;

body2
        :  '{' top '}'                          
        |  missing_module_keyword top close     
;

top    
        : importdecls                           
        | importdecls ';' cvtopdecls            
        | cvtopdecls                            
;

cvtopdecls
        : topdecls                              
;

header 
        : maybedocheader "module" modid maybemodwarning maybeexports "where" header_body

        | header_body2
;

header_body
        :  '{'            importdecls           
        |      vocurly    importdecls           
;

header_body2
        :  '{' importdecls                      
        |  missing_module_keyword importdecls   
;

maybeexports
        :  '(' exportlist ')'                   
        |                            
;

exportlist
        : expdoclist ',' expdoclist             
        | exportlist1                           
;

exportlist1
        : expdoclist export expdoclist ',' exportlist  
        | expdoclist export expdoclist                 
        | expdoclist                                   
;

expdoclist
        : exp_doc expdoclist                           
        |                                   
;

exp_doc
        : docsection    
        | docnamed       
        | docnext              
;

export 
        : qcname_ext export_subspec     
        |  "module" modid               
;

export_subspec
        :                    
        | '(' ".." ')'                  
        | '(' ')'                       
        | '(' qcnames ')'               
;

qcnames
        :  qcnames ',' qcname_ext       
        |  qcname_ext                   
;

qcname_ext

        :  qcname                       
        |  "type" qcname                
;

qcname 
        :  qvar                         
        |  qcon                         
;

importdecls
        : importdecls ';' importdecl            
        | importdecls ';'                       
        | importdecl                            
        |                            
;

importdecl
        : "import" maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec 
;

maybe_src
        : "{-# SOURCE" "#-}"                    
        |                            
;

maybe_safe
        : "safe"                                
        |                            
;

maybe_pkg
        : STRING                                
        |                            
;

optqualified
        : "qualified"                           
        |                            
;

maybeas
        : "as" modid                            
        |                            
;

maybeimpspec
        : impspec                               
        |                            
;

impspec
        :  '(' exportlist ')'                   
        |  "hiding" '(' exportlist ')'          
;

prec   
        :            
        | INTEGER               
;

infix  
        : "infix"                               
        | "infixl"                              
        | "infixr"                              
;

ops    
        : ops ',' op                            
        | op                                    
;

topdecls
        : topdecls ';' topdecl                  
        | topdecls ';'                          
        | topdecl                               
;

topdecl
        : cl_decl                               
        | ty_decl                               
        | inst_decl                             
        | stand_alone_deriving                  
        | "default" '(' comma_types0 ')'        
        | "foreign" fdecl                       
        | "{-# DEPRECATED" deprecations "#-}"   
        | "{-# WARNING" warnings "#-}"          
        | "{-# RULES" rules "#-}"               
        | "{-# VECTORISE_SCALAR" qvar "#-}"     
        | "{-# VECTORISE" qvar '=' exp "#-}"    
        | "{-# NOVECTORISE" qvar "#-}"          
        | "{-# VECTORISE" "type" gtycon "#-}"     

        | "{-# VECTORISE_SCALAR" "type" gtycon "#-}"     

        | "{-# VECTORISE" "type" gtycon '=' gtycon "#-}"     

        | "{-# VECTORISE_SCALAR" "type" gtycon '=' gtycon "#-}"     

        | "{-# VECTORISE" "class" gtycon "#-}"  
        | "{-# VECTORISE_SCALAR" "instance" type "#-}"     

        | annotation 
        | decl                                  

        | infixexp                               
;

cl_decl
        : "class" tycl_hdr fds where_cls        
;

ty_decl

        : "type" type '=' ctypedoc

        | "type" "family" type opt_kind_sig 

        | data_or_newtype capi_ctype tycl_hdr constrs deriving

        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig 
                 gadt_constrlist
                 deriving

        | "data" "family" type opt_kind_sig
;

inst_decl
        : "instance" inst_type where_inst

        | "type" "instance" type '=' ctype

        | data_or_newtype "instance" tycl_hdr constrs deriving

        | data_or_newtype "instance" tycl_hdr opt_kind_sig 
                 gadt_constrlist
                 deriving
;

at_decl_cls

        : "type" type opt_kind_sig

        | "data" type opt_kind_sig

        | "type" type '=' ctype
;

at_decl_inst

        : "type" type '=' ctype

        | data_or_newtype capi_ctype tycl_hdr constrs deriving

        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig 
                 gadt_constrlist
                 deriving
;

data_or_newtype
        : "data"        
        | "newtype"     
;

opt_kind_sig
        :                               
        | "::" kind                     
;

tycl_hdr
        : context "=>" type             
        | type                          

capi_ctype : "{-# CTYPE" STRING STRING "#-}" 
           | "{-# CTYPE"        STRING "#-}" 
           |                                 
;

stand_alone_deriving
        : "deriving" "instance" inst_type 

decl_cls 
decl_cls  : at_decl_cls                 
          | decl                        

          | "default" infixexp "::" sigtypedoc
;

decls_cls
          : decls_cls ';' decl_cls      
          | decls_cls ';'               
          | decl_cls                    
          |                  
;

decllist_cls

        : '{'         decls_cls '}'     
        |     vocurly decls_cls close   
;

where_cls

        : "where" decllist_cls          
        |                    

decl_inst 
decl_inst  : at_decl_inst               
           | decl                       
;

decls_inst
           : decls_inst ';' decl_inst   
           | decls_inst ';'             
           | decl_inst                  
           |                 
;

decllist_inst 

        : '{'         decls_inst '}'    
        |     vocurly decls_inst close  
;

where_inst

        : "where" decllist_inst         
        |                    
;

decls  
        : decls ';' decl                
        | decls ';'                     
        | decl                          
        |                    
;

decllist
        : '{'            decls '}'      
        |     vocurly    decls close    
;

binds  

        : decllist                      
        | '{'            dbinds '}'     
        |     vocurly    dbinds close   
;

wherebinds

        : "where" binds                 
        |                    
;

rules  
        :  rules ';' rule                       
        |  rules ';'                            
        |  rule                                 
        |                            
;

rule   
        : STRING activation rule_forall infixexp '=' exp
;

activation
        :                            
        | explicit_activation                   
;

explicit_activation
        : '[' INTEGER ']'               
        | '[' '~' INTEGER ']'           
;

rule_forall
        : "forall" rule_var_list '.'            
        |                            
;

rule_var_list
        : rule_var                              
        | rule_var rule_var_list                
;

rule_var
        : varid                                 
        | '(' varid "::" ctype ')'              
;

warnings
        : warnings ';' warning          
        | warnings ';'                  
        | warning                               
        |                            
;

warning
        : namelist strings
;

deprecations
        : deprecations ';' deprecation          
        | deprecations ';'                      
        | deprecation                           
        |                            
;

deprecation
        : namelist strings
;

strings
    : STRING 
    | '[' stringlist ']' 
;

stringlist
    : stringlist ',' STRING 
    | STRING                
;

annotation
    : "{-# ANN" name_var aexp "#-}"      
    | "{-# ANN" "type" tycon aexp "#-}"  
    | "{-# ANN" "module" aexp "#-}"      

fdecl : "import" callconv safety fspec

      | "import" callconv        fspec          

      | "export" callconv fspec
;

callconv
          : "stdcall"                   
          | "ccall"                     
          | "capi"                      
          | "prim"                      
;

safety
        : "unsafe"                      
        | "safe"                        
        | "interruptible"               
;

fspec
       : STRING var "::" sigtypedoc     
       |        var "::" sigtypedoc     
;

opt_sig
        :                    
        | "::" sigtype                  
;

opt_asig
        :                    
        | "::" atype                    
;

sigtype

        : ctype                         
;

sigtypedoc
        : ctypedoc                      
;

sig_vars
         : sig_vars ',' var             
         | var                          
;

sigtypes1
        : sigtype                       
        | sigtype ',' sigtypes1         
;

infixtype
        : btype qtyconop type         
        | btype tyvarop  type    
;

strict_mark
        : '!'                           
        | "{-# UNPACK" "#-}" '!'        
        | "{-# NOUNPACK" "#-}" '!'      
;

ctype  
        : "forall" tv_bndrs '.' ctype   
        | context "=>" ctype            

        | ipvar "::" type               
        | type                          
;

ctypedoc
        : "forall" tv_bndrs '.' ctypedoc        
        | context "=>" ctypedoc         

        | ipvar "::" type               
        | typedoc                       
;

context
        : btype '~'      btype          
        | btype                         
;

type
        : btype                         
        | btype qtyconop type           
        | btype tyvarop  type           
        | btype "->"     ctype          
        | btype '~'      btype          

        | btype SIMPLEQUOTE qconop type     
        | btype SIMPLEQUOTE varop  type     
;

typedoc
        : btype                          
        | btype docprev                  
        | btype qtyconop type            
        | btype qtyconop type docprev    
        | btype tyvarop  type            
        | btype tyvarop  type docprev    
        | btype "->"     ctypedoc        
        | btype docprev "->" ctypedoc    
        | btype '~'      btype           

        | btype SIMPLEQUOTE qconop type     
        | btype SIMPLEQUOTE varop  type     
;

btype
        : btype atype                   
        | atype                         
;

atype
        : ntgtycon                             
        | tyvar                                
        | strict_mark atype                
        | '{' fielddecls '}'              
        | '(' ')'                        
        | '(' ctype ',' comma_types1 ')' 
        | "(#" "#)"                             
        | "(#" comma_types1 "#)"         
        | '[' ctype ']'                  
        | "[:" ctype ":]"                
        | '(' ctype ')'                  
        | '(' ctype "::" kind ')'        
        | quasiquote                     
        | "$(" exp ')'                   
        | TH_ID_SPLICE                   

        | SIMPLEQUOTE qconid                          
        | SIMPLEQUOTE  '(' ')'                        
        | SIMPLEQUOTE  '(' ctype ',' comma_types1 ')' 
        | SIMPLEQUOTE  '[' comma_types0 ']'           
        | '[' ctype ',' comma_types1 ']'              
        | INTEGER            
        | STRING             
;

inst_type
        : sigtype                       
;

inst_types1
        : inst_type                     
        | inst_type ',' inst_types1     
;

comma_types0 
        : comma_types1                  
        |                    
;

comma_types1   
        : ctype                         
        | ctype  ',' comma_types1       
;

tv_bndrs
         : tv_bndr tv_bndrs             
         |                   
;

tv_bndr
        : tyvar                         
        | '(' tyvar "::" kind ')'       
;

fds
        :                    
        | '|' fds1                      
;

fds1
        : fds1 ',' fd                   
        | fd                            
;

fd
        : varids0 "->" varids0          
;

varids0
        :                    
        | varids0 tyvar                 
;

kind
        : bkind                  
        | bkind "->" kind        
;

bkind
        : akind                  
        | bkind akind            
;

akind
        : '*'                    
        | '(' kind ')'           
        | pkind                  
        | tyvar                  
;

pkind
        : qtycon                          
        | '(' ')'                         
        | '(' kind ',' comma_kinds1 ')'   
        | '[' kind ']'                    
;

comma_kinds1
        : kind                          
        | kind  ',' comma_kinds1        
;

gadt_constrlist
        : "where" '{'        gadt_constrs '}'      
        | "where" vocurly    gadt_constrs close    
        |                               
;

gadt_constrs
        : gadt_constr ';' gadt_constrs  
        | gadt_constr                   
        |                    
;

gadt_constr
        : con_list "::" sigtype

        | oqtycon '{' fielddecls '}' "::" sigtype
;

constrs
        : maybe_docnext '=' constrs1    
;

constrs1
        : constrs1 maybe_docnext '|' maybe_docprev constr 
        | constr                                          
;

constr
        : maybe_docnext forall context "=>" constr_stuff maybe_docprev  

        | maybe_docnext forall constr_stuff maybe_docprev
;

forall
        : "forall" tv_bndrs '.'         
        |                    
;

constr_stuff

        : btype                         
        | btype conop btype             
;

fielddecls
        :      
        | fielddecls1     
;

fielddecls1
        : fielddecl maybe_docnext ',' maybe_docprev fielddecls1

        | fielddecl   
;

fielddecl
        : maybe_docnext sig_vars "::" ctype maybe_docprev      
;

deriving
        :                            
        | "deriving" qtycon                      
        | "deriving" '(' ')'                    
        | "deriving" '(' inst_types1 ')'        
;

docdecl
        : docdecld 
;

docdecld
        : docnext                               
        | docprev                               
        | docnamed                              
        | docsection                            
;

decl   
        : sigdecl               

        | '!' aexp rhs          

        | infixexp opt_sig rhs  
        | docdecl               
;

rhs    
        : '=' exp wherebinds    
        | gdrhs wherebinds      
;

gdrhs
        : gdrhs gdrh            
        | gdrh                  
;

gdrh
        : '|' guardquals '=' exp        
;

sigdecl
        : 

          infixexp "::" sigtypedoc

        | var ',' sig_vars "::" sigtypedoc

        | infix prec ops        
        | "{-# INLINE" activation qvar "#-}"        

        | "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"

        | "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"

        | "{-# SPECIALISE" "instance" inst_type "#-}"
;

quasiquote
        : TH_QUASIQUOTE   
        | TH_QQUASIQUOTE  
;

exp  
        : infixexp "::" sigtype         
        | infixexp "-<" exp             
        | infixexp ">-" exp             
        | infixexp "-<<" exp            
        | infixexp ">>-" exp            
        | infixexp                      
;

infixexp
        : exp10                         
        | infixexp qop exp10            
;

exp10
        : "\\" apat apats opt_asig "->" exp     

        | "let" binds "in" exp                  
        | "\\" "lcase" altslist

        | "if" exp optSemi "then" exp optSemi "else" exp

        | "if" gdpats                   
        | "case" exp "of" altslist              
        | '-' fexp                              

        | "do" stmtlist                 
        | "mdo" stmtlist                

        | scc_annot exp                         
        | hpc_annot exp                         

        | "proc" aexp "->" exp  

        | "{-# CORE" STRING "#-}" exp           

        | fexp                                  
;

optSemi
        : ';'         
        |  
;

scc_annot
        : "_scc_" STRING                        
        | "{-# SCC" STRING "#-}"                
        | "{-# SCC" VARID  "#-}"                
;

hpc_annot
        : "{-# GENERATED" STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER "#-}"
;

fexp   
        : fexp aexp                             
        | aexp                                  
;

aexp   
        : qvar '@' aexp                 
        | '~' aexp                      
        | aexp1                 
;

aexp1  
        : aexp1 '{' fbinds '}'  
        | aexp2                 
;

aexp2  
        : ipvar                         
        | qcname                        
        | literal                       

        | INTEGER                       
        | RATIONAL                      

        | '(' texp ')'                  
        | '(' tup_exprs ')'             

        | "(#" texp "#)"                
        | "(#" tup_exprs "#)"           

        | '[' list ']'                  
        | "[:" parr ":]"                
        | '_'                           

        | TH_ID_SPLICE           
        | "$(" exp ')'                         

        | SIMPLEQUOTE  qvar     
        | SIMPLEQUOTE  qcon     
        | TH_TY_QUOTE tyvar     
        | TH_TY_QUOTE gtycon    
        | "[|" exp "|]"                                
        | "[t|" ctype "|]"                             
        | "[p|" infixexp "|]"   
        | "[d|" cvtopbody "|]"  
        | quasiquote            

        | "(|" aexp2 cmdargs "|)"       
;

cmdargs
        : cmdargs acmd                  
        |                    
;

acmd   
        : aexp2                 
;

cvtopbody
        :  '{'            cvtopdecls0 '}'               
        |      vocurly    cvtopdecls0 close             
;

cvtopdecls0
        :            
        | cvtopdecls            
;

texp
        : exp                           

        | infixexp qop        
        | qopm infixexp       

        | exp "->" texp   
;

tup_exprs
           : texp commas_tup_tail  
           | commas tup_tail       

commas_tup_tail : commas tup_tail  
;

tup_tail
          : texp commas_tup_tail        
          | texp                        
          |                  
;

list
        : texp                  
        | lexps                 
        | texp ".."             
        | texp ',' exp ".."     
        | texp ".." exp         
        | texp ',' exp ".." exp 
        | texp '|' flattenedpquals      
;

lexps
        : lexps ',' texp                
        | texp ',' texp                 
;

flattenedpquals
    : pquals   
;

pquals
    : squals '|' pquals     
    | squals                
;

squals

    : squals ',' transformqual               
    | squals ',' qual                        
    | transformqual                          
    | qual                                   
;

transformqual

    : "then" exp                           
    | "then" exp "by" exp                  
    | "then" "group" "using" exp           
    | "then" "group" "by" exp "using" exp  
;

parr
        :                               
        | texp                          
        | lexps                         
        | texp ".." exp                 
        | texp ',' exp ".." exp         
        | texp '|' flattenedpquals      
;

guardquals
    : guardquals1           
;

guardquals1
    : guardquals1 ',' qual  
    | qual                  
;

altslist
        : '{'            alts '}'       
        |     vocurly    alts  close    
;

alts   
        : alts1                         
        | ';' alts                      
;

alts1  
        : alts1 ';' alt                 
        | alts1 ';'                     
        | alt                           
;

alt    
        : pat opt_sig alt_rhs           
;

alt_rhs
        : ralt wherebinds               
;

ralt
        : "->" exp                      
        | gdpats                        
;

gdpats
        : gdpats gdpat                  
        | gdpat                         
;

gdpat  
        : '|' guardquals "->" exp               

pat    
pat     :  exp                  
        | '!' aexp              

apat  
apat    : aexp                  
        | '!' aexp              
;

apats 
        : apat apats            
        |            
;

stmtlist
        : '{'           stmts '}'       
        |     vocurly   stmts close     
;

stmts
        : stmt stmts_help               
        | ';' stmts                     
        |                    
;

stmts_help
        : ';' stmts                     
        |                    
;

maybe_stmt
        : stmt                          
        |                  
;

stmt 
        : qual                              
        | "rec" stmtlist                
;

qual 
    : pat "<-" exp                      
    | exp                                   
    | "let" binds                       
;

fbinds 
        : fbinds1                       
        |                    
;

fbinds1
        : fbind ',' fbinds1              
        | fbind                         
        | ".."                          
;

fbind  
        : qvar '=' texp 

        | qvar          
;

dbinds 
        : dbinds ';' dbind              
        | dbinds ';'                    
        | dbind                         

dbind   : ipvar '=' exp                 
;

ipvar  
        : IPDUPVARID            

namelist : name_var              
         | name_var ',' namelist 

name_var : var 
         | con 
;

qcon   
        : qconid                
        | '(' qconsym ')'       
        | sysdcon               
;

con    
        : conid                 
        | '(' consym ')'        
        | sysdcon               

con_list : con                  
         | con ',' con_list     
;

sysdcon
        : '(' ')'               
        | '(' commas ')'        
        | "(#" "#)"             
        | "(#" commas "#)"      
        | '[' ']'               
;

conop
        : consym                  
        | '`' conid '`'         
;

qconop
        : qconsym               
        | '`' qconid '`'        
;

gtycon
        : ntgtycon                      
        | '(' ')'                       
        | "(#" "#)"                     
;

ntgtycon
        : oqtycon                       
        | '(' commas ')'                
        | "(#" commas "#)"              
        | '(' "->" ')'                  
        | '[' ']'                       
        | "[:" ":]"                     
        | '(' "~#" ')'                  
;

oqtycon

        : qtycon                        
        | '(' qtyconsym ')'             
        | '(' '~' ')'                   
;

qtyconop
        : qtyconsym                     
        | '`' qtycon '`'                
;

qtycon
        : QCONID                        
        | PREFIXQCONSYM                 
        | tycon                         
;

tycon  
        : CONID                         
;

qtyconsym
        : QCONSYM                       
        | QVARSYM                       
        | tyconsym                      
;

tyconsym
        : CONSYM                        
        | VARSYM                        
        | '*'                           
;

op     
        : varop                 
        | conop                 
;

varop  
        : varsym                
        | '`' varid '`'         
;

qop    
        : qvarop                
        | qconop                
;

qopm   
        : qvaropm               
        | qconop                
;

qvarop
        : qvarsym               
        | '`' qvarid '`'        
;

qvaropm
        : qvarsym_no_minus      
        | '`' qvarid '`'        

tyvar   : tyvarid               

tyvarop : '`' tyvarid '`'       
        | '.'                   
;

tyvarid
        : VARID                 
        | special_id            
        | "unsafe"              
        | "safe"                
        | "interruptible"       
;

var    
        : varid                 
        | '(' varsym ')'        
;

qvar   
        : qvarid                
        | '(' varsym ')'        
        | '(' qvarsym1 ')'      
;

qvarid
        : varid                 
        | QVARID                
        | PREFIXQVARSYM         
;

varid
        : VARID                 
        | special_id            
        | "unsafe"              
        | "safe"                
        | "interruptible"       
        | "forall"              
        | "family"              
;

qvarsym
        : varsym                
        | qvarsym1              
;

qvarsym_no_minus
        : varsym_no_minus       
        | qvarsym1              

qvarsym1 : QVARSYM              
;

varsym
        : varsym_no_minus       
        | '-'                   
;

varsym_no_minus
        : VARSYM                
        | special_sym           

special_id
        : "as"                  
        | "qualified"           
        | "hiding"              
        | "export"              
        | "label"               
        | "dynamic"             
        | "stdcall"             
        | "ccall"               
        | "capi"                
        | "prim"                
        | "group"               

special_sym : '!'       
            | '.'       
            | '*'       
;

qconid
        : conid                 
        | QCONID                
        | PREFIXQCONSYM         
;

conid  
        : CONID                 
;

qconsym
        : consym                
        | QCONSYM               
;

consym
        : CONSYM                

        | ':'                   
;

literal
        : CHAR                  
        | STRING                
        | PRIMINTEGER           
        | PRIMWORD              
        | PRIMCHAR              
        | PRIMSTRING            
        | PRIMFLOAT             
        | PRIMDOUBLE            
;

close
        : vccurly                
        | error                 
;

modid  
        : CONID                 
        | QCONID                
;

commas
        : commas ','                    
        | ','                           
;

docnext
  : DOCNEXT 
;

docprev
  : DOCPREV 
;

docnamed
  : DOCNAMED 
;

docsection
  : DOCSECTION 
;

moduleheader
        : DOCNEXT 
;

maybe_docprev
        : docprev                       
        |                    
;

maybe_docnext
        : docnext                       
        |                    
        ;

