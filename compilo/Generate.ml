open CLessType
open Tools
open ASMType

type environnement = (string * string) list

let val_to_reg = fun i ->
  match i with 
  | 0 -> "%rdi"
  | 1 -> "%rsi"
  | 2 -> "%rdx"
  | 3 -> "%rcx"
  | 4 -> "%r8"
  | 5 -> "%r9"
  | _ -> "null"

let generate_asm_un_op = fun varl sp uop il ->
  match uop with
  | Not -> (il  |+ "xorq $1, %rax" )
  | MinusM -> (il  |+ "negq %rax" )

let generate_asm_bin_op2 = fun varl sp bop il ->
    match bop with
    |Mult -> (il |+ "imulq (%rsp),%rax"|+ "addq $8 , %rsp")
    |Add -> (il |+ "addq (%rsp),%rax"|+ "addq $8 , %rsp")
    |Sub -> (il |+ "negq %rax" |+ "addq (%rsp),%rax" |+ "addq $8 , %rsp")
    |EQ -> (il  |+ "cmpq (%rsp),%rax" |+ "sete %al" |+ "addq $8 , %rsp")
    |NEQ -> (il  |+ "cmpq (%rsp),%rax" |+ "setne %al" |+ "addq $8 , %rsp")
    |LE -> (il  |+ "cmpq (%rsp),%rax" |+ "setns %al" |+ "addq $8 , %rsp")
    |LL -> (il  |+ "cmpq (%rsp),%rax" |+ "setg %al" |+ "addq $8 , %rsp")
    |Div -> (il  |+ "movq %rax, %rdx" |+ "movq (%rsp), %rax" |+ "movq %rdx, (%rsp)" |+ "idivq (%rsp) ,%rax " |+ "addq $8 , %rsp")
    |Mod -> (il |+ "movq %rax, %rdx" |+ "movq (%rsp), %rax" |+ "movq %rdx, (%rsp)" |+ "movq $0,%rdx" |+ "idivq (%rsp),%rax" |+ "movq %rdx, %rax" |+ "addq $8 , %rsp")
    |Or -> (il  |+ "orq (%rsp),%rax" |+ "addq $8 , %rsp")
    |And -> (il  |+ "andq (%rsp),%rax" |+ "addq $8 , %rsp")
(*let rec generate_asm_bin_op = fun varl sp bop il ->
    try match bop with
    | BOp (IntegerLiteral i,bo,IntegerLiteral i2) -> let ill = il |+ "movq $"^string_of_int i^", %rax"
                                                                      |+  "pushq %rax" 
                                                                      |+ "movq $"^string_of_int i2^", %rax"in
                    
                                                          generate_asm_bin_op2 varl sp bo ill
    | BOp (IntegerLiteral i,bo,b) -> let ill = il |+ "movq $"^string_of_int i^", %rax"
                                           |+  "pushq %rax" in
                                           let ill2 = generate_asm_bin_op varl sp b ill in
                                           generate_asm_bin_op2 varl sp bo ill2
    | BOp (b,bo,IntegerLiteral i) -> let ill = (generate_asm_bin_op varl sp b il)|+"pushq %rax" in
                                     let ill2 = ill |+ "movq $"^string_of_int i^", %rax" in
                                              generate_asm_bin_op2 varl sp bo ill2
    
    with Match_failure(_) -> raise (Code_gen_failure_expression bop)
  *)



let rec generate_asm_expression = fun varl sp e il ->
  let rec mov_reg = fun varl sp el il i sp2 -> 
    match el with 
    | [] -> (il,sp2)
    | e::s -> 
              let sp3 = sp2+8 in 
              let (il1,sp4) = (mov_reg varl sp s il (i+1) (sp3)) in
              let il2 = generate_asm_expression varl sp e il1 in 
              let il4 = il2 |+ "movq %rax, "^(val_to_reg i) in
              (il4,sp4)
    in
  let rec save_reg = fun i il->
    if i < 6 then let il2 = il |+ "pushq "^(val_to_reg i) in (save_reg (i+1) il2)
    else il
  in
  let rec load_reg = fun i il->
    if i >= 0 then let il2 = il |+ "popq "^(val_to_reg i) in (load_reg (i-1) il2)
    else il
  in
  let rec rsp_mod = fun sp i il ->
      if (sp mod 16) == 0 then (il,i)
      else let il2 = il |+ "pushq %rax" in (rsp_mod (sp+8) (i+1) il2) 
  in
  let rec rsp_unmod = fun i il ->
      if i < 0 then il
      else let il2 = il |+ "popq %rdx" in (rsp_unmod (i-1) il2) 
  in
  try match e with
      (* *)
  | IntegerLiteral i -> (il |+ "movq $"^string_of_int i^", %rax" )
  | BOp (b,bo,b2) -> let ill = (generate_asm_expression varl sp b il)|+"pushq %rax" in
                     let ill2 = (generate_asm_expression varl sp b2 ill ) in
                     (generate_asm_bin_op2  varl sp bo ill2 )
  | UOp (uop,b1) -> let ill = (generate_asm_expression varl sp b1 il)in
                     (generate_asm_un_op varl sp uop ill )
  | Var v -> let b = List.assoc v varl in
             (il |+ "movq "^b^", %rax" )
  |Set (v,e) ->  let il2 = (generate_asm_expression varl sp e il) in
                 let addr = (List.assoc v varl) in
                (il2 |+ "movq %rax, "^addr )
  | Call(fn,exp) -> let ill = save_reg 0 il in
                    let (il2,sp2) = (mov_reg varl sp exp ill 0 0) in
                   let il3 = (( il2 |+ ("callq "^fn))) in
                  (load_reg 5 il3)       
  | StringLiteral s -> let addr = addr_lbl_of_string s in
                        (il |+ "leaq "^addr^", %rax")
  with Match_failure(_) -> raise (Code_gen_failure_expression e)



let rec generate_asm_var = fun vl varl sp ->
  match vl with
  | [] -> (sp,varl)
  | var::s -> let sp2 = sp + 8 in
            let addr = "-"^string_of_int sp2^"(%rbp)" in  
            let nvarl = (var,addr)::varl in
            (generate_asm_var s nvarl sp2)


let rec generate_asm_statement ?retlbl = fun varl sp s il ->
  try match s with
  | CompoundStmt([],sl) -> (try match sl with 
                          | [] -> il
                          | x::s -> let il2 = generate_asm_statement varl sp x il in
                            generate_asm_statement varl sp (CompoundStmt ([],s)) il2
                          with Match_failure(_) -> raise (Code_gen_failure_statment s)
                          )
  | CompoundStmt(vl,sl) -> let (sp2,nvarl) = generate_asm_var vl varl sp in 
                           let iln = il |+ "subq $"^string_of_int (sp2-sp)^", %rsp"in
                          (try match sl with 
                          | [] -> il
                          | x::s -> let il2 = generate_asm_statement nvarl sp2 x iln in
                                    let il3 = generate_asm_statement nvarl sp2 (CompoundStmt ([],s)) il2 in
                                    (il3 |+ "addq $"^string_of_int (sp2-sp)^", %rsp")
                          with Match_failure(_) -> raise (Code_gen_failure_statment s)
                          )
  | IfStmt (e,s1,s2) ->  let fin_l = fresh_lbl "fin_if" in
                  let else_l = fresh_lbl "else_if" in 
                  let il2 = generate_asm_expression varl sp e il in
                  let il3 = il2 |+ "testq %rax, %rax"
                            |+ "jz "^else_l in
                  let il4 =  generate_asm_statement varl sp s1 il3 in
                  let il5 = il4 |+ "jmp "^fin_l 
                            |+ (else_l)^":" in  
                  let il6 =  generate_asm_statement varl sp s2 il5 in   
                  (il6 |+ (fin_l)^":") 
   |Expr e -> (generate_asm_expression varl sp e il)     
   |WhileStmt (e,s) ->  let d_l = fresh_lbl "debut_while" in
                        let f_l = fresh_lbl "fin_while" in 
                        let ill = il |+ (d_l)^":" in
                        let il2 = generate_asm_expression varl sp e ill in 
                        let il3 = il2 |+ "testq %rax, %rax"
                            |+ "jz "^f_l in       
                        let il4 =  generate_asm_statement varl sp s il3 in
                        let il5 = il4 |+ "jmp "^d_l 
                            |+ (f_l)^":" in  
                        il5
                                  
  | ReturnStmt None ->
     (il
         |+ "addq $"^(string_of_int sp)^", %rsp"
         |+ "popq %rbp"
         |+ "retq"
     )
  | ReturnStmt (Some e) -> ((generate_asm_expression varl sp e il)
                    |+ "addq $"^(string_of_int sp)^", %rsp"
                    |+ "popq %rbp"
                    |+ "retq"
     )
  with Match_failure(_) -> raise (Code_gen_failure_statment s)



let rec generate_asm_param_pile = fun varl al i sp il->
    match al with 
    | [] -> (il,varl,sp)
    | x::s ->  let sp2 = sp+8 in
                let nvarl = (x,("-"^(string_of_int sp2)^"(%rbp)"))::varl in 
                          let il2  = il |+ "pushq "^(val_to_reg i) in
                          (generate_asm_param_pile nvarl s (i+1) sp2 il2)      


let generate_asm_top = fun varl il decl ->
  match decl with
  | FunctionDecl(fn,al,CompoundStmt([],[])) -> 
                   let ill = il |+ (fn)^":"
                             |+ "pushq %rbp  " 
                             |+ "movq %rsp , %rbp" in
                 (generate_asm_statement varl 0 (ReturnStmt None) ill )                          
  | FunctionDecl(fn,al,s) ->  
                              let ill = il |+ (fn)^":"
                             |+ "pushq %rbp  " 
                             |+ "movq %rsp , %rbp" in
                             let (il2,nvarl,sp2) = generate_asm_param_pile varl al 0 0 ill in
                            (generate_asm_statement nvarl sp2 s il2 )          
  | VarDecl(_) -> il
    (* les variables globals sont déjà geré dans le fichier compilo.ml.
       On ne fait donc rien ici. *)
 
