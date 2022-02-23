open CLessType
open Tools
open ASMType

type environnement = (string * string) list


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

let generate_asm_top = fun varl il decl ->
  match decl with
  | FunctionDecl(fn,al,s) -> let ill = il |+ (fn)^":"
                             |+ "pushq %rbp  " 
                             |+ "movq %rsp , %rbp" in
                              generate_asm_statement varl 0 s ill
  | VarDecl(_) -> il
    (* les variables globals sont déjà geré dans le fichier compilo.ml.
       On ne fait donc rien ici. *)
 
