open CLessType
open Tools
open ASMType

type environnement = (string * string) list

let generate_asm_bin_op2 = fun varl sp bop il ->
    match bop with
    |Mult -> (il |+ "imulq (%rsp),%rax"|+ "addq $8 , %rsp")
    |Add -> (il |+ "addq (%rsp),%rax"|+ "addq $8 , %rsp")
    |Sub -> (il |+ "negq %rax" |+ "addq (%rsp),%rax" |+ "addq $8 , %rsp")
  
let rec generate_asm_bin_op = fun varl sp bop il ->
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
    | BOp (b,bo,b2) -> let ill = (generate_asm_bin_op varl sp b il)|+"pushq %rax" in
                       let ill2 = generate_asm_bin_op varl sp b2 ill in
                      generate_asm_bin_op2  varl sp bo ill2 
    with Match_failure(_) -> raise (Code_gen_failure_expression bop)
  

let rec generate_asm_expression = fun varl sp e il ->
  try match e with
      (* *)
  | IntegerLiteral i -> il |+ "movq $"^string_of_int i^", %rax" 
  | BOp (_,_,_) ->  generate_asm_bin_op varl sp e il
  with Match_failure(_) -> raise (Code_gen_failure_expression e)

let rec generate_asm_statement ?retlbl = fun varl sp s il ->
  try match s with
  | CompoundStmt(vl,sl) -> (try match sl with 
                          | [] -> il
                          | x::s -> let il2 = generate_asm_statement varl sp x il in
                            generate_asm_statement varl sp (CompoundStmt (vl,s)) il2
                          with Match_failure(_) -> raise (Code_gen_failure_statment s)
                          )
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
 
