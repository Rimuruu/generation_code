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

let taille_expr = fun e -> 
  let rec pop_e acc exp  = 
    match exp with
    | IntegerLiteral i -> 1 + acc
    | BOp (b,bo,b2) -> 1 + acc + (pop_e 0 b) + (pop_e 0 b2)
    | UOp (uop,b1) -> 1 + acc + (pop_e 0 b1)
    | Var v -> 1 +acc
    | Set (v,e1) -> pop_e (acc+1) e1 
    | Call(fn,e1) -> List.fold_left pop_e acc e1 
    | StringLiteral s -> 1+acc
    | _ -> acc
  in
  pop_e 0 e

  let taille_stat = fun stat -> 
    let rec pop_stat acc st = 
    match st with
    | CompoundStmt( vl, ls) -> (List.length vl) + (List.fold_left pop_stat acc ls )
    | IfStmt (e,s1,s2) -> (taille_expr e) + acc + (pop_stat 0 s1) + (pop_stat 0 s2)
    |Expr e -> (taille_expr e) + acc
    |WhileStmt(e,s) -> pop_stat acc s                           
    | ReturnStmt None -> acc
    | ReturnStmt (Some e) -> acc
    | _ -> acc
    in
  pop_stat 0 stat

  let taille_decl = fun decl -> 
  let rec pop_decl acc dcl = 
  match dcl with                      
  | FunctionDecl(fn,al,s) -> taille_stat s           
  | VarDecl(_) -> 0
  | _ -> 0
  in
  pop_decl 0 decl


let check_inline = fun s  -> 
    let al,st = try  getFunDec s with Not_found -> ([], CompoundStmt( [], [])) in
    let f = FunctionDecl(s,al,st) in
    if (taille_decl f ) < 40  then true else false

(*let register = ["%rdi";"%rsi";"%rdx";"%rcx";"%r8";"%r9"]*)

let generate_asm_un_op = fun varl sp uop il ->
  match uop with
  | Not -> (il  |+ "cmpq $0, %rax" |+ "sete %al" )
  | MinusM -> (il  |+ "negq %rax" )
 

let generate_asm_bin_op2 = fun varl sp bop il ->
    match bop with
    |Mult -> (il |+ "imulq (%rsp),%rax")
    |Add -> (il |+ "addq (%rsp),%rax")
    |Sub -> (il |+ "subq (%rsp),%rax" )
    |EQ -> (il  |+ "movq %rax, %rbx" |+ "xorq %rax, %rax" |+ "cmpq %rbx,(%rsp)" |+ "sete %al")
    |NEQ -> (il  |+ "movq %rax, %rbx" |+ "xorq %rax, %rax" |+ "cmpq %rbx,(%rsp)" |+ "setne %al" )
    |LE -> (il  |+ "movq %rax, %rbx" |+ "xorq %rax, %rax" |+ "cmpq %rbx,(%rsp)" |+ "setns %al" )
    |LL -> (il  |+ "movq %rax, %rbx" |+ "xorq %rax, %rax" |+ "cmpq %rbx,(%rsp)" |+ "setg %al" )
    |Div -> (il  |+ "xorq %rdx,%rdx" |+ "idivq (%rsp) ,%rax " )
    |Mod -> (il |+ "xorq %rdx,%rdx" |+ "idivq (%rsp),%rax" |+ "movq %rdx, %rax" )
    |Or -> (il  |+ "orq (%rsp),%rax" )
    |And -> (il  |+ "andq (%rsp),%rax" )



let rec generate_asm_expression = fun varl sp e il ->
  let rec call_stack = fun varl sp el il i sp2 -> 
    match el with 
    | [] -> (il,sp2)
    | e::s -> 
              let sp3 = sp+8 in 
              let (il1,sp4) = (call_stack varl sp s il (i+1) (sp3)) in
              let il2 = generate_asm_expression varl sp e il1 in 
              let il4 = il2 |+ "pushq %rax" in
              (il4,sp4)
    in
  let rec pop_reg = fun varl sp el il i sp2 -> 
    match el with 
    | [] -> (il,sp2)
    | e::s -> 
              let il2 = il |+ "popq "^(val_to_reg i) in
              let (il3,sp4) = (pop_reg varl sp s il2 (i+1) (sp-8)) in  
              (il3,sp4)
  in
  let rec inline_var = fun varl sp el il->
  match el with
  | [] -> (il |+ "subq $"^string_of_int sp^",%rsp",varl,sp)
  | tv::s -> let sp2 = sp+8 in
            let nvarl = (tv,(string_of_int (-sp2))^"(%rbp)")::varl in
            inline_var nvarl sp2 s il
  in
  try match e with
      (* *)
  | IntegerLiteral i -> (il |+ "movq $"^string_of_int i^", %rax" )
  | SetArray (x,i,e) -> let addr = (List.assoc x varl) in
                        let ill = (generate_asm_expression varl sp i il) |+ "imulq $8, %rax" |+ "pushq %rax" in
                        let ill2 = (ill |+ "leaq "^addr^", %rax") in
                        let ill3 = ill2 |+ "movq %rax, %rbx"
                        |+ "popq %rax"
                        |+ "addq %rbx, %rax"  
                        |+ "movq %rax, %rbx"
                        in 
                        (generate_asm_expression varl sp e ill3)
                        |+ "movq %rax, (%rbx)"
  | BOp (x,Index,e) -> let ill = (generate_asm_expression varl sp e il) |+ "imulq $8, %rax" |+ "pushq %rax" in
                       (generate_asm_expression varl sp x ill)
                        |+ "movq %rax, %rbx"
                        |+ "popq %rax"
                        |+ "addq %rbx, %rax"
                        |+ "movq (%rax),%rax"   
  | BOp (x,SetReference,e) -> let ill = (generate_asm_expression varl sp e il) |+ "pushq %rax" in
                              (generate_asm_expression varl sp x ill)
                              |+ "movq %rax, %rbx"
                              |+ "popq %rax"
                              |+ "movq %rax, (%rbx)"
  | BOp (b,bo,b2) -> let ill = (generate_asm_expression varl sp b2 il)|+"pushq %rax" in
                     let ill2 = (generate_asm_expression varl sp b ill ) in
                     (generate_asm_bin_op2  varl sp bo ill2 ) |+ "addq $8 , %rsp"
  | UOp(Deref,x) -> (generate_asm_expression varl sp x il) |+ "movq (%rax),%rax"
  | Ref x -> let addr = (List.assoc x varl) in
              (il |+ "leaq "^addr^", %rax")
  | UOp (uop,b1) -> let ill = (generate_asm_expression varl sp b1 il)in
                     (generate_asm_un_op varl sp uop ill )

  | Var v -> let b = List.assoc v varl in
             (il |+ "movq "^b^", %rax" )
  |Set (v,e) ->  let il2 = (generate_asm_expression varl sp e il) in
                 let addr = (List.assoc v varl) in
                (il2 |+ "movq %rax, "^addr )
  | Call(fn,exp) -> (*if check_inline fn then 
                        let (sl,s) = try  getFunDec fn with Not_found -> ([], CompoundStmt( [], [])) in
                        let (il2,nvarl,sp2) = inline_var varl sp sl il in
                        let l = fresh_lbl "l" in
                        (generate_asm_statement ~retlbl:l nvarl sp2 s il2)|+ l^":"
                    else*)
                        let ill,sp2 = call_stack varl sp exp il 0 0 in
                        let ill2,sp3 = pop_reg varl sp2 exp ill 0 0 in
                        if( sp3 mod 16 <> 0) then 
                                let il3 = ill2 |+ "subq $8, %rsp" in
                                let il4 = (( il3 |+ ("callq "^fn))) in
                                il4 |+ "addq $8, %rsp"  
                        else 
                                let il3 = (( ill2 |+ ("callq "^fn))) in
                                il3            
  | StringLiteral s -> let addr = addr_lbl_of_string s in
                        (il |+ "leaq "^addr^", %rax")
  with Match_failure(_) -> raise (Code_gen_failure_expression e)
and generate_asm_statement ?retlbl = fun varl sp s il ->
  try match s with
  | CompoundStmt( [], []) -> il
  | CompoundStmt([],sl) -> (try match sl with 
                          | [] -> il
                          | x::s -> let il2 = generate_asm_statement ?retlbl:retlbl varl sp x il in
                            generate_asm_statement ?retlbl:retlbl varl sp (CompoundStmt ([],s)) il2
                          with Match_failure(_) -> raise (Code_gen_failure_statment s)
                          )
  (*| CompoundStmt(vl,sl) -> let (sp2,nvarl) = generate_asm_var vl varl sp in 
                           let iln = il |+ "subq $"^string_of_int (sp2-sp)^", %rsp"in
                          (try match sl with 
                          | [] -> il
                          | x::s -> let il2 = generate_asm_statement nvarl sp2 x iln in
                                    let il3 = generate_asm_statement nvarl sp2 (CompoundStmt ([],s)) il2 in
                                    (il3 |+ "addq $"^string_of_int (sp2-sp)^", %rsp")
                          with Match_failure(_) -> raise (Code_gen_failure_statment s)
                          )*)
  | CompoundStmt( tv::qv, ls) -> 
      let sp2 = sp+8 in
      let nvarl = (tv,(string_of_int (-sp2))^"(%rbp)")::varl in
      let ill =  il |+ "subq $8, %rsp"in
         ((generate_asm_statement ?retlbl nvarl sp2 (CompoundStmt (qv,ls)) ill)
         |+ "addq $8, %rsp")

  | IfStmt (e,s1,s2) ->  let fin_l = fresh_lbl "fin_if" in
                  let else_l = fresh_lbl "else_if" in 
                  let il2 = generate_asm_expression varl sp e il in
                  let il3 = il2 |+ "testq %rax, %rax"
                            |+ "jz "^else_l in
                  let il4 =  generate_asm_statement ?retlbl varl sp s1 il3 in
                  let il5 = il4 |+ "jmp "^fin_l 
                            |+ (else_l)^":" in  
                  let il6 =  generate_asm_statement ?retlbl varl sp s2 il5 in   
                  (il6 |+ (fin_l)^":") 
   |Expr e -> (generate_asm_expression varl sp e il)     
   |WhileStmt(e,s) ->
     let le = fresh_lbl "while_end"
     and lb = fresh_lbl "while_begin" in
     let il2 = il |+ lb ^":" in
     let il3 = generate_asm_expression varl sp e il2
         |+ "testq %rax, %rax"
         |+ "jz "^le in
     generate_asm_statement ?retlbl varl sp s il3
         |+ "jmp "^lb
         |+ le^":"                            
  | ReturnStmt None ->(*(match retlbl with
                    | Some sr -> let l = sr in 
                                (il |+ "jmp "^l)
                    | None ->*)(il
                      |+ "addq $"^(string_of_int sp)^", %rsp"
                      |+ "popq %rbp"
                      |+ "retq") 
                  
  | ReturnStmt (Some e) -> (*(match retlbl with
                    | Some sr -> let l = sr in 
                                (il |+ "jmp "^l)
                    | None ->*)((generate_asm_expression varl sp e il)
                    |+ "addq $"^(string_of_int sp)^", %rsp"
                    |+ "popq %rbp"
                    |+ "retq"
                    )
  with Match_failure(_) -> raise (Code_gen_failure_statment s)

let list_registres = ["%rdi";"%rsi";"%rdx";"%rcx";"%r8";"%r9"]


let rec dec_arg varl sp il args regs =
    match (args, regs) with
       [],_ -> (varl,sp,il)
      | _,[] -> failwith "plus de 6 arguments"
      | ( ta::qa, tr::qr) -> 
        let sp2 = sp+8 in
        let nvarl = (ta,(string_of_int (-sp2))^"(%rbp)")::varl in
        let il2 = (il |+ "pushq "^tr) in
        dec_arg nvarl sp2 il2 qa qr

    
  let generate_asm_top = fun varl il decl ->
  match decl with
  | FunctionDecl(fn,args,s) -> 
        let il2 = 
        il |+ fn^":"
           |+ "pushq %rbp"
           |+ "movq %rsp, %rbp"
            in
        let (nvarl, sp2, il3) = dec_arg varl 0 il2 args list_registres in

          let il4 =  generate_asm_statement nvarl sp2 s il3 in
          generate_asm_statement nvarl sp2 (ReturnStmt None) il4 
  | VarDecl(_) -> il
    (* les variables globals sont déjà geré dans le fichier compilo.ml.
       On ne fait donc rien ici. *)