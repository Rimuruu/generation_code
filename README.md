Generation code

{
    long x , y;
    x = 3;
    y = 5;
    return (x+2y);

}

On supose que sp = 16

subq $8 (rsp)
subq $8 (rsp)
movq $3 , %rax
movq rax, -24(%rbp)
movq $5 , %rax
movq rax, -32(%rbp)
movq -24(%rbp), rax
pushq rax
movq $2, rax
pushq rax
movq -32(%rbp), rax
imul (%rbp) , rax
addq $8, rsp
addq (%rbp) , rax
addq $8, %rsp
addq $32, %rsp
pushq rbp
ret

long fact(){
  long x = 5;
  long r; r=1;
  while( x>1) {
    r = x*r; x = x-1;
  }
  return r;
}

sp = 0
fact:
            pushq rbp
            mov rsp, rbp
            subq $8 %rsp
            movq $5, rax
            movq %rax, -8(rsp) 
            subq $8 %rsp
            movq $1, rax
            movq %rax, -16(rsp) 
debut_while:
            movq -8(rsp) rax
            pushq rax
            movq $1 rax
            cmp %rsp rax
            test rax rax
            jz fin_while
            movq -8(rsp) rax
            pushq rax
            movq -16(rsp) rax
            imul %rsp ,rax
            addq $8, rsp
            movq rax,-16(rsp)
            movq -8(rsp) rax
            pushq rax
            movq $1, rax
            neg rax
            addq %rsp, rax
            addq $8, rsp
            movq rax,-8(rsp)
            jmp debut_while
fin_while:
        movq -16(rsp) rax
        addq $16 rsp
        popq rbp
        ret