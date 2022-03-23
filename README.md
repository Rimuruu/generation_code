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
r = x\*r; x = x-1;
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

Déclaration de fonction sur pile

long f( long a, long b){ long x;  
x = 2∗a+b;  
return x ; }

f:  
pushq %rbp  
movq %rsp, %rbp  
subq $8, rsp  
movq $2 , %rax  
pushq %rax  
movq 16(%rbp),%rax  
imulq (%rsp), %rax  
addq $8, %rsp  
pushq %rax  
movq 24(%rbp), %rax  
addq (%rsp), %rax  
addq $8, %rsp  
movq %rax,-8(%rbp)  
movq -8(%rbp),%rax  
movq %rbp, %rsp  
popq %rbp  
ret

appel de fonction sur la pile;

long g ( long x ){  
return f( f ( 1 , 2 ) , 3 );  
}

g:  
pushq %rbp  
movq %rsp, %rbp  
movq $3, %rax  
pushq %rax  
movq $2, %rax  
pushq %rax,  
movq $1, %rax  
pushq rax, %rax  
callq f  
addq $16, %rsp  
pushq %rax;  
callq f  
addq $16, %rs^p  
popq %rbp  
retq  
popq %rbp  
retq

Interference registre  
((5x)+(2y))- y\*y

movq $5, r1x  
movq $x, r2x  
imulq r2x, r1x  
movq $2 ,r3x  
movq $y ,r4x  
imulq r4x, r3x  
addq r3x, r1x  
movq $y,r5x  
movq $y,r6x  
imulq r6x, r5x  
neq r5x  
addq r5x, r1x

compilation méthode JAVA

bipush 30
iload_1
imul
bipush 20
iload_2
imul
iadd
bipush 10
isub
ireturn
