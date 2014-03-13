;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-64 assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :ultraelf) 

(defparameter *test-code-x64*
  #a
  cs:
  ds:
  es:
  fs:
  gs:
  ss:
  clc
  cld
  cli
  cmc
  cmpsb
  cmpsd
  cmpsq
  cmpsw
  hlt
  in    al,dx
  in    ax,dx
  in    eax,dx
  insb
  insd
  insw
  lodsb
  lodsd
  lodsq
  lodsw
  nop
  out   dx,al
  out   dx,ax
  out   dx,eax
  outsb
  outsd
  outsw
  pop   ax
  pop   cx
  pop   dx
  pop   bx
  pop   sp
  pop   bp
  pop   si
  pop   di
  pop   r8w
  pop   r9w
  pop   r10w
  pop   r11w
  pop   r12w
  pop   r13w
  pop   r14w
  pop   r15w
  pop   rax
  pop   rcx
  pop   rdx
  pop   rbx
  pop   rsp
  pop   rbp
  pop   rsi
  pop   rdi
  pop   r8
  pop   r9
  pop   r10
  pop   r11
  pop   r12
  pop   r13
  pop   r14
  pop   r15
  push  ax
  push  cx
  push  dx
  push  bx
  push  sp
  push  bp
  push  si
  push  di
  push  r8w
  push  r9w
  push  r10w
  push  r11w
  push  r12w
  push  r13w
  push  r14w
  push  r15w
  push  rax
  push  rcx
  push  rdx
  push  rbx
  push  rsp
  push  rbp
  push  rsi
  push  rdi
  push  r8
  push  r9
  push  r10
  push  r11
  push  r12
  push  r13
  push  r14
  push  r15
  rep
  rep   cmpsb
  rep   cmpsd
  rep   cmpsq
  rep   cmpsw
  rep   insb
  rep   insd
  rep   insw
  rep   lodsb
  rep   lodsd
  rep   lodsq
  rep   lodsw
  rep   movsb
  rep   movsd
  rep   movsq
  rep   movsw
  rep   outsb
  rep   outsd
  rep   outsw
  rep   scasb
  rep   scasd
  rep   scasq
  rep   scasw
  rep   stosb
  rep   stosd
  rep   stosq
  rep   stosw
  repnz cmpsb
  repnz cmpsd
  repnz cmpsq
  repnz cmpsw
  repnz insb
  repnz insd
  repnz insw
  repnz lodsb
  repnz lodsd
  repnz lodsq
  repnz lodsw
  repnz movsb
  repnz movsd
  repnz movsq
  repnz movsw
  repnz outsb
  repnz outsd
  repnz outsw
  repnz scasb
  repnz scasd
  repnz scasq
  repnz scasw
  repnz stosb
  repnz stosd
  repnz stosq
  repnz stosw
  scasb
  scasd
  scasq
  scasw
  stc
  std
  sti
  stosb
  stosd
  stosq
  stosw
  movsb
  movsd
  movsq
  movsw
  inc   al
  inc   cl
  inc   dl
  inc   bl
  inc   ah
  inc   ch
  inc   dh
  inc   bh
  inc   spl
  inc   bpl
  inc   sil
  inc   dil
  inc   ax
  inc   cx
  inc   dx
  inc   bx
  inc   sp
  inc   bp
  inc   si
  inc   di
  inc   eax
  inc   ecx
  inc   edx
  inc   ebx
  inc   esp
  inc   ebp
  inc   esi
  inc   edi
  inc   rax
  inc   rcx
  inc   rdx
  inc   rbx
  inc   r8b
  inc   r9b
  inc   r10b
  inc   r11b
  inc   r12b
  inc   r13b
  inc   r14b
  inc   r15b
  inc   r8w
  inc   r9w
  inc   r10w
  inc   r11w
  inc   r12w
  inc   r13w
  inc   r14w
  inc   r15w
  inc   r8d
  inc   r9d
  inc   r10d
  inc   r11d
  inc   r12d
  inc   r13d
  inc   r14d
  inc   r15d
  inc   r8
  inc   r9
  inc   r10
  inc   r11
  inc   r12
  inc   r13
  inc   r14
  inc   r15
  #)

(defparameter *example-code-x64*
  #a
  mul   rax           ; rdx:rax = rax^2.
  (
   mov   rbp,rsp        ; create the stack frame
   lea   rdi,[ rbx + 4*rax + testmsg1 ] ; load effective address.
   )
  #)
