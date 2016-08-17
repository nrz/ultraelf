;;;; ultraELF 0.0.1
;;;
;;; ultraELF x86-16, x86-32, x86-64 & ARM assembler, disassembler and metamorphic engine.
;;; ultraELF packs and reconstructs ELF executables, maintaining original functionality.

(in-package :x64)

(defun test-x64-assembling-functions ()
  "This function tests the functioning of `assemble-x64-and-print-hex`."
  (rt:rem-all-tests)

  ;; Tests for single instruction at once, print in hexadecimal.
  (rt:deftest |test-x64-and-print-hex-adc-ax,-12345| (assemble-x64-and-print-hex #a adc ax,-12345 #e) "(66 15 C7 CF)")
  (rt:deftest |test-x64-and-print-hex-add-al,7| (assemble-x64-and-print-hex #a add al,7 #e) "(04 07)")
  (rt:deftest test-x64-and-print-hex-aesimc-xmmreg.xmmrm128-xmm12-[r15] (assemble-x64-and-print-hex #a aesimc-xmmreg.xmmrm128 xmm12 [r15] #e) "(66 45 0F 38 DB 27)")
  (rt:deftest test-x64-and-print-hex-aesimc-xmmreg.xmmrm128-xmm2-[rbx] (assemble-x64-and-print-hex #a aesimc-xmmreg.xmmrm128 xmm2 [rbx] #e) "(66 0F 38 DB 13)")
  (rt:deftest |test-x64-and-print-hex-and-ax,-4096| (assemble-x64-and-print-hex #a and ax,-4096 #e) "(66 25 00 F0)")
  (rt:deftest |test-x64-and-print-hex-bsf-ax,ax| (assemble-x64-and-print-hex #a bsf ax,ax #e) "(66 0F BC C0)")
  (rt:deftest |test-x64-and-print-hex-bsf-eax,[rsi]| (assemble-x64-and-print-hex #a bsf eax,[rsi] #e) "(0F BC 06)")
  (rt:deftest |test-x64-and-print-hex-bsf-eax,eax| (assemble-x64-and-print-hex #a bsf eax,eax #e) "(0F BC C0)")
  (rt:deftest |test-x64-and-print-hex-bsf-eax,ecx| (assemble-x64-and-print-hex #a bsf eax,ecx #e) "(0F BC C1)")
  (rt:deftest |test-x64-and-print-hex-bsf-r14w,[rsi]| (assemble-x64-and-print-hex #a bsf r14w,[rsi] #e) "(66 44 0F BC 36)")
  (rt:deftest |test-x64-and-print-hex-bsf-rax,[rsi]| (assemble-x64-and-print-hex #a bsf rax,[rsi] #e) "(48 0F BC 06)")
  (rt:deftest |test-x64-and-print-hex-bsf-rax,rcx| (assemble-x64-and-print-hex #a bsf rax,rcx #e) "(48 0F BC C1)")
  (rt:deftest |test-x64-and-print-hex-bsr-r14w,[rsi]| (assemble-x64-and-print-hex #a bsr r14w,[rsi] #e) "(66 44 0F BD 36)")
  (rt:deftest test-x64-and-print-hex-bswap-reg32-eax (assemble-x64-and-print-hex #a bswap-reg32 eax #e) "(0F C8)")
  (rt:deftest test-x64-and-print-hex-bswap-reg32-ebp (assemble-x64-and-print-hex #a bswap-reg32 ebp #e) "(0F CD)")
  (rt:deftest test-x64-and-print-hex-bswap-reg32-edx (assemble-x64-and-print-hex #a bswap-reg32 edx #e) "(0F CA)")
  (rt:deftest test-x64-and-print-hex-bswap-reg32-esi (assemble-x64-and-print-hex #a bswap-reg32 esi #e) "(0F CE)")
  (rt:deftest test-x64-and-print-hex-bswap-reg32-r14d (assemble-x64-and-print-hex #a bswap-reg32 r14d #e) "(41 0F CE)")
  (rt:deftest test-x64-and-print-hex-bswap-reg32-r8d (assemble-x64-and-print-hex #a bswap-reg32 r8d #e) "(41 0F C8)")
  (rt:deftest test-x64-and-print-hex-bswap-reg64-r14 (assemble-x64-and-print-hex #a bswap-reg64 r14 #e) "(49 0F CE)")
  (rt:deftest test-x64-and-print-hex-bswap-reg64-r8 (assemble-x64-and-print-hex #a bswap-reg64 r8 #e) "(49 0F C8)")
  (rt:deftest test-x64-and-print-hex-bswap-reg64-rax (assemble-x64-and-print-hex #a bswap-reg64 rax #e) "(48 0F C8)")
  (rt:deftest test-x64-and-print-hex-bswap-reg64-rcx (assemble-x64-and-print-hex #a bswap-reg64 rcx #e) "(48 0F C9)")
  (rt:deftest test-x64-and-print-hex-bswap-reg64-rsi (assemble-x64-and-print-hex #a bswap-reg64 rsi #e) "(48 0F CE)")
  (rt:deftest test-x64-and-print-hex-call-[rsi] (assemble-x64-and-print-hex #a call [rsi] #e) "(FF 16)")
  (rt:deftest test-x64-and-print-hex-cli-void (assemble-x64-and-print-hex #a cli-void #e) "(FA)")
  (rt:deftest test-x64-and-print-hex-cmc-void (assemble-x64-and-print-hex #a cmc-void #e) "(F5)")
  (rt:deftest test-x64-and-print-hex-cmpxchg-reg16.reg16-mr-r12w-bp (assemble-x64-and-print-hex #a cmpxchg-reg16.reg16-mr r12w bp #e) "(66 41 0F B1 EC)")
  (rt:deftest test-x64-and-print-hex-cpuid (assemble-x64-and-print-hex #a cpuid #e) "(0F A2)")
  (rt:deftest test-x64-and-print-hex-cvtps2dq-xmmreg.xmmrm-xmm7-xmm0 (assemble-x64-and-print-hex #a cvtps2dq-xmmreg.xmmrm xmm7 xmm0 #e) "(66 0F 5B F8)")
  (rt:deftest test-x64-and-print-hex-cvtps2dq-xmmreg.xmmrm-xmm7-xmm14 (assemble-x64-and-print-hex #a cvtps2dq-xmmreg.xmmrm xmm7 xmm14 #e) "(66 41 0F 5B FE)")
  (rt:deftest test-x64-and-print-hex-div-rm16-sp (assemble-x64-and-print-hex #a div-rm16 sp #e) "(66 F7 F4)")
  (rt:deftest test-x64-and-print-hex-hlt-void (assemble-x64-and-print-hex #a hlt-void #e) "(F4)")
  (rt:deftest test-x64-and-print-hex-idiv-rm8-ch (assemble-x64-and-print-hex #a idiv-rm8 ch #e) "(F6 FD)")
  ; (rt:deftest test-x64-and-print-hex-imul-rm32-\[rbx\] (assemble-x64-and-print-hex #a imul-rm32 \[rbx\] #e) "(F7 2B)")
  (rt:deftest |test-x64-and-print-hex-in-al,dx| (assemble-x64-and-print-hex #a in al,dx #e) "(EC)")
  (rt:deftest |test-x64-and-print-hex-in-ax,dx| (assemble-x64-and-print-hex #a in ax,dx #e) "(66 ED)")
  (rt:deftest |test-x64-and-print-hex-in-eax,dx| (assemble-x64-and-print-hex #a in eax,dx #e) "(ED)")
  ; (rt:deftest test-x64-and-print-hex-inc-rm64-\[rax\] (assemble-x64-and-print-hex #a inc-rm64 \[rax\] #e) "(48 FF 00)")
  (rt:deftest test-x64-and-print-hex-iret (assemble-x64-and-print-hex #a iret #e) "(CF)")
  (rt:deftest test-x64-and-print-hex-iretd (assemble-x64-and-print-hex #a iretd #e) "(CF)")
  (rt:deftest test-x64-and-print-hex-iretq (assemble-x64-and-print-hex #a iretq #e) "(48 CF)")
  (rt:deftest test-x64-and-print-hex-iretw (assemble-x64-and-print-hex #a iretw #e) "(66 CF)")
  (rt:deftest test-x64-and-print-hex-jmp-[rsi] (assemble-x64-and-print-hex #a jmp [rsi] #e) "(FF 26)")
  (rt:deftest test-x64-and-print-hex-loadall-void (assemble-x64-and-print-hex #a loadall-void #e) "(0F 07)")
  (rt:deftest test-x64-and-print-hex-lodsb (assemble-x64-and-print-hex #a lodsb #e) "(AC)")
  (rt:deftest |test-x64-and-print-hex-mov-ax,bx| (assemble-x64-and-print-hex #a mov ax,bx #e) "(66 89 D8)")
  (rt:deftest test-x64-and-print-hex-movdqu-xmmreg.mem-xmm2-[r14] (assemble-x64-and-print-hex #a movdqu-xmmreg.mem xmm2 [r14] #e) "(F3 41 0F 6F 16)")
  (rt:deftest test-x64-and-print-hex-movdqu-xmmreg.mem-xmm2-[rsi] (assemble-x64-and-print-hex #a movdqu-xmmreg.mem xmm2 [rsi] #e) "(F3 0F 6F 16)")
  (rt:deftest test-x64-and-print-hex-movsx-reg16.mem-r14w-[rdx] (assemble-x64-and-print-hex #a movsx-reg16.mem r14w [rdx] #e) "(66 44 0F BE 32)")
  (rt:deftest test-x64-and-print-hex-movzx-reg16.reg8-rm-dx-ch (assemble-x64-and-print-hex #a movzx-reg16.reg8-rm dx ch #e) "(66 0F B6 D5)")
  (rt:deftest test-x64-and-print-hex-nop (assemble-x64-and-print-hex #a nop #e) "(90)")
  (rt:deftest test-x64-and-print-hex-not-rm32-ebp (assemble-x64-and-print-hex #a not-rm32 ebp #e) "(F7 D5)")
  (rt:deftest |test-x64-and-print-hex-or-ax,517| (assemble-x64-and-print-hex #a or ax,517 #e) "(66 0D 05 02)")
  (rt:deftest |test-x64-and-print-hex-out-dx,al| (assemble-x64-and-print-hex #a out dx,al #e) "(EE)")
  (rt:deftest |test-x64-and-print-hex-out-dx,ax| (assemble-x64-and-print-hex #a out dx,ax #e) "(66 EF)")
  (rt:deftest |test-x64-and-print-hex-out-dx,eax| (assemble-x64-and-print-hex #a out dx,eax #e) "(EF)")
  (rt:deftest test-x64-and-print-hex-popf (assemble-x64-and-print-hex #a popf #e) "(9D)")
  (rt:deftest test-x64-and-print-hex-push-reg16-bx (assemble-x64-and-print-hex #a push-reg16 bx #e) "(66 53)")
  (rt:deftest test-x64-and-print-hex-push-reg16-r11w (assemble-x64-and-print-hex #a push-reg16 r11w #e) "(66 41 53)")
  (rt:deftest test-x64-and-print-hex-push-reg16-r14w (assemble-x64-and-print-hex #a push-reg16 r14w #e) "(66 41 56)")
  (rt:deftest test-x64-and-print-hex-push-reg16-r8w (assemble-x64-and-print-hex #a push-reg16 r8w #e) "(66 41 50)")
  (rt:deftest test-x64-and-print-hex-push-reg16-sp (assemble-x64-and-print-hex #a push-reg16 sp #e) "(66 54)")
  (rt:deftest test-x64-and-print-hex-push-reg64-r14 (assemble-x64-and-print-hex #a push-reg64 r14 #e) "(41 56)")
  (rt:deftest test-x64-and-print-hex-push-reg64-rax (assemble-x64-and-print-hex #a push-reg64 rax #e) "(50)")
  (rt:deftest test-x64-and-print-hex-push-reg64-rsi (assemble-x64-and-print-hex #a push-reg64 rsi #e) "(56)")
  (rt:deftest test-x64-and-print-hex-push-rm64-[r14] (assemble-x64-and-print-hex #a push-rm64 [r14] #e) "(41 FF 36)")
  (rt:deftest test-x64-and-print-hex-push-rm64-[rax] (assemble-x64-and-print-hex #a push-rm64 [rax] #e) "(FF 30)")
  (rt:deftest test-x64-and-print-hex-push-rm64-[rsi] (assemble-x64-and-print-hex #a push-rm64 [rsi] #e) "(FF 36)")
  (rt:deftest test-x64-and-print-hex-push-rm64-r14 (assemble-x64-and-print-hex #a push-rm64 r14 #e) "(41 FF F6)")
  (rt:deftest test-x64-and-print-hex-push-rm64-rax (assemble-x64-and-print-hex #a push-rm64 rax #e) "(FF F0)")
  (rt:deftest test-x64-and-print-hex-push-rm64-rsi (assemble-x64-and-print-hex #a push-rm64 rsi #e) "(FF F6)")
  (rt:deftest test-x64-and-print-hex-pushf (assemble-x64-and-print-hex #a pushf #e) "(9C)")
  (rt:deftest test-x64-and-print-hex-rep (assemble-x64-and-print-hex #a rep #e) "(F3)")
  (rt:deftest |test-x64-and-print-hex-rep-mov-ax,bx| (assemble-x64-and-print-hex #a rep mov ax,bx #e) "(F3 66 89 D8)")
  (rt:deftest test-x64-and-print-hex-rep-movsb (assemble-x64-and-print-hex #a rep movsb #e) "(F3 A4)")
  (rt:deftest test-x64-and-print-hex-rep-nop (assemble-x64-and-print-hex #a rep nop #e) "(F3 90)")
  (rt:deftest |test-x64-and-print-hex-rep-rep-mov-ax,bx| (assemble-x64-and-print-hex #a rep rep mov ax,bx #e) "(F3 F3 66 89 D8)")
  (rt:deftest test-x64-and-print-hex-rep-rep-nop (assemble-x64-and-print-hex #a rep rep nop #e) "(F3 F3 90)")
  (rt:deftest test-x64-and-print-hex-rep-stosd (assemble-x64-and-print-hex #a rep stosd #e) "(F3 AB)")
  (rt:deftest test-x64-and-print-hex-repne (assemble-x64-and-print-hex #a repne #e) "(F2)")
  (rt:deftest test-x64-and-print-hex-repne-scasw (assemble-x64-and-print-hex #a repne scasw #e) "(66 F3 AE)")
  (rt:deftest test-x64-and-print-hex-repnz (assemble-x64-and-print-hex #a repnz #e) "(F2)")
  (rt:deftest test-x64-and-print-hex-repnz-cmpsq (assemble-x64-and-print-hex #a repnz cmpsq #e) "(F2 48 A7)")
  (rt:deftest test-x64-and-print-hex-repnz-rep (assemble-x64-and-print-hex #a repnz rep #e) "(F2 F3)")
  (rt:deftest test-x64-and-print-hex-repnz-rep-repnz-rep (assemble-x64-and-print-hex #a repnz rep repnz rep #e) "(F2 F3 F2 F3)")
  (rt:deftest test-x64-and-print-hex-repnz-scasd (assemble-x64-and-print-hex #a repnz scasd #e) "(F3 AF)")
  (rt:deftest test-x64-and-print-hex-repz (assemble-x64-and-print-hex #a repz #e) "(F3)")
  (rt:deftest |test-x64-and-print-hex-sbb-al,15| (assemble-x64-and-print-hex #a sbb al,15 #e) "(1C 0F)")
  (rt:deftest test-x64-and-print-hex-stosb-void (assemble-x64-and-print-hex #a stosb-void #e) "(AA)")
  (rt:deftest test-x64-and-print-hex-syscall-void (assemble-x64-and-print-hex #a syscall-void #e) "(0F 05)")
  (rt:deftest |test-x64-and-print-hex-xor-al,-128| (assemble-x64-and-print-hex #a xor al,-128 #e) "(34 80)")

  ;; Tests for single instruction at once, no hexadecimal printing.
  (rt:deftest |test-x64-mov-ax,bx| (assemble-x64 #a mov ax,bx #e) (102 137 216))

  ;; Tests for single instruction at once, print in hexadecimal, with given emit function selector functions.
  (rt:deftest |test-x64-and-print-hex-mov-ax,bx-first| (assemble-x64-and-print-hex #a mov ax,bx #e :emit-function-selector-function #'first) "(66 89 D8)")
  (rt:deftest |test-x64-and-print-hex-mov-ax,bx-sort-first-last| (assemble-x64-and-print-hex #a mov ax,bx #e :emit-function-selector-function (list #'sort-sublists-shortest-first #'last)) "(66 8B C3)")
  (rt:deftest |test-x64-and-print-hex-mov-ax,bx-sort-last-first| (assemble-x64-and-print-hex #a mov ax,bx #e :emit-function-selector-function (list #'sort-sublists-shortest-first #'last #'first)) "(66 8B C3)")

  ;; Tests for single instruction at once, print in hexadecimal, with all alternatives.
  (rt:deftest |test-alternatives-x64-and-print-hex-add-[r8],r8b| (assemble-alternatives-x64-and-print-hex #a add [r8],r8b #e) (("(45 00 00)" "(47 00 00)" "(4D 00 00)" "(4F 00 00)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-add-[r8],r8d| (assemble-alternatives-x64-and-print-hex #a add [r8],r8d #e) (("(45 01 00)" "(47 01 00)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-add-[r8],r8w| (assemble-alternatives-x64-and-print-hex #a add [r8],r8w #e) (("(66 45 01 00)" "(66 47 01 00)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-add-eax,3| (assemble-alternatives-x64-and-print-hex #a add eax,3 #e) (("(83 C0 03)" "(05 03 00 00 00)" "(81 C0 03 00 00 00)")))
  (rt:deftest test-alternatives-x64-and-print-hex-bswap-r15d (assemble-alternatives-x64-and-print-hex #a bswap r15d #e) (("(41 0F CF)" "(43 0F CF)" "(45 0F CF)" "(47 0F CF)")))
  (rt:deftest test-alternatives-x64-and-print-hex-bswap-rax (assemble-alternatives-x64-and-print-hex #a bswap rax #e) (("(48 0F C8)" "(4A 0F C8)" "(4C 0F C8)" "(4E 0F C8)")))
  (rt:deftest test-alternatives-x64-and-print-hex-cdqe (assemble-alternatives-x64-and-print-hex #a cdqe #e) (("(48 98)" "(49 98)" "(4A 98)" "(4B 98)" "(4C 98)" "(4D 98)" "(4E 98)" "(4F 98)")))
  (rt:deftest test-alternatives-x64-and-print-hex-cmpsq (assemble-alternatives-x64-and-print-hex #a cmpsq #e) (("(48 A7)" "(49 A7)" "(4A A7)" "(4B A7)" "(4C A7)" "(4D A7)" "(4E A7)" "(4F A7)")))
  (rt:deftest test-alternatives-x64-and-print-hex-cpuid (assemble-alternatives-x64-and-print-hex #a cpuid #e) (("(0F A2)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-[r8] (assemble-alternatives-x64-and-print-hex #a inc [r8] #e) (("(41 FE 00)" "(43 FE 00)" "(45 FE 00)" "(47 FE 00)" "(49 FE 00)" "(4B FE 00)" "(4D FE 00)" "(4F FE 00)" "(66 41 FF 00)" "(66 43 FF 00)" "(66 45 FF 00)" "(66 47 FF 00)" "(41 FF 00)" "(43 FF 00)" "(45 FF 00)" "(47 FF 00)" "(49 FF 00)" "(4B FF 00)" "(4D FF 00)" "(4F FF 00)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-r12b (assemble-alternatives-x64-and-print-hex #a inc r12b #e) (("(41 FE C4)" "(43 FE C4)" "(45 FE C4)" "(47 FE C4)" "(49 FE C4)" "(4B FE C4)" "(4D FE C4)" "(4F FE C4)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-r8 (assemble-alternatives-x64-and-print-hex #a inc r8 #e) (("(49 FF C0)" "(4B FF C0)" "(4D FF C0)" "(4F FF C0)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-r8b (assemble-alternatives-x64-and-print-hex #a inc r8b #e) (("(41 FE C0)" "(43 FE C0)" "(45 FE C0)" "(47 FE C0)" "(49 FE C0)" "(4B FE C0)" "(4D FE C0)" "(4F FE C0)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-r8d (assemble-alternatives-x64-and-print-hex #a inc r8d #e) (("(41 FF C0)" "(43 FF C0)" "(45 FF C0)" "(47 FF C0)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-r8w (assemble-alternatives-x64-and-print-hex #a inc r8w #e) (("(66 41 FF C0)" "(66 43 FF C0)" "(66 45 FF C0)" "(66 47 FF C0)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-rax (assemble-alternatives-x64-and-print-hex #a inc rax #e) (("(48 FF C0)" "(4A FF C0)" "(4C FF C0)" "(4E FF C0)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-rm16-[r8] (assemble-alternatives-x64-and-print-hex #a inc-rm16 [r8] #e) (("(66 41 FF 00)" "(66 43 FF 00)" "(66 45 FF 00)" "(66 47 FF 00)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-rm32-[r8] (assemble-alternatives-x64-and-print-hex #a inc-rm32 [r8] #e) (("(41 FF 00)" "(43 FF 00)" "(45 FF 00)" "(47 FF 00)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-rm64-[r8] (assemble-alternatives-x64-and-print-hex #a inc-rm64 [r8] #e) (("(49 FF 00)" "(4B FF 00)" "(4D FF 00)" "(4F FF 00)")))
  (rt:deftest test-alternatives-x64-and-print-hex-inc-rm8-[r8] (assemble-alternatives-x64-and-print-hex #a inc-rm8 [r8] #e) (("(41 FE 00)" "(43 FE 00)" "(45 FE 00)" "(47 FE 00)" "(49 FE 00)" "(4B FE 00)" "(4D FE 00)" "(4F FE 00)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-mov-ax,bx| (assemble-alternatives-x64-and-print-hex #a mov ax,bx #e) (("(66 89 D8)" "(66 8B C3)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-mov-rax,[rsi]| (assemble-alternatives-x64-and-print-hex #a mov rax,[rsi] #e) (("(48 8B 06)" "(4A 8B 06)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-mov-rax,rcx| (assemble-alternatives-x64-and-print-hex #a mov rax,rcx #e) (("(48 89 C8)" "(4A 89 C8)" "(48 8B C1)" "(4A 8B C1)")))
  (rt:deftest test-alternatives-x64-and-print-hex-mov-reg64.imm-rax-3 (assemble-alternatives-x64-and-print-hex #a mov-reg64.imm rax 3 #e) (("(48 B8 03 00 00 00 00 00 00 00)" "(4A B8 03 00 00 00 00 00 00 00)")))
  (rt:deftest test-alternatives-x64-and-print-hex-movsq (assemble-alternatives-x64-and-print-hex #a movsq #e) (("(48 A5)" "(49 A5)" "(4A A5)" "(4B A5)" "(4C A5)" "(4D A5)" "(4E A5)" "(4F A5)")))
  (rt:deftest test-alternatives-x64-and-print-hex-push-imm8--128 (assemble-alternatives-x64-and-print-hex #a push-imm8 -128 #e) (("(6A 80)")))
  (rt:deftest test-alternatives-x64-and-print-hex-push-imm8-0 (assemble-alternatives-x64-and-print-hex #a push-imm8 0 #e) (("(6A 00)")))
  (rt:deftest test-alternatives-x64-and-print-hex-push-imm8-127 (assemble-alternatives-x64-and-print-hex #a push-imm8 127 #e) (("(6A 7F)")))
  (rt:deftest test-alternatives-x64-and-print-hex-push-r12 (assemble-alternatives-x64-and-print-hex #a push r12 #e) (("(41 54)" "(43 54)" "(45 54)" "(47 54)" "(49 54)" "(4B 54)" "(4D 54)" "(4F 54)" "(41 FF F4)" "(43 FF F4)" "(45 FF F4)" "(47 FF F4)" "(49 FF F4)" "(4B FF F4)" "(4D FF F4)" "(4F FF F4)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-ax,ax| (assemble-alternatives-x64-and-print-hex #a xchg ax,ax #e) (("(66 90)" "(66 87 C0)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-ax,cx| (assemble-alternatives-x64-and-print-hex #a xchg ax,cx #e) (("(66 91)" "(66 87 C1)" "(66 87 C8)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-cx,ax| (assemble-alternatives-x64-and-print-hex #a xchg cx,ax #e) (("(66 91)" "(66 87 C8)" "(66 87 C1)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-eax,eax| (assemble-alternatives-x64-and-print-hex #a xchg eax,eax #e) (("(87 C0)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-eax,ecx| (assemble-alternatives-x64-and-print-hex #a xchg eax,ecx #e) (("(91)" "(87 C1)" "(87 C8)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-ecx,eax| (assemble-alternatives-x64-and-print-hex #a xchg ecx,eax #e) (("(91)" "(87 C8)" "(87 C1)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-rax,rax| (assemble-alternatives-x64-and-print-hex #a xchg rax,rax #e) (("(48 90)" "(4A 90)" "(48 87 C0)" "(4A 87 C0)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-rax,rcx| (assemble-alternatives-x64-and-print-hex #a xchg rax,rcx #e) (("(48 91)" "(4A 91)" "(48 87 C1)" "(4A 87 C1)" "(48 87 C8)" "(4A 87 C8)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xchg-rcx,rax| (assemble-alternatives-x64-and-print-hex #a xchg rcx,rax #e) (("(48 91)" "(4A 91)" "(48 87 C8)" "(4A 87 C8)" "(48 87 C1)" "(4A 87 C1)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xor-rax,-1| (assemble-alternatives-x64-and-print-hex #a xor rax,-1 #e) (("(48 83 F0 FF)" "(4A 83 F0 FF)" "(48 35 FF FF FF FF)" "(4A 35 FF FF FF FF)" "(48 81 F0 FF FF FF FF)" "(4A 81 F0 FF FF FF FF)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-xor-rcx,-1| (assemble-alternatives-x64-and-print-hex #a xor rcx,-1 #e) (("(48 83 F1 FF)" "(4A 83 F1 FF)" "(48 81 F1 FF FF FF FF)" "(4A 81 F1 FF FF FF FF)")))

  ;; Tests for single instruction at once, no hexadecimal printing, with all alternatives.
  (rt:deftest |test-alternatives-x64-mov-ax,bx| (assemble-alternatives-x64 #a mov ax,bx #e) (((102 137 216) (102 139 195))))

  ;; Tests for single instruction at once, no hexadecimal printing, with given emit function selector functions.
  (rt:deftest |test-x64-mov-ax,bx-first| (assemble-x64 #a mov ax,bx #e :emit-function-selector-function #'first) (102 137 216))
  (rt:deftest |test-x64-mov-ax,bx-sort-first| (assemble-x64 #a mov ax,bx #e :emit-function-selector-function (list #'sort-sublists-shortest-first #'last #'first)) (102 139 195))
  (rt:deftest |test-x64-mov-ax,bx-sort-last| (assemble-x64 #a mov ax,bx #e :emit-function-selector-function (list #'sort-sublists-shortest-first #'last)) (102 139 195))

  ;; Tests for single instruction with relative memory reference (`$`) at once, print in hexadecimal.
  (rt:deftest test-x64-and-print-hex-jecxz-$ (assemble-x64-and-print-hex #a jecxz $ #e) "(67 E3 FD)")
  (rt:deftest test-x64-and-print-hex-jmp-imm-short-$ (assemble-x64-and-print-hex #a jmp-imm-short $ #e) "(EB FE)")
  (rt:deftest test-x64-and-print-hex-jrcxz-$ (assemble-x64-and-print-hex #a jrcxz $ #e) "(E3 FE)")
  (rt:deftest test-x64-and-print-hex-loop-$ (assemble-x64-and-print-hex #a loop $ #e) "(E2 FE)")
  (rt:deftest |test-x64-and-print-hex-loop-imm.reg_ecx-$,ecx| (assemble-x64-and-print-hex #a loop-imm.reg_ecx $,ecx #e) "(67 E2 FD)")
  (rt:deftest |test-x64-and-print-hex-loop-imm.reg_rcx-$,rcx| (assemble-x64-and-print-hex #a loop-imm.reg_rcx $,rcx #e) "(E2 FE)")
  (rt:deftest test-x64-and-print-hex-loopnz-$ (assemble-x64-and-print-hex #a loopnz $ #e) "(E0 FE)")
  (rt:deftest |test-x64-and-print-hex-loopnz-imm.reg_ecx-$,ecx| (assemble-x64-and-print-hex #a loopnz-imm.reg_ecx $,ecx #e) "(67 E0 FD)")
  (rt:deftest |test-x64-and-print-hex-loopnz-imm.reg_rcx-$,rcx| (assemble-x64-and-print-hex #a loopnz-imm.reg_rcx $,rcx #e) "(E0 FE)")
  (rt:deftest test-x64-and-print-hex-loopz-$ (assemble-x64-and-print-hex #a loopz $ #e) "(E1 FE)")
  (rt:deftest |test-x64-and-print-hex-loopz-imm.reg_ecx-$,ecx| (assemble-x64-and-print-hex #a loopz-imm.reg_ecx $,ecx #e) "(67 E1 FD)")
  (rt:deftest |test-x64-and-print-hex-loopz-imm.reg_rcx-$,rcx| (assemble-x64-and-print-hex #a loopz-imm.reg_rcx $,rcx #e) "(E1 FE)")

  ;; Tests for single instruction with Common Lisp macro at once, print in hexadecimal.
  (rt:deftest |test-x64-and-print-hex-jmp-imm-short-(+-$-2)| (assemble-x64-and-print-hex #a jmp-imm-short (+ $ 2) #e) "(EB 00)")
  (rt:deftest |test-x64-and-print-hex-jmp-imm-short-(+-$-64)| (assemble-x64-and-print-hex #a jmp-imm-short (+ $ 64) #e) "(EB 3E)")
  (rt:deftest |test-x64-and-print-hex-jmp-imm-short-(+-(+-$-2)-4)| (assemble-x64-and-print-hex #a jmp-imm-short (+ (+ $ 2) 4) #e) "(EB 04)")
  (rt:deftest |test-x64-and-print-hex-jmp-imm-short-(+-(+-(+-$-2)-4)-8)| (assemble-x64-and-print-hex #a jmp-imm-short (+ (+ (+ $ 2) 4) 8) #e) "(EB 0C)")
  (rt:deftest |test-x64-and-print-hex-jmp-imm-short-(logxor-$-17)| (assemble-x64-and-print-hex #a jmp-imm-short (logxor $ 17) #e) "(EB 0F)")

  ;; Tests for single instruction with Common Lisp macro at once, print in hexadecimal, with all alternatives.
  (rt:deftest |test-alternatives-x64-and-print-hex-add-ax\(*--1-(expt-2-15))| (assemble-alternatives-x64-and-print-hex #a add ax,(* -1 (expt 2 15)) #e) (("(66 05 00 80)" "(66 81 C0 00 80)")))
(rt:deftest |test-alternatives-x64-and-print-hex-add-ax,(1--(expt-2-16))| (assemble-alternatives-x64-and-print-hex #a add ax,(1- (expt 2 16)) #e) (("(66 83 C0 FF)" "(66 05 FF FF)" "(66 81 C0 FF FF)")))
(rt:deftest |test-alternatives-x64-and-print-hex-add-eax,(*--1 (expt-2-31))| (assemble-alternatives-x64-and-print-hex #a add eax,(* -1 (expt 2 31)) #e) (("(05 00 00 00 80)" "(81 C0 00 00 00 80)")))
(rt:deftest |test-alternatives-x64-and-print-hex-add-eax,(1--(expt-2-32))| (assemble-alternatives-x64-and-print-hex #a add eax,(1- (expt 2 32)) #e) (("(83 C0 FF)" "(05 FF FF FF FF)" "(81 C0 FF FF FF FF)")))
(rt:deftest |test-alternatives-x64-and-print-hex-mov-reg64.imm-rcx-(1--(expt-2-64))| (assemble-alternatives-x64-and-print-hex #a mov-reg64.imm rcx (1- (expt 2 64)) #e) (("(48 B9 FF FF FF FF FF FF FF FF)" "(4A B9 FF FF FF FF FF FF FF FF)")))
(rt:deftest |test-alternatives-x64-and-print-hex-push-(--(expt-2-31)-1)| (assemble-alternatives-x64-and-print-hex #a push (- (expt 2 31) 1) #e) (("(68 FF FF FF 7F)")))
(rt:deftest |test-alternatives-x64-and-print-hex-push-imm32-(--(expt-2-31)-1)| (assemble-alternatives-x64-and-print-hex #a push-imm32 (- (expt 2 31) 1) #e) (("(68 FF FF FF 7F)")))
(rt:deftest |test-alternatives-x64-and-print-hex-push-imm64-(--(expt-2-31)-1)| (assemble-alternatives-x64-and-print-hex #a push-imm64 (- (expt 2 31) 1) #e) (("(68 FF FF FF 7F)")))
(rt:deftest |test-alternatives-x64-and-print-hex-push-imm8-(--(expt-2-64)-128)| (assemble-alternatives-x64-and-print-hex #a push-imm8 (- (expt 2 64) 128) #e) (("(6A 80)")))
(rt:deftest |test-alternatives-x64-and-print-hex-push-imm8-(1--(expt-2-64))| (assemble-alternatives-x64-and-print-hex #a push-imm8 (1- (expt 2 64)) #e) (("(6A FF)")))
(rt:deftest |test-alternatives-x64-and-print-hex-sub ecx-(1--(expt-2-32))| (assemble-alternatives-x64-and-print-hex #a sub ecx (1- (expt 2 32)) #e) (("(83 E9 FF)" "(81 E9 FF FF FF FF)")))
(rt:deftest |test-alternatives-x64-and-print-hex-sub-rm32.imm8-ecx-(--(expt-2-32)-128)| (assemble-alternatives-x64-and-print-hex #a sub-rm32.imm8 ecx (- (expt 2 32) 128) #e) (("(83 E9 80)")))
(rt:deftest |test-alternatives-x64-and-print-hex-xor-rax,(--(expt-2-64)-1)| (assemble-alternatives-x64-and-print-hex #a xor rax,(- (expt 2 64) 1) #e) (("(48 83 F0 FF)" "(4A 83 F0 FF)" "(48 35 FF FF FF FF)" "(4A 35 FF FF FF FF)" "(48 81 F0 FF FF FF FF)" "(4A 81 F0 FF FF FF FF)")))
(rt:deftest |test-alternatives-x64-and-print-hex-xor-rcx,(--(expt-2-64)-1)| (assemble-alternatives-x64-and-print-hex #a xor rcx,(- (expt 2 64) 1) #e) (("(48 83 F1 FF)" "(4A 83 F1 FF)" "(48 81 F1 FF FF FF FF)" "(4A 81 F1 FF FF FF FF)")))

;; Tests for 2 or more instructions at once, print in hexadecimal.
(rt:deftest |test-x64-and-print-hex-cpuid-mov-ax,bx| (assemble-x64-and-print-hex #a cpuid #a mov ax,bx #e) "(0F A2 66 89 D8)")
(rt:deftest |test-x64-and-print-hex-mov-ax,bx-inc-dx| (assemble-x64-and-print-hex #a mov ax,bx #a inc dx #e) "(66 89 D8 66 FF C2)")
(rt:deftest |test-x64-and-print-hex-mov-ax,bx-xor-cx\,cx| (assemble-x64-and-print-hex #a mov ax,bx #a xor cx,cx #e) "(66 89 D8 66 31 C9)")
(rt:deftest |test-x64-and-print-hex-movsq-movsq-xchg-eax,ecx| (assemble-x64-and-print-hex #a movsq #a movsq #a xchg eax,ecx #e) "(48 A5 48 A5 91)")
(rt:deftest |test-x64-and-print-hex-movsq-scasq-xchg-eax,ecx| (assemble-x64-and-print-hex #a movsq #a scasq #a xchg eax,ecx #e) "(48 A5 48 AF 91)")

;; Tests for 2 or more instructions at once, print in hexadecimal, with all alternatives.
(rt:deftest |test-alternatives-x64-and-print-hex-cpuid-mov-ax,bx| (assemble-alternatives-x64-and-print-hex #a cpuid #a mov ax,bx #e) (("(0F A2)") ("(66 89 D8)" "(66 8B C3)")))
(rt:deftest |test-alternatives-x64-and-print-hex-mov-ax,bx-inc-dx| (assemble-alternatives-x64-and-print-hex #a mov ax,bx #a inc dx #e) (("(66 89 D8)" "(66 8B C3)") ("(66 FF C2)")))
(rt:deftest |test-alternatives-x64-and-print-hex-movsq-scasq-xchg-eax,ecx| (assemble-alternatives-x64-and-print-hex #a movsq #a scasq #a xchg eax,ecx #e) (("(48 A5)" "(49 A5)" "(4A A5)" "(4B A5)" "(4C A5)" "(4D A5)" "(4E A5)" "(4F A5)") ("(48 AF)" "(49 AF)" "(4A AF)" "(4B AF)" "(4C AF)" "(4D AF)" "(4E AF)" "(4F AF)") ("(91)" "(87 C1)" "(87 C8)")))

;; Tests for 2 or more instructions at once, print in hexadecimal, using assembly source code saved in variable.
(rt:deftest test-code-x64-with-lisp-number-1 (assemble-x64-and-print-hex *test-code-x64-with-lisp-number-1*) "(49 FF CC 66 F7 D0 66 F7 DB)")
(rt:deftest test-code-x64-with-lisp-number-2 (assemble-x64-and-print-hex *test-code-x64-with-lisp-number-2*) "(49 FF C2 49 FF CC 66 F7 D0 66 F7 DB)")
(rt:deftest test-code-x64-with-lisp-number-3 (assemble-x64-and-print-hex *test-code-x64-with-lisp-number-3*) "(49 FF C2 49 FF C3 49 FF CC 66 F7 D0 66 F7 DB)")
(rt:deftest test-example-code-x64-with-lisp (assemble-x64-and-print-hex *example-code-x64-with-lisp*) "(49 FF C2 49 FF C3 49 FF C5 49 FF C6 49 FF C7 49 FF CD 49 FF CE 49 FF CF 66 F7 D0 66 F7 DB)")

(rt:do-tests))

(defun test-x64-assembling-functions-limited ()
  "This function tests the functioning of `assemble-x64-and-print-hex`."
  (rt:rem-all-tests)

  ;; Tests for single instruction at once, print in hexadecimal.
  (rt:deftest |test-x64-and-print-hex-add-al,7| (assemble-x64-and-print-hex #a add al,7 #e) "(04 07)")

  ;; Tests for single instruction at once, no hexadecimal printing.

  ;; Tests for single instruction at once, print in hexadecimal, with given emit function selector functions.

  ;; Tests for single instruction at once, print in hexadecimal, with all alternatives.
  (rt:deftest |test-alternatives-x64-and-print-hex-add-[r8],r8b| (assemble-alternatives-x64-and-print-hex #a add [r8],r8b #e) (("(45 00 00)" "(47 00 00)" "(4D 00 00)" "(4F 00 00)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-add-[r8],r8d| (assemble-alternatives-x64-and-print-hex #a add [r8],r8d #e) (("(45 01 00)" "(47 01 00)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-add-[r8],r8w| (assemble-alternatives-x64-and-print-hex #a add [r8],r8w #e) (("(66 45 01 00)" "(66 47 01 00)")))
  (rt:deftest |test-alternatives-x64-and-print-hex-add-eax,3| (assemble-alternatives-x64-and-print-hex #a add eax,3 #e) (("(83 C0 03)" "(05 03 00 00 00)" "(81 C0 03 00 00 00)")))

  ;; Tests for single instruction at once, no hexadecimal printing, with all alternatives.

  ;; Tests for single instruction at once, no hexadecimal printing, with given emit function selector functions.

  ;; Tests for single instruction with relative memory reference (`$`) at once, print in hexadecimal.

  ;; Tests for single instruction with Common Lisp macro at once, print in hexadecimal.

  ;; Tests for single instruction with Common Lisp macro at once, print in hexadecimal, with all alternatives.
  (rt:deftest |test-alternatives-x64-and-print-hex-add-ax\(*--1-(expt-2-15))| (assemble-alternatives-x64-and-print-hex #a add ax,(* -1 (expt 2 15)) #e) (("(66 05 00 80)" "(66 81 C0 00 80)")))
(rt:deftest |test-alternatives-x64-and-print-hex-add-ax,(1--(expt-2-16))| (assemble-alternatives-x64-and-print-hex #a add ax,(1- (expt 2 16)) #e) (("(66 83 C0 FF)" "(66 05 FF FF)" "(66 81 C0 FF FF)")))
(rt:deftest |test-alternatives-x64-and-print-hex-add-eax,(*--1 (expt-2-31))| (assemble-alternatives-x64-and-print-hex #a add eax,(* -1 (expt 2 31)) #e) (("(05 00 00 00 80)" "(81 C0 00 00 00 80)")))
(rt:deftest |test-alternatives-x64-and-print-hex-add-eax,(1--(expt-2-32))| (assemble-alternatives-x64-and-print-hex #a add eax,(1- (expt 2 32)) #e) (("(83 C0 FF)" "(05 FF FF FF FF)" "(81 C0 FF FF FF FF)")))

;; Tests for 2 or more instructions at once, print in hexadecimal.

;; Tests for 2 or more instructions at once, print in hexadecimal, with all alternatives.

;; Tests for 2 or more instructions at once, print in hexadecimal, using assembly source code saved in variable.

(rt:do-tests))
