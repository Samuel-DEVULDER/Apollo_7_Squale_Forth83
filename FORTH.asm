; f9dasm: M6800/1/2/3/8/9 / H6309 Binary/OS9/FLEX9 Disassembler V1.83
; Loaded binary file FORTH.BIN

;****************************************************
;* Used Labels                                      *
;****************************************************

LINBUF  EQU     $C080
SYSDAT  EQU     $CC0E
LINBFP  EQU     $CC14
MEMEND  EQU     $CC2B
PRTCHK  EQU     $CCD8
PRTOUT  EQU     $CCE4
WARMS   EQU     $CD03
GETCHR  EQU     $CD15
PUTCHR  EQU     $CD18
GETFIL  EQU     $CD2D
SETEXT  EQU     $CD33
RPTERR  EQU     $CD3F
DOCMND  EQU     $CD4B
CINCHNE EQU     $D3E5
CSTAT   EQU     $D3F7
COUTCH  EQU     $D3F9
FMSCLS  EQU     $D403
FMS     EQU     $D406
ZDDFD   EQU     $DDFD
EF936X_REG_CMD EQU     $F000
EF936X_REG_CTRL1 EQU     $F001
EF936X_REG_CTRL2 EQU     $F002
EF936X_REG_CSIZE EQU     $F003
EF936X_REG_DELTAX EQU     $F005
EF936X_REG_DELTAY EQU     $F007
EF936X_REG_X_MSB EQU     $F008
EF936X_REG_Y_MSB EQU     $F00A

;****************************************************
;* Program Code / Data Areas                        *
;****************************************************

        ORG     $0000

; Found 1071 words over 16 vocabularies
;
; $8A8B: TASK `STEP LEGENDE TOCH2 TOCH1 HLP3 HLP2 HLP1 $CH8 $CH6
;        $CH4 $CH2 $CH7 $CH3 $CH5 $CH1 $CHB $CHM $CHH LIS TECRAN
;        DANS DIKOA DHB DDD <DEFI> <VOCS> <CONS> <USER> <VARS>
;        DEF(CODE) <CODE> <;CODE> PRESENTE <."> <BRAN> <?BRA>
;        <LOOP> <DO> <LIT> <;> <<T>> DMP L-ANALY <<< <IS> DEFER
;        $DAT <MOTIF> <GTEXTE> H-ITAL H-DROIT HAUT LARGE PIX? PLAN
;        X,Y PRET? PRET XREG DELTAY DELTAX LINE PEINT <PEINT> TEINT
;        <TEINT> TMIXTE TPTI2 TPTI1 TCONT <SEGM> <LINE> PIXEL
;        <TRAIT> DECDSPLY3 <5D> DODAT PR<3 PRINT PRTIT <DO-BL> DFDD
;        TRAIT DS TIT TOWRITE $$ L-VIDE? PA2>LI PILE? TO>SP DELCAR
;        TOBOT PA3>LI LCUR DCUR PAD>SC DEB-MIL DEBU LI>PA3 LI>PA
;        PA>LI LASTLI? LASTLI <DEBLI LI>PA2 XY? LIMAD DEBAD LP3 LP2
;        LP DISPEC `VAL `FIN `EDPAD `LP3 `LP2 `LP PILE DELAI DELAI'
;        DATE DATE' TEMPO XZ XA TX LF HT 4P 8P 0P DEC <> <OF HOME
;        P. PAP L/E DEB 0CAT TTYSET (") DSQR D> DMOD D/ D>S (."F)
;        <=> P~ TRUE <UD> D* DM/MOD DM* D2* TUCK | DOS TRACE (EXEC)
;        (DSPLY) (4H) DEBUG (ST) (RUN) (RSM) <IP DUMP HDR 4H. (NS)
;        (RW) (CB) DR1 DR0 TRK/DRV PUTCHR POUT PCHK TOUTCH TINCH
;        TCHECK PRINTER TERMINAL HLD PRNT DP (BK) (LNK) (SV) PREV
;        DSKERR HIDE DEFINITIONS (FORGET) LINK> L>NAME TRAVERSE
;        (FIND) LAST LIST DOES> (;CODE) THRU LOAD 0 TYPE (CHR) (BS)
;        ( (SCN) (SKP) DIGIT D. D.R <# HOLD PAD HEX DECIMAL , HERE
;        TIB DPL (ABORT") ," (.") LITERAL LOOP LEAVE DO THEN
;        <RESOLVE <MARK (?LEAVE) (LEAVE) (+LOOP) (LOOP) (?DO) (DO)
;        DMAX DMIN D= D0= D- DEPTH DU< D< < 0> 0< 0= XOR DNEGATE
;        DABS D+ D2/ PICK DUP DROP @ (LIT) (;) PAUSE
;
; $8A7E: >DDD >WORDS >TEMP >TRI >TT >MOVETO >EDPT >TL RESTORE
;        RESTITUE V-LIG RTHLP && BANNIERE-PRESENTATION REVUE FEREF
;        "~ >MIN FLG .TB VOC(CODE) VAR(CODE) B-ANALY >>T>> REFER
;        FIN RAPELBODY N-ANALY N-LETR RAPELTIT BORNE1 BADAT V-ITAL
;        V-DROIT 2PI NPIX >SQ N>P .MT FAIT= 2PRINT BLSCR BLDSK BST
;        .IN NOMME: NBS RE READ VA>VIDE FREE RAZEND >SP REDEX VALID
;        RCUR RZ>LI VEXEC BLINE VERPAD RECPAD NAME= BOTM RASEDPAD
;        VBDAT VEC3 VEC2 VEC1 VEC .HEX .DEC .BIN BIN B@ B! N: FOIS
;        VISU VPAD BOX VT BELL 2P 6P >OF BMD BS " NCU ."F FTYPE
;        FRP~ FRP" FRP^ BSP FR ~ >= B/BUF NBCAR FALSE VXTERMINAL
;        VXEXPECT VXWHERE VX." RENDE VXBODY 2SWAP2 NIP 2CTE 2VAR
;        VAR FMS> >FMS FMS RPTERR FILENAME >DOS RESUME BUG >DNEXT
;        N2H. NCH. 2H. >TYPE .S REPORT RDWT BLK/DRV B/SEC BASEDRV
;        BASETRK FLEX RESET BASE RP! R0 VERSION FLUSH BUFFER BLOCK
;        BUFSIZ >BUFS R/W RECURSE REVEAL FORTH VOCABULARY FORGET
;        >LINK N>LINK NAME> >NAME BODY> >BODY FIND >THREAD FENCE
;        VOC-LINK : 2CONSTANT 2VARIABLE VARIABLE FORTH-83 NUMBER BL
;        2 BLANK .( . .R >IN BLK >TIB ." REPEAT BEGIN >RESOLVE
;        >MARK J BRANCH * */ */MOD 2ROT 2SWAP 2DROP FILL > NOT 2/
;        NEGATE 2* 2- 2+ ROLL ROT 2OVER 2DUP R@ R> >R 2@ 2! RP@
;        NOOP >NEXT
;
; $89D3: STEP SORPILE KOA CON(CODE) KOADANS CODES-OP SECURITE
;        SAUTADR CONTENU ?QUIT _PFA SHIFT CTRLHD _LIGNE _CAR _OCT
;        ?END 3* CRASH SPLASH GTEXTE COORD COL? CADRE SELONX SELONY
;        CIRCLE GO CSIZE CTRL2 CTRL1 STATUS CMD SEGM ?N SYSDT CAR~
;        3PRINT SAVE CARNOR CARETR SEPA SAUVE COPIES WIPE ?DON CR8
;        SFSD SFDD SS CTR SCA WW WRIT _READ _WRIT COMM COM! CLWIN
;        OPWIN COL /LI SC>PAD CURCOM SP> ?JMP +LINE SETCUR SORT?
;        SORT CUR! CAR? CURAD COM CUR SITE ?AT CROSS CHAINE KECAR
;        O/N _ON ?ON // ?? 'DATE WIN GR CLEAN CLS CLC SCPA CLR SCRO
;        CES CEL 3P 7P _OF SSDUP _BS SQR S>D WHERE> SP CHANGEBODY
;        CTE C/L C-NAME CLSALL SETEXT GETFIL _( ;CODE CODE GOBUG
;        'DEBUG OF CASE CH. ?BRK ? +THRU WHERE STARTUP SEC/BLK
;        SEC/TRK GETCHR ?KEY KEY 'IO 'STARTUP OUT OFFSET SP! S0
;        SAVE-BUFFERS COPY #BUFS CREATE WORDS CAPS ?CAP WARNING
;        WIDTH CONTEXT CURRENT ; ?CSP CONSTANT 'ABORT 'PROMPT
;        ?STACK [COMPILE] ['] ' CR SPACES SPACE STRING WORD STREAM
;        CONVERT #S # SIGN #> ?COMP [ OCTAL C, SCR CSP STATE SPAN
;        #TIB 'REPORT COMPILE +LOOP ?LEAVE ?DO WHILE ?PAIRS K
;        ?BRANCH / /MOD CMOVE> CMOVE OR +! + SWAP OVER ?DUP COUNT
;        C@ C! SP@
;
; $8933: -OK EXEC EFCAD MESSINIT INMEMO IDEM MEMO E-T QUELCFA
;        USE(CODE) EXCEPTED ANACODE IDENTITE ADREFER MT ECFIN
;        MEMCTR ECDEB ANAT IS MOTIF ATTRIBUTS MOVEREL YREG ARCTO
;        ARC ACDAT INITMT EWR ERE AFE ECARTEX INIPAD UNCOM -RZ>LI
;        UN-LINE -RAZEND INSLI INSCAR -SC>PAD ALINEA -LINE UCUR
;        MEMIN ECRI MILU ENT ENDLI ADLICU MEM ECRAD MANKMEM EDPAD
;        94~ ELD Y/N ARR U: MOVE YZ YA EFF AT 14P 9P 13P 12P 11P 1P
;        10P 5P EE 1CAT YCU INVS -AL AL US =< UD.R UD. EXP-FLEX
;        ARRAY MEMOR -ROT EDITE> ASSEMBLER EDITOR UNBUG IP> ENDCASE
;        ENDOF 1LN EMIT IOSTAT IO INPT USER UP EMPTY-BUFFERS UPDATE
;        IMMEDIATE ID. ABORT QUIT --> QUERY INTERPRET -1 1
;        -TRAILING ERASE EXPECT U. U.R ] ALLOT ABORT" ELSE IF UNTIL
;        AGAIN I MOD M* M/MOD UM/MOD UM* MAX MIN U< = AND ABS - 1-
;        1+ ! EXECUTE EXIT
;
; $39EB: NEXT, REPEAT, BEGIN, VS VC NE NEVER BSR BRA JSR BITB BITA
;        JMP ROL ROR NEG B,S B,U B,Y B,X RTI RTS ROLB RORB NEGB
;        ROLA RORA NEGA NOP
;
; $39D0: END-CODE AGAIN, UNTIL, ELSE, IF, %-B %PAIR MI EQ ALWAYS
;        %CB %RA EXG %PP %PTE %RL ANDCC %C %L %S ADDB ADCB EORB
;        ANDB ADDD ADDA ADCA EORA ANDA %3 %2 %23 INC ASL ASR %1 %IX
;        %PC %R A,S A,U A,Y A,X %X MUL ABX INCB ASLB ASRB INCA ASLA
;        ASRA %Z %CHK %INIT %B %T %P %I %<< %#
;
; $396F: WHILE, GT GE CS CC _A _B _CC _DP _D _X _Y _U _S _PC CWAI
;        ORCC STS STY STU STD STX STB STA CMPU CMPS CMPD CMPY ORB
;        SBCB CMPB SUBB CMPX ORA SUBD SBCA CMPA SUBA CLR COM []
;        SWI3 SWI2 SWI CLRB COMB CLRA COMA SEX SYNC #
;
; $3912: THEN, LE LT PL LS HI TFR PULU PULS PSHU PSHS LEAU LEAS
;        LEAY LEAX LDS LDY LDU LDD LDB LDX LDA TST DEC LSR D,S D,U
;        D,Y D,X ,--S ,-S ,S++ ,S+ ,--U ,-U ,U++ ,U+ ,--Y ,-Y ,Y++
;        ,Y+ ,--X ,-X ,X++ ,X+ ,S ,U ,Y ,X ,PCR ,PC TSTB DECB LSRB
;        TSTA DECA LSRA DAA <<
;
; $2D53: UNDER M U I E 1LINE INSERT-BUF -MOVE MATCH
;
; $2CFD: NEW BRING R F B N FIND-BUF >LINE# BUF-MOVE R#
;
; $2CDB: (L) (ENT) TILL D (E) (F) (DELETE) (SEEK) P (R) (TOP) L T
;        DISPLAY-CURSOR X (SPREAD) (KILL) (HOLD) LINE TEXT (MATCH)
;
; $2C55: G S WIPE SEEK-ERROR #LAG #LEAD #LOCATE CRS C/L
;
; $210D: SEAL ORDER WORDS ONLY
;
; $20DA: VOCABULARIES FORTH
;
; $205E: DEFINITIONS
;
; $2039: ALSO

COLD    JMP     p_reset
WARM    JMP     Z1BB6
MEMLIM  FDB     MEMEND

; ==================================================================
; >NEXT
; ==================================================================
l_next  FDB     $0000

n_next  FCB     $85                      ; NFA -> 5
        FCB     $3E,$4E,$45,$58,$D4      ; ">NEXT"

c_next  FDB     do_var                   ; ASSEMBLER
p_next  LDX     ,Y++
        JMP     [,X]

; ==================================================================
; NOOP
; ==================================================================
l_noop  FDB     l_next                   ; LFA -> >NEXT

n_noop  FCB     $84                      ; NFA -> 4
        FCB     $4E,$4F,$4F,$D0          ; "NOOP"

c_noop  FDB     p_next

; ==================================================================
; PAUSE
; ==================================================================
l_pause FDB     $0000

n_pause FCB     $85                      ; NFA -> 5
        FCB     $50,$41,$55,$53,$C5      ; "PAUSE"

c_pause FDB     p_next

; ==================================================================
; EXIT
; ==================================================================
l_exit  FDB     $0000

n_exit  FCB     $84                      ; NFA -> 4
        FCB     $45,$58,$49,$D4          ; "EXIT"

c_exit  FDB     p_exit                   ; ASSEMBLER
p_exit  PULS    Y
        JMP     p_next

; ==================================================================
; (;)
; ==================================================================
l_semi_ FDB     l_pause                  ; LFA -> PAUSE

n_semi_ FCB     $83                      ; NFA -> 3
        FCB     $28,$3B,$A9              ; "(;)"

c_semi_ FDB     p_exit

; ==================================================================
; EXECUTE
; ==================================================================
l_execu FDB     l_exit                   ; LFA -> EXIT

n_execu FCB     $87                      ; NFA -> 7
        FCB     $45,$58,$45,$43,$55,$54  ; "EXECUT"
        FCB     $C5                      ; "E"

c_execu FDB     p_execu                  ; ASSEMBLER
p_execu PULU    X
        JMP     [,X]

; ==================================================================
; (LIT)
; ==================================================================
l_lit_  FDB     l_semi_                  ; LFA -> (;)

n_lit_  FCB     $85                      ; NFA -> 5
        FCB     $28,$4C,$49,$54,$A9      ; "(LIT)"

c_lit_  FDB     p_lit_                   ; ASSEMBLER
p_lit_  LDD     ,Y++
        PSHU    D
        JMP     p_next

; ==================================================================
; SP@
; ==================================================================
l_spat  FDB     $0000

n_spat  FCB     $83                      ; NFA -> 3
        FCB     $53,$50,$C0              ; "SP@"

c_spat  FDB     p_spat                   ; ASSEMBLER
p_spat  LEAX    ,U
        PSHU    X
        JMP     p_next

; ==================================================================
; RP@
; ==================================================================
l_rpat  FDB     l_noop                   ; LFA -> NOOP

n_rpat  FCB     $83                      ; NFA -> 3
        FCB     $52,$50,$C0              ; "RP@"

c_rpat  FDB     p_rpat                   ; ASSEMBLER
p_rpat  PSHU    S
        JMP     p_next

; ==================================================================
; !
; ==================================================================
l_exc   FDB     l_execu                  ; LFA -> EXECUTE

n_exc   FCB     $81                      ; NFA -> 1
        FCB     $A1                      ; "!"

c_exc   FDB     p_exc                    ; ASSEMBLER
p_exc   LDD     $02,U
        STD     [,U]
        LEAU    $04,U
        JMP     p_next

; ==================================================================
; 2!
; ==================================================================
l_2exc  FDB     l_rpat                   ; LFA -> RP@

n_2exc  FCB     $82                      ; NFA -> 2
        FCB     $32,$A1                  ; "2!"

c_2exc  FDB     p_2exc                   ; ASSEMBLER
p_2exc  PULU    X
        PULU    D
        STD     ,X
        PULU    D
        STD     $02,X
        JMP     p_next

; ==================================================================
; C!
; ==================================================================
l_cexc  FDB     l_spat                   ; LFA -> SP@

n_cexc  FCB     $82                      ; NFA -> 2
        FCB     $43,$A1                  ; "C!"

c_cexc  FDB     p_cexc                   ; ASSEMBLER
p_cexc  LDA     $03,U
        STA     [,U]
        LEAU    $04,U
        JMP     p_next

; ==================================================================
; @
; ==================================================================
l_at    FDB     l_lit_                   ; LFA -> (LIT)

n_at    FCB     $81                      ; NFA -> 1
        FCB     $C0                      ; "@"

c_at    FDB     p_at                     ; ASSEMBLER
p_at    LDD     [,U]
        STD     ,U
        JMP     p_next

; ==================================================================
; 2@
; ==================================================================
l_2at   FDB     l_2exc                   ; LFA -> 2!

n_2at   FCB     $82                      ; NFA -> 2
        FCB     $32,$C0                  ; "2@"

c_2at   FDB     p_2at                    ; ASSEMBLER
p_2at   PULU    X
        LDD     ,X
        LDX     $02,X
        PSHU    X,D
        JMP     p_next

; ==================================================================
; C@
; ==================================================================
l_cat   FDB     l_cexc                   ; LFA -> C!

n_cat   FCB     $82                      ; NFA -> 2
        FCB     $43,$C0                  ; "C@"

c_cat   FDB     p_cat                    ; ASSEMBLER
p_cat   CLRA
        LDB     [,U]
        STD     ,U
        JMP     p_next

; ==================================================================
; COUNT
; ==================================================================
l_count FDB     l_cat                    ; LFA -> C@

n_count FCB     $85                      ; NFA -> 5
        FCB     $43,$4F,$55,$4E,$D4      ; "COUNT"

c_count FDB     p_count                  ; ASSEMBLER
p_count PULU    X
        CLRA
        LDB     ,X+
        PSHU    X,D
        JMP     p_next

; ==================================================================
; >R
; ==================================================================
l_to_r  FDB     l_2at                    ; LFA -> 2@

n_to_r  FCB     $82                      ; NFA -> 2
        FCB     $3E,$D2                  ; ">R"

c_to_r  FDB     p_to_r                   ; ASSEMBLER
p_to_r  PULU    D
        PSHS    D
        JMP     p_next

; ==================================================================
; R>
; ==================================================================
l_from_ FDB     l_to_r                   ; LFA -> >R

n_from_ FCB     $82                      ; NFA -> 2
        FCB     $52,$BE                  ; "R>"

c_from_ FDB     p_from_                  ; ASSEMBLER
p_from_ PULS    D
        PSHU    D
        JMP     p_next

; ==================================================================
; R@
; ==================================================================
l_rat   FDB     l_from_                  ; LFA -> R>

n_rat   FCB     $82                      ; NFA -> 2
        FCB     $52,$C0                  ; "R@"

c_rat   FDB     p_rat                    ; ASSEMBLER
p_rat   LDD     ,S
        PSHU    D
        JMP     p_next

; ==================================================================
; DROP
; ==================================================================
l_drop  FDB     l_at                     ; LFA -> @

n_drop  FCB     $84                      ; NFA -> 4
        FCB     $44,$52,$4F,$D0          ; "DROP"

c_drop  FDB     p_drop                   ; ASSEMBLER
p_drop  LEAU    $02,U
        JMP     p_next

; ==================================================================
; DUP
; ==================================================================
l_dup   FDB     l_drop                   ; LFA -> DROP

n_dup   FCB     $83                      ; NFA -> 3
        FCB     $44,$55,$D0              ; "DUP"

c_dup   FDB     p_dup                    ; ASSEMBLER
p_dup   LDD     ,U
        PSHU    D
        JMP     p_next
        FCB     $00

; ==================================================================
; 2DUP
; ==================================================================
l_2dup  FDB     l_rat                    ; LFA -> R@

n_2dup  FCB     $84                      ; NFA -> 4
        FCB     $32,$44,$55,$D0          ; "2DUP"

c_2dup  FDB     p_2dup                   ; ASSEMBLER
p_2dup  LDD     ,U
        LDX     $02,U
        PSHU    X,D
        JMP     p_next

; ==================================================================
; ?DUP
; ==================================================================
l_qmdup FDB     l_count                  ; LFA -> COUNT

n_qmdup FCB     $84                      ; NFA -> 4
        FCB     $3F,$44,$55,$D0          ; "?DUP"

c_qmdup FDB     p_qmdup                  ; ASSEMBLER
p_qmdup LDD     ,U
        BEQ     Z0149
        PSHU    D
Z0149   JMP     p_next

; ==================================================================
; OVER
; ==================================================================
l_over  FDB     l_qmdup                  ; LFA -> ?DUP

n_over  FCB     $84                      ; NFA -> 4
        FCB     $4F,$56,$45,$D2          ; "OVER"

c_over  FDB     p_over                   ; ASSEMBLER
p_over  LDD     $02,U
        PSHU    D
        JMP     p_next
        FCB     $20

; ==================================================================
; 2OVER
; ==================================================================
l_2over FDB     l_2dup                   ; LFA -> 2DUP

n_2over FCB     $85                      ; NFA -> 5
        FCB     $32,$4F,$56,$45,$D2      ; "2OVER"

c_2over FDB     p_2over                  ; ASSEMBLER
p_2over LDD     $04,U
        LDX     $06,U
        PSHU    X,D
        JMP     p_next

; ==================================================================
; SWAP
; ==================================================================
l_swap  FDB     l_over                   ; LFA -> OVER

n_swap  FCB     $84                      ; NFA -> 4
        FCB     $53,$57,$41,$D0          ; "SWAP"

c_swap  FDB     p_swap                   ; ASSEMBLER
p_swap  PULU    X,D
        PSHU    D
        PSHU    X
        JMP     p_next
        FCB     $20

; ==================================================================
; ROT
; ==================================================================
l_rot   FDB     l_2over                  ; LFA -> 2OVER

n_rot   FCB     $83                      ; NFA -> 3
        FCB     $52,$4F,$D4              ; "ROT"

c_rot   FDB     p_rot                    ; ASSEMBLER
p_rot   PSHS    Y
        PULU    Y,X,D
        PSHU    X,D
        PSHU    Y
        PULS    Y
        JMP     p_next

; ==================================================================
; PICK
; ==================================================================
l_pick  FDB     l_dup                    ; LFA -> DUP

n_pick  FCB     $84                      ; NFA -> 4
        FCB     $50,$49,$43,$CB          ; "PICK"

c_pick  FDB     p_pick                   ; ASSEMBLER
p_pick  PULU    D
        ASLB
        ROLA
        LDD     D,U
        PSHU    D
        JMP     p_next
        FCB     $35

; ==================================================================
; ROLL
; ==================================================================
l_roll  FDB     l_rot                    ; LFA -> ROT

n_roll  FCB     $84                      ; NFA -> 4
        FCB     $52,$4F,$4C,$CC          ; "ROLL"

c_roll  FDB     p_roll                   ; ASSEMBLER
p_roll  PSHS    Y
        PULU    D
        ASLB
        ROLA
        LEAY    D,U
        LDX     ,Y
        PSHU    X
Z01BC   LDX     ,--Y
        STX     $02,Y
        SUBD    #$0002
        BCC     Z01BC
        LEAU    $02,U
        PULS    Y
        JMP     p_next

; ==================================================================
; +
; ==================================================================
l_add   FDB     l_swap                   ; LFA -> SWAP

n_add   FCB     $81                      ; NFA -> 1
        FCB     $AB                      ; "+"

c_add   FDB     p_add                    ; ASSEMBLER
p_add   PULU    D
Z01D3   ADDD    ,U
        STD     ,U
        JMP     p_next

; ==================================================================
; 1+
; ==================================================================
l_1add  FDB     l_exc                    ; LFA -> !

n_1add  FCB     $82                      ; NFA -> 2
        FCB     $31,$AB                  ; "1+"

c_1add  FDB     p_1add                   ; ASSEMBLER
p_1add  LDD     #$0001
        BRA     Z01D3

; ==================================================================
; 1-
; ==================================================================
l_1sub  FDB     l_1add                   ; LFA -> 1+

n_1sub  FCB     $82                      ; NFA -> 2
        FCB     $31,$AD                  ; "1-"

c_1sub  FDB     p_1sub                   ; ASSEMBLER
p_1sub  LDD     #$FFFF
        BRA     Z01D3
        FCB     $38

; ==================================================================
; 2+
; ==================================================================
l_2add  FDB     l_roll                   ; LFA -> ROLL

n_2add  FCB     $82                      ; NFA -> 2
        FCB     $32,$AB                  ; "2+"

c_2add  FDB     p_2add                   ; ASSEMBLER
p_2add  LDD     #$0002
        BRA     Z01D3
        FCB     $2F

; ==================================================================
; 2-
; ==================================================================
l_2sub  FDB     l_2add                   ; LFA -> 2+

n_2sub  FCB     $82                      ; NFA -> 2
        FCB     $32,$AD                  ; "2-"

c_2sub  FDB     p_2sub                   ; ASSEMBLER
p_2sub  LDD     #$FFFE
        BRA     Z01D3
        FCB     $32

; ==================================================================
; 2*
; ==================================================================
l_2ast  FDB     l_2sub                   ; LFA -> 2-

n_2ast  FCB     $82                      ; NFA -> 2
        FCB     $32,$AA                  ; "2*"

c_2ast  FDB     p_2ast                   ; ASSEMBLER
p_2ast  LDD     ,U
        BRA     Z01D3

; ==================================================================
; -
; ==================================================================
l_sub   FDB     l_1sub                   ; LFA -> 1-

n_sub   FCB     $81                      ; NFA -> 1
        FCB     $AD                      ; "-"

c_sub   FDB     p_sub                    ; ASSEMBLER
p_sub   LDD     $02,U
        SUBD    ,U++
        STD     ,U
        JMP     p_next

; ==================================================================
; ABS
; ==================================================================
l_abs   FDB     l_sub                    ; LFA -> -

n_abs   FCB     $83                      ; NFA -> 3
        FCB     $41,$42,$D3              ; "ABS"

c_abs   FDB     p_abs                    ; ASSEMBLER
p_abs   LDA     ,U
        BPL     Z0237
Z0231   CLRA
        CLRB
        SUBD    ,U
        STD     ,U
Z0237   JMP     p_next
        FCB     $31

; ==================================================================
; NEGATE
; ==================================================================
l_negat FDB     l_2ast                   ; LFA -> 2*

n_negat FCB     $86                      ; NFA -> 6
        FCB     $4E,$45,$47,$41,$54,$C5  ; "NEGATE"

c_negat FDB     Z0231
p_negat FCB     $20

; ==================================================================
; 2/
; ==================================================================
l_2div  FDB     l_negat                  ; LFA -> NEGATE

n_2div  FCB     $82                      ; NFA -> 2
        FCB     $32,$AF                  ; "2/"

c_2div  FDB     p_2div                   ; ASSEMBLER
p_2div  ASR     ,U
        ROR     $01,U
        JMP     p_next

; ==================================================================
; D2/
; ==================================================================
l_d2div FDB     l_pick                   ; LFA -> PICK

n_d2div FCB     $83                      ; NFA -> 3
        FCB     $44,$32,$AF              ; "D2/"

c_d2div FDB     p_d2div                  ; ASSEMBLER
p_d2div ASR     ,U
        ROR     $01,U
        ROR     $02,U
        ROR     $03,U
        JMP     p_next

; ==================================================================
; +!
; ==================================================================
l_addex FDB     l_add                    ; LFA -> +

n_addex FCB     $82                      ; NFA -> 2
        FCB     $2B,$A1                  ; "+!"

c_addex FDB     p_addex                  ; ASSEMBLER
p_addex PULU    X
        PULU    D
        ADDD    ,X
        STD     ,X
        JMP     p_next

; ==================================================================
; D+
; ==================================================================
l_dadd  FDB     l_d2div                  ; LFA -> D2/

n_dadd  FCB     $82                      ; NFA -> 2
        FCB     $44,$AB                  ; "D+"

c_dadd  FDB     p_dadd                   ; ASSEMBLER
p_dadd  LDD     $02,U
        ADDD    $06,U
        STD     $06,U
        LDD     ,U
        ADCB    $05,U
        ADCA    $04,U
        LEAU    $04,U
        STD     ,U
        JMP     p_next

; ==================================================================
; DABS
; ==================================================================
l_dabs  FDB     l_dadd                   ; LFA -> D+

n_dabs  FCB     $84                      ; NFA -> 4
        FCB     $44,$41,$42,$D3          ; "DABS"

c_dabs  FDB     p_dabs                   ; ASSEMBLER
p_dabs  LDA     ,U
        BPL     Z02AB
NEG_D2  CLRA
        CLRB
        SUBD    $02,U
        STD     $02,U
        LDD     #COLD
        SBCB    $01,U
        SBCA    ,U
        STD     ,U
Z02AB   JMP     p_next

; ==================================================================
; DNEGATE
; ==================================================================
l_dnega FDB     l_dabs                   ; LFA -> DABS

n_dnega FCB     $87                      ; NFA -> 7
        FCB     $44,$4E,$45,$47,$41,$54  ; "DNEGAT"
        FCB     $C5                      ; "E"

c_dnega FDB     NEG_D2

; ==================================================================
; AND
; ==================================================================
l_and   FDB     l_abs                    ; LFA -> ABS

n_and   FCB     $83                      ; NFA -> 3
        FCB     $41,$4E,$C4              ; "AND"

c_and   FDB     p_and                    ; ASSEMBLER
p_and   PULU    D
        ANDB    $01,U
        ANDA    ,U
        STD     ,U
        JMP     p_next

; ==================================================================
; OR
; ==================================================================
l_or    FDB     l_addex                  ; LFA -> +!

n_or    FCB     $82                      ; NFA -> 2
        FCB     $4F,$D2                  ; "OR"

c_or    FDB     p_or                     ; ASSEMBLER
p_or    PULU    D
        ORB     $01,U
        ORA     ,U
        STD     ,U
        JMP     p_next

; ==================================================================
; XOR
; ==================================================================
l_xor   FDB     l_dnega                  ; LFA -> DNEGATE

n_xor   FCB     $83                      ; NFA -> 3
        FCB     $58,$4F,$D2              ; "XOR"

c_xor   FDB     p_xor                    ; ASSEMBLER
p_xor   PULU    D
        EORB    $01,U
        EORA    ,U
        STD     ,U
        JMP     p_next
        FCB     $30

; ==================================================================
; NOT
; ==================================================================
l_not   FDB     l_2div                   ; LFA -> 2/

n_not   FCB     $83                      ; NFA -> 3
        FCB     $4E,$4F,$D4              ; "NOT"

c_not   FDB     p_not                    ; ASSEMBLER
p_not   COM     ,U
        COM     $01,U
        JMP     p_next

; ==================================================================
; 0=
; ==================================================================
l_0eq   FDB     l_xor                    ; LFA -> XOR

n_0eq   FCB     $82                      ; NFA -> 2
        FCB     $30,$BD                  ; "0="

c_0eq   FDB     p_0eq                    ; ASSEMBLER
p_0eq   LDD     ,U
        BNE     Z030D
Z0308   LDD     #$FFFF
        BRA     Z030F
Z030D   CLRA
        CLRB
Z030F   STD     ,U
        JMP     p_next

; ==================================================================
; 0<
; ==================================================================
l_0lt   FDB     l_0eq                    ; LFA -> 0=

n_0lt   FCB     $82                      ; NFA -> 2
        FCB     $30,$BC                  ; "0<"

c_0lt   FDB     p_0lt                    ; ASSEMBLER
p_0lt   LDA     ,U
        BMI     Z0308
        BRA     Z030D

; ==================================================================
; 0>
; ==================================================================
l_0gt   FDB     l_0lt                    ; LFA -> 0<

n_0gt   FCB     $82                      ; NFA -> 2
        FCB     $30,$BE                  ; "0>"

c_0gt   FDB     p_0gt                    ; ASSEMBLER
p_0gt   LDD     ,U
        BGT     Z0308
        BRA     Z030D

; ==================================================================
; =
; ==================================================================
l_eq    FDB     l_and                    ; LFA -> AND

n_eq    FCB     $81                      ; NFA -> 1
        FCB     $BD                      ; "="

c_eq    FDB     p_eq                     ; ASSEMBLER
p_eq    PULU    X
        CMPX    ,U
        BEQ     Z0308
        BRA     Z030D

; ==================================================================
; <
; ==================================================================
l_lt    FDB     l_0gt                    ; LFA -> 0>

n_lt    FCB     $81                      ; NFA -> 1
        FCB     $BC                      ; "<"

c_lt    FDB     p_lt                     ; ASSEMBLER
p_lt    PULU    X
        CMPX    ,U
Z0345   BGT     Z0308
        BRA     Z030D
        FCB     $31

; ==================================================================
; >
; ==================================================================
l_gt    FDB     l_not                    ; LFA -> NOT

n_gt    FCB     $81                      ; NFA -> 1
        FCB     $BE                      ; ">"

c_gt    FDB     p_gt                     ; ASSEMBLER
p_gt    PULU    X
        CMPX    ,U
        BLT     Z0308
        BRA     Z030D

; ==================================================================
; U<
; ==================================================================
l_ult   FDB     l_eq                     ; LFA -> =

n_ult   FCB     $82                      ; NFA -> 2
        FCB     $55,$BC                  ; "U<"

c_ult   FDB     p_ult                    ; ASSEMBLER
p_ult   PULU    X
Z0361   CMPX    ,U
Z0363   BHI     Z0308
        BRA     Z030D

; ==================================================================
; D<
; ==================================================================
l_dlt   FDB     l_lt                     ; LFA -> <

n_dlt   FCB     $82                      ; NFA -> 2
        FCB     $44,$BC                  ; "D<"

c_dlt   FDB     p_dlt                    ; ASSEMBLER
p_dlt   PULU    X,D
        CMPD    ,U++
        BNE     Z0345
        BRA     Z0361

; ==================================================================
; DU<
; ==================================================================
l_dult  FDB     l_dlt                    ; LFA -> D<

n_dult  FCB     $83                      ; NFA -> 3
        FCB     $44,$55,$BC              ; "DU<"

c_dult  FDB     p_dult                   ; ASSEMBLER
p_dult  PULU    X,D
        CMPD    ,U++
        BNE     Z0363
        BRA     Z0361

; ==================================================================
; MIN
; ==================================================================
l_min   FDB     l_ult                    ; LFA -> U<

n_min   FCB     $83                      ; NFA -> 3
        FCB     $4D,$49,$CE              ; "MIN"

c_min   FDB     p_min                    ; ASSEMBLER
p_min   PULU    X
        CMPX    ,U
        BGE     Z0398
Z0396   STX     ,U
Z0398   JMP     p_next

; ==================================================================
; MAX
; ==================================================================
l_max   FDB     l_min                    ; LFA -> MIN

n_max   FCB     $83                      ; NFA -> 3
        FCB     $4D,$41,$D8              ; "MAX"

c_max   FDB     p_max                    ; ASSEMBLER
p_max   PULU    X
        CMPX    ,U
        BGT     Z0396
        JMP     p_next

; ==================================================================
; CMOVE
; ==================================================================
l_cmove FDB     l_or                     ; LFA -> OR

n_cmove FCB     $85                      ; NFA -> 5
        FCB     $43,$4D,$4F,$56,$C5      ; "CMOVE"

c_cmove FDB     p_cmove                  ; ASSEMBLER
p_cmove PSHS    U,Y
        LDY     $02,U
        LDU     $04,U
        LDX     [$02,S]
        BEQ     Z03C8
Z03C0   LDA     ,U+
        STA     ,Y+
        LEAX    -$01,X
        BNE     Z03C0
Z03C8   PULS    U,Y
        LEAU    $06,U
        JMP     p_next

; ==================================================================
; CMOVE>
; ==================================================================
l_cmov0 FDB     l_cmove                  ; LFA -> CMOVE

n_cmov0 FCB     $86                      ; NFA -> 6
        FCB     $43,$4D,$4F,$56,$45,$BE  ; "CMOVE>"

c_cmov0 FDB     p_cmov0                  ; ASSEMBLER
p_cmov0 PSHS    U,Y
        LDY     $02,U
        LDU     $04,U
        LDX     [$02,S]
        BEQ     Z03F3
        TFR     X,D
        LEAU    D,U
        LEAY    D,Y
Z03EB   LDA     ,-U
        STA     ,-Y
        LEAX    -$01,X
        BNE     Z03EB
Z03F3   PULS    U,Y
        LEAU    $06,U
        JMP     p_next
        FCB     $2E

; ==================================================================
; FILL
; ==================================================================
l_fill  FDB     l_gt                     ; LFA -> >

n_fill  FCB     $84                      ; NFA -> 4
        FCB     $46,$49,$4C,$CC          ; "FILL"

c_fill  FDB     p_fill                   ; ASSEMBLER
p_fill  PSHS    Y
        PULU    Y,X,D
        LEAX    ,X
        BEQ     Z0411
Z040B   STB     ,Y+
        LEAX    -$01,X
        BNE     Z040B
Z0411   PULS    Y
        JMP     p_next

; ==================================================================
; UM*
; ==================================================================
l_umast FDB     l_max                    ; LFA -> MAX

n_umast FCB     $83                      ; NFA -> 3
        FCB     $55,$4D,$AA              ; "UM*"

c_umast FDB     p_umast                  ; ASSEMBLER
p_umast LDA     $01,U
        LDB     $03,U
        MUL
        PSHS    D
        LDA     ,U
        LDB     $02,U
        MUL
        PSHS    D
        LDD     $01,U
        MUL
        ADDD    $01,S
        STD     $01,S
        BCC     Z0436
        INC     ,S
Z0436   LDA     ,U
        LDB     $03,U
        MUL
        ADDD    $01,S
        STD     $01,U
        PULS    D
        ADCA    #$00
        STA     ,U
        PULS    D
        STB     $03,U
        JMP     p_next

; ==================================================================
; UM/MOD
; ==================================================================
l_umdiv FDB     l_umast                  ; LFA -> UM*

n_umdiv FCB     $86                      ; NFA -> 6
        FCB     $55,$4D,$2F,$4D,$4F,$C4  ; "UM/MOD"

c_umdiv FDB     p_umdiv                  ; ASSEMBLER
p_umdiv LDD     $02,U
        LDX     $04,U
        STX     $02,U
        LDX     #$0011
Z045F   CMPD    ,U
        BCC     Z0468
        ANDCC   #$FE
        BRA     Z046C
Z0468   SUBD    ,U
        ORCC    #$01
Z046C   ROL     $03,U
        ROL     $02,U
        LEAX    -$01,X
        BNE     Z047A
        STD     $04,U
        LEAU    $02,U
        JMP     p_next
Z047A   ROLB
        ROLA
        BCS     Z0468
        BRA     Z045F

; ==================================================================
; DEPTH
; ==================================================================
l_depth FDB     l_dult                   ; LFA -> DU<

n_depth FCB     $85                      ; NFA -> 5
        FCB     $44,$45,$50,$54,$C8      ; "DEPTH"

c_depth FDB     do_col                   ; : DEPTH
p_depth FDB     c_spat                   ; SP@
        FDB     c_s0                     ; S0
        FDB     c_at                     ; @
        FDB     c_swap                   ; SWAP
        FDB     c_sub                    ; -
        FDB     c_2div                   ; 2/
        FDB     c_semi_                  ; (;)
        FCB     $31

; ==================================================================
; 2DROP
; ==================================================================
l_2drop FDB     l_fill                   ; LFA -> FILL

n_2drop FCB     $85                      ; NFA -> 5
        FCB     $32,$44,$52,$4F,$D0      ; "2DROP"

c_2drop FDB     do_col                   ; : 2DROP
p_2drop FDB     c_drop                   ; DROP
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)
        FCB     $20

; ==================================================================
; 2SWAP
; ==================================================================
l_2swap FDB     l_2drop                  ; LFA -> 2DROP

n_2swap FCB     $85                      ; NFA -> 5
        FCB     $32,$53,$57,$41,$D0      ; "2SWAP"

c_2swap FDB     do_col                   ; : 2SWAP
p_2swap FDB     c_2swa0                  ; 2SWAP2
        FDB     c_exit                   ; EXIT
        FDB     c_roll,c_lit_            ; ROLL
        FDB     $0003                    ; 3
        FDB     c_roll                   ; ROLL
        FDB     c_semi_                  ; (;)
        FCB     $2E

; ==================================================================
; 2ROT
; ==================================================================
l_2rot  FDB     l_2swap                  ; LFA -> 2SWAP

n_2rot  FCB     $84                      ; NFA -> 4
        FCB     $32,$52,$4F,$D4          ; "2ROT"

c_2rot  FDB     do_col                   ; : 2ROT
p_2rot  FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_roll,c_lit_            ; ROLL
        FDB     $0005                    ; 5
        FDB     c_roll                   ; ROLL
        FDB     c_semi_                  ; (;)

; ==================================================================
; D-
; ==================================================================
l_dsub  FDB     l_depth                  ; LFA -> DEPTH

n_dsub  FCB     $82                      ; NFA -> 2
        FCB     $44,$AD                  ; "D-"

c_dsub  FDB     do_col                   ; : D-
p_dsub  FDB     c_dnega                  ; DNEGATE
        FDB     c_dadd                   ; D+
        FDB     c_semi_                  ; (;)

; ==================================================================
; D0=
; ==================================================================
l_d0eq  FDB     l_dsub                   ; LFA -> D-

n_d0eq  FCB     $83                      ; NFA -> 3
        FCB     $44,$30,$BD              ; "D0="

c_d0eq  FDB     do_col                   ; : D0=
p_d0eq  FDB     c_or                     ; OR
        FDB     c_0eq                    ; 0=
        FDB     c_semi_                  ; (;)

; ==================================================================
; D=
; ==================================================================
l_deq   FDB     l_d0eq                   ; LFA -> D0=

n_deq   FCB     $82                      ; NFA -> 2
        FCB     $44,$BD                  ; "D="

c_deq   FDB     do_col                   ; : D=
p_deq   FDB     c_dsub                   ; D-
        FDB     c_d0eq                   ; D0=
        FDB     c_semi_                  ; (;)

; ==================================================================
; DMIN
; ==================================================================
l_dmin  FDB     l_deq                    ; LFA -> D=

n_dmin  FCB     $84                      ; NFA -> 4
        FCB     $44,$4D,$49,$CE          ; "DMIN"

c_dmin  FDB     do_col                   ; : DMIN
p_dmin  FDB     c_2over                  ; 2OVER
        FDB     c_2over                  ; 2OVER
        FDB     c_dlt                    ; D<
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z0519            ; ?BRANCH --Z0519--v
        FDB     c_2swap                  ; 2SWAP
Z0519   FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; DMAX
; ==================================================================
l_dmax  FDB     l_dmin                   ; LFA -> DMIN

n_dmax  FCB     $84                      ; NFA -> 4
        FCB     $44,$4D,$41,$D8          ; "DMAX"

c_dmax  FDB     do_col                   ; : DMAX
p_dmax  FDB     c_2over                  ; 2OVER
        FDB     c_2over                  ; 2OVER
        FDB     c_dlt                    ; D<
        FDB     c_qmbra,Z0532            ; ?BRANCH --Z0532--v
        FDB     c_2swap                  ; 2SWAP
Z0532   FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; M/MOD
; ==================================================================
l_mdivm FDB     l_umdiv                  ; LFA -> UM/MOD

n_mdivm FCB     $85                      ; NFA -> 5
        FCB     $4D,$2F,$4D,$4F,$C4      ; "M/MOD"

c_mdivm FDB     do_col                   ; : M/MOD
p_mdivm FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_2dup                   ; 2DUP
        FDB     c_xor                    ; XOR
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_dabs                   ; DABS
        FDB     c_rat                    ; R@
        FDB     c_abs                    ; ABS
        FDB     c_umdiv                  ; UM/MOD
        FDB     c_from_                  ; R>
        FDB     c_0lt                    ; 0<
        FDB     c_qmbra,Z0562            ; ?BRANCH --Z0562--v
        FDB     c_swap                   ; SWAP
        FDB     c_negat                  ; NEGATE
        FDB     c_swap                   ; SWAP
Z0562   FDB     c_from_                  ; R>
        FDB     c_0lt                    ; 0<
        FDB     c_qmbra,Z057C            ; ?BRANCH --Z057C--v
        FDB     c_negat                  ; NEGATE
        FDB     c_over                   ; OVER
        FDB     c_qmbra,Z057C            ; ?BRANCH --Z057C--v
        FDB     c_1sub                   ; 1-
        FDB     c_rat                    ; R@
        FDB     c_rot                    ; ROT
        FDB     c_sub                    ; -
        FDB     c_swap                   ; SWAP
Z057C   FDB     c_from_                  ; R>
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; M*
; ==================================================================
l_mast  FDB     l_mdivm                  ; LFA -> M/MOD

n_mast  FCB     $82                      ; NFA -> 2
        FCB     $4D,$AA                  ; "M*"

c_mast  FDB     do_col                   ; : M*
p_mast  FDB     c_2dup                   ; 2DUP
        FDB     c_xor                    ; XOR
        FDB     c_to_r                   ; >R
        FDB     c_abs                    ; ABS
        FDB     c_swap                   ; SWAP
        FDB     c_abs                    ; ABS
        FDB     c_umast                  ; UM*
        FDB     c_from_                  ; R>
        FDB     c_0lt                    ; 0<
        FDB     c_qmbra,Z05A1            ; ?BRANCH --Z05A1--v
        FDB     c_dnega                  ; DNEGATE
Z05A1   FDB     c_semi_                  ; (;)
        FCB     $72

; ==================================================================
; */MOD
; ==================================================================
l_astdi FDB     l_2rot                   ; LFA -> 2ROT

n_astdi FCB     $85                      ; NFA -> 5
        FCB     $2A,$2F,$4D,$4F,$C4      ; "*/MOD"

c_astdi FDB     do_col                   ; : */MOD
p_astdi FDB     c_to_r                   ; >R
        FDB     c_mast                   ; M*
        FDB     c_from_                  ; R>
        FDB     c_mdivm                  ; M/MOD
        FDB     c_semi_                  ; (;)
        FCB     $65

; ==================================================================
; */
; ==================================================================
l_astd0 FDB     l_astdi                  ; LFA -> */MOD

n_astd0 FCB     $82                      ; NFA -> 2
        FCB     $2A,$AF                  ; "*/"

c_astd0 FDB     do_col                   ; : */
p_astd0 FDB     c_astdi                  ; */MOD
        FDB     c_swap                   ; SWAP
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; /MOD
; ==================================================================
l_divmo FDB     l_cmov0                  ; LFA -> CMOVE>

n_divmo FCB     $84                      ; NFA -> 4
        FCB     $2F,$4D,$4F,$C4          ; "/MOD"

c_divmo FDB     do_col                   ; : /MOD
p_divmo FDB     c_to_r                   ; >R
        FDB     c_dup                    ; DUP
        FDB     c_0lt                    ; 0<
        FDB     c_from_                  ; R>
        FDB     c_mdivm                  ; M/MOD
        FDB     c_semi_                  ; (;)

; ==================================================================
; /
; ==================================================================
l_div   FDB     l_divmo                  ; LFA -> /MOD

n_div   FCB     $81                      ; NFA -> 1
        FCB     $AF                      ; "/"

c_div   FDB     do_col                   ; : /
p_div   FDB     c_divmo                  ; /MOD
        FDB     c_swap                   ; SWAP
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; MOD
; ==================================================================
l_mod   FDB     l_mast                   ; LFA -> M*

n_mod   FCB     $83                      ; NFA -> 3
        FCB     $4D,$4F,$C4              ; "MOD"

c_mod   FDB     do_col                   ; : MOD
p_mod   FDB     c_divmo                  ; /MOD
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)
        FCB     $56

; ==================================================================
; *
; ==================================================================
l_ast   FDB     l_astd0                  ; LFA -> */

n_ast   FCB     $81                      ; NFA -> 1
        FCB     $AA                      ; "*"

c_ast   FDB     do_col                   ; : *
p_ast   FDB     c_umast                  ; UM*
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?BRANCH
; ==================================================================
l_qmbra FDB     l_div                    ; LFA -> /

n_qmbra FCB     $87                      ; NFA -> 7
        FCB     $3F,$42,$52,$41,$4E,$43  ; "?BRANC"
        FCB     $C8                      ; "H"

c_qmbra FDB     p_qmbra                  ; ASSEMBLER
p_qmbra LDD     ,U++
        BNE     Z061B
BYES    LDY     ,Y
        JMP     p_next
Z061B   LEAY    $02,Y
        JMP     p_next
        FCB     $20

; ==================================================================
; BRANCH
; ==================================================================
l_branc FDB     l_ast                    ; LFA -> *

n_branc FCB     $86                      ; NFA -> 6
        FCB     $42,$52,$41,$4E,$43,$C8  ; "BRANCH"

c_branc FDB     BYES

; ==================================================================
; (DO)
; ==================================================================
l_do_   FDB     l_dmax                   ; LFA -> DMAX

n_do_   FCB     $84                      ; NFA -> 4
        FCB     $28,$44,$4F,$A9          ; "(DO)"

c_do_   FDB     p_do_                    ; ASSEMBLER
p_do_   LDX     ,Y++
        LDD     $02,U
        ADDA    #$80
        PSHS    X,D
        PULU    X,D
        SUBD    ,S
        PSHS    D
        JMP     p_next

; ==================================================================
; (?DO)
; ==================================================================
l_qmdo_ FDB     l_do_                    ; LFA -> (DO)

n_qmdo_ FCB     $85                      ; NFA -> 5
        FCB     $28,$3F,$44,$4F,$A9      ; "(?DO)"

c_qmdo_ FDB     p_qmdo_                  ; ASSEMBLER
p_qmdo_ LDD     ,U
        SUBD    $02,U
        BNE     p_do_
        LEAU    $04,U
        LDY     ,Y
        JMP     p_next

; ==================================================================
; (LOOP)
; ==================================================================
l_loop_ FDB     l_qmdo_                  ; LFA -> (?DO)

n_loop_ FCB     $86                      ; NFA -> 6
        FCB     $28,$4C,$4F,$4F,$50,$A9  ; "(LOOP)"

c_loop_ FDB     p_loop_                  ; ASSEMBLER
p_loop_ LDD     #$0001
Z0669   ADDD    ,S
        BVC     Z0671
        LEAS    $06,S
        BRA     Z061B
Z0671   STD     ,S
        LDY     ,Y
        JMP     p_next

; ==================================================================
; (+LOOP)
; ==================================================================
l_addlo FDB     l_loop_                  ; LFA -> (LOOP)

n_addlo FCB     $87                      ; NFA -> 7
        FCB     $28,$2B,$4C,$4F,$4F,$50  ; "(+LOOP"
        FCB     $A9                      ; ")"

c_addlo FDB     p_addlo                  ; ASSEMBLER
p_addlo PULU    D
        BRA     Z0669

; ==================================================================
; (LEAVE)
; ==================================================================
l_leave FDB     l_addlo                  ; LFA -> (+LOOP)

n_leave FCB     $87                      ; NFA -> 7
        FCB     $28,$4C,$45,$41,$56,$45  ; "(LEAVE"
        FCB     $A9                      ; ")"

c_leave FDB     p_leave                  ; ASSEMBLER
p_leave PULS    Y,X,D
        JMP     p_next

; ==================================================================
; (?LEAVE)
; ==================================================================
l_qmlea FDB     l_leave                  ; LFA -> (LEAVE)

n_qmlea FCB     $88                      ; NFA -> 8
        FDB     $283F,$4C45,$4156,$45A9  ; "(?LEAVE)"

c_qmlea FDB     p_qmlea                  ; ASSEMBLER
p_qmlea LDD     ,U++
        BNE     p_leave
        JMP     p_next

; ==================================================================
; I
; ==================================================================
l_i     FDB     l_mod                    ; LFA -> MOD

n_i     FCB     $81                      ; NFA -> 1
        FCB     $C9                      ; "I"

c_i     FDB     p_i                      ; ASSEMBLER
p_i     LDD     ,S
        ADDD    $02,S
        PSHU    D
        JMP     p_next
        FCB     $2D

; ==================================================================
; J
; ==================================================================
l_j     FDB     l_branc                  ; LFA -> BRANCH

n_j     FCB     $81                      ; NFA -> 1
        FCB     $CA                      ; "J"

c_j     FDB     p_j                      ; ASSEMBLER
p_j     LDD     $06,S
        ADDD    $08,S
        PSHU    D
        JMP     p_next

; ==================================================================
; K
; ==================================================================
l_k     FDB     l_qmbra                  ; LFA -> ?BRANCH

n_k     FCB     $81                      ; NFA -> 1
        FCB     $CB                      ; "K"

c_k     FDB     p_k                      ; ASSEMBLER
p_k     LDD     $0C,S
        ADDD    $0E,S
        PSHU    D
        JMP     p_next

; ==================================================================
; <MARK
; ==================================================================
l_from0 FDB     l_qmlea                  ; LFA -> (?LEAVE)

n_from0 FCB     $85                      ; NFA -> 5
        FCB     $3C,$4D,$41,$52,$CB      ; "<MARK"

c_from0 FDB     do_col                   ; : <MARK
p_from0 FDB     c_here                   ; HERE
        FDB     c_semi_                  ; (;)

; ==================================================================
; <RESOLVE
; ==================================================================
l_from1 FDB     l_from0                  ; LFA -> <MARK

n_from1 FCB     $88                      ; NFA -> 8
        FDB     $3C52,$4553,$4F4C,$56C5  ; "<RESOLVE"

c_from1 FDB     do_col                   ; : <RESOLVE
p_from1 FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)
        FCB     $20

; ==================================================================
; >MARK
; ==================================================================
l_to_ma FDB     l_j                      ; LFA -> J

n_to_ma FCB     $85                      ; NFA -> 5
        FCB     $3E,$4D,$41,$52,$CB      ; ">MARK"

c_to_ma FDB     do_col                   ; : >MARK
p_to_ma FDB     c_here                   ; HERE
        FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)
        FCB     $69

; ==================================================================
; >RESOLVE
; ==================================================================
l_to_re FDB     l_to_ma                  ; LFA -> >MARK

n_to_re FCB     $88                      ; NFA -> 8
        FDB     $3E52,$4553,$4F4C,$56C5  ; ">RESOLVE"

c_to_re FDB     do_col                   ; : >RESOLVE
p_to_re FDB     c_here                   ; HERE
        FDB     c_swap                   ; SWAP
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?PAIRS
; ==================================================================
l_qmpai FDB     l_k                      ; LFA -> K

n_qmpai FCB     $86                      ; NFA -> 6
        FCB     $3F,$50,$41,$49,$52,$D3  ; "?PAIRS"

c_qmpai FDB     do_col                   ; : ?PAIRS
p_qmpai FDB     c_sub                    ; -
        FDB     c_abort                  ; (ABORT") len=15
        FCB     $0F
        FDB     $5374,$7275,$6374,$7572  ; "Structur"
        FCB     $65,$20,$65,$72,$72,$6F  ; "e erro"
        FCB     $72                      ; "r"
        FDB     c_semi_                  ; (;)
        FCB     $63

; ==================================================================
; BEGIN
; ==================================================================
l_begin FDB     l_to_re                  ; LFA -> >RESOLVE

n_begin FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $42,$45,$47,$49,$CE      ; "BEGIN"

c_begin FDB     do_col                   ; : BEGIN
p_begin FDB     c_qmcom                  ; ?COMP
        FDB     c_from0                  ; <MARK
        FDB     c_1                      ; 1
        FDB     c_semi_                  ; (;)

; ==================================================================
; AGAIN
; ==================================================================
l_again FDB     l_i                      ; LFA -> I

n_again FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $41,$47,$41,$49,$CE      ; "AGAIN"

c_again FDB     do_col                   ; : AGAIN
p_again FDB     c_qmcom                  ; ?COMP
        FDB     c_1                      ; 1
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_branc,c_from1          ; BRANCH --' <RESOLVE--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; UNTIL
; ==================================================================
l_until FDB     l_again                  ; LFA -> AGAIN

n_until FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $55,$4E,$54,$49,$CC      ; "UNTIL"

c_until FDB     do_col                   ; : UNTIL
p_until FDB     c_qmcom                  ; ?COMP
        FDB     c_1                      ; 1
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_qmbra,c_from1          ; ?BRANCH --' <RESOLVE--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; WHILE
; ==================================================================
l_while FDB     l_qmpai                  ; LFA -> ?PAIRS

n_while FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $57,$48,$49,$4C,$C5      ; "WHILE"

c_while FDB     do_col                   ; : WHILE
p_while FDB     c_qmcom                  ; ?COMP
        FDB     c_dup                    ; DUP
        FDB     c_1                      ; 1
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_qmbra,c_to_ma          ; ?BRANCH --' >MARK--^
        FDB     c_2                      ; 2
        FDB     c_semi_                  ; (;)
        FCB     $69

; ==================================================================
; REPEAT
; ==================================================================
l_repea FDB     l_begin                  ; LFA -> BEGIN

n_repea FCB     $C6                      ; NFA -> 6 IMMEDIATE
        FCB     $52,$45,$50,$45,$41,$D4  ; "REPEAT"

c_repea FDB     do_col                   ; : REPEAT
p_repea FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_again                  ; AGAIN
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_2                      ; 2
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_to_re                  ; >RESOLVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; IF
; ==================================================================
l_if    FDB     l_until                  ; LFA -> UNTIL

n_if    FCB     $C2                      ; NFA -> 2 IMMEDIATE
        FCB     $49,$C6                  ; "IF"

c_if    FDB     do_col                   ; : IF
p_if    FDB     c_qmcom                  ; ?COMP
        FDB     c_compi                  ; COMPILE
        FDB     c_qmbra,c_to_ma,c_lit_   ; ?BRANCH --' >MARK--^
        FDB     $0004                    ; 4
        FDB     c_semi_                  ; (;)

; ==================================================================
; ELSE
; ==================================================================
l_else  FDB     l_if                     ; LFA -> IF

n_else  FCB     $C4                      ; NFA -> 4 IMMEDIATE
        FCB     $45,$4C,$53,$C5          ; "ELSE"

c_else  FDB     do_col                   ; : ELSE
p_else  FDB     c_qmcom,c_lit_           ; ?COMP
        FDB     $0004                    ; 4
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_branc,c_to_ma          ; BRANCH --' >MARK--^
        FDB     c_swap                   ; SWAP
        FDB     c_to_re,c_lit_           ; >RESOLVE
        FDB     $0005                    ; 5
        FDB     c_semi_                  ; (;)

; ==================================================================
; THEN
; ==================================================================
l_then  FDB     l_from1                  ; LFA -> <RESOLVE

n_then  FCB     $C4                      ; NFA -> 4 IMMEDIATE
        FCB     $54,$48,$45,$CE          ; "THEN"

c_then  FDB     do_col                   ; : THEN
p_then  FDB     c_qmcom                  ; ?COMP
        FDB     c_1                      ; 1
        FDB     c_or,c_lit_              ; OR
        FDB     $0005                    ; 5
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_to_re                  ; >RESOLVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; DO
; ==================================================================
l_do    FDB     l_then                   ; LFA -> THEN

n_do    FCB     $C2                      ; NFA -> 2 IMMEDIATE
        FCB     $44,$CF                  ; "DO"

c_do    FDB     do_col                   ; : DO
p_do    FDB     c_qmcom                  ; ?COMP
        FDB     c_compi                  ; COMPILE
        FDB     c_do_,c_to_ma            ; (DO) --' >MARK--^
        FDB     c_from0                  ; <MARK
        FDB     c_csp                    ; CSP
        FDB     c_at                     ; @
        FDB     c_spat                   ; SP@
        FDB     c_csp                    ; CSP
        FDB     c_exc,c_lit_             ; !
        FDB     $0011                    ; 17
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?DO
; ==================================================================
l_qmdo  FDB     l_while                  ; LFA -> WHILE

n_qmdo  FCB     $C3                      ; NFA -> 3 IMMEDIATE
        FCB     $3F,$44,$CF              ; "?DO"

c_qmdo  FDB     do_col                   ; : ?DO
p_qmdo  FDB     c_do,c_lit_              ; DO
        FDB     $064C                    ; ' (?DO)
        FDB     c_here,c_lit_            ; HERE
        FDB     $0004                    ; 4
        FDB     c_sub                    ; -
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; LEAVE
; ==================================================================
l_leav0 FDB     l_do                     ; LFA -> DO

n_leav0 FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $4C,$45,$41,$56,$C5      ; "LEAVE"

c_leav0 FDB     do_col                   ; : LEAVE
p_leav0 FDB     c_qmcom                  ; ?COMP
        FDB     c_csp                    ; CSP
        FDB     c_at                     ; @
        FDB     c_2sub                   ; 2-
        FDB     c_at,c_lit_              ; @
        FDB     $0011                    ; 17
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_leave                  ; (LEAVE)
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?LEAVE
; ==================================================================
l_qmle0 FDB     l_qmdo                   ; LFA -> ?DO

n_qmle0 FCB     $C6                      ; NFA -> 6 IMMEDIATE
        FCB     $3F,$4C,$45,$41,$56,$C5  ; "?LEAVE"

c_qmle0 FDB     do_col                   ; : ?LEAVE
p_qmle0 FDB     c_leav0,c_lit_           ; LEAVE
        FDB     $06A3                    ; ' (?LEAVE)
        FDB     c_here                   ; HERE
        FDB     c_2sub                   ; 2-
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; LOOP
; ==================================================================
l_loop  FDB     l_leav0                  ; LFA -> LEAVE

n_loop  FCB     $C4                      ; NFA -> 4 IMMEDIATE
        FCB     $4C,$4F,$4F,$D0          ; "LOOP"

c_loop  FDB     do_col                   ; : LOOP
p_loop  FDB     c_qmcom,c_lit_           ; ?COMP
        FDB     $0011                    ; 17
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_csp                    ; CSP
        FDB     c_exc                    ; !
        FDB     c_compi                  ; COMPILE
        FDB     c_loop_,c_from1          ; (LOOP) --' <RESOLVE--^
        FDB     c_to_re                  ; >RESOLVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; +LOOP
; ==================================================================
l_addl0 FDB     l_qmle0                  ; LFA -> ?LEAVE

n_addl0 FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $2B,$4C,$4F,$4F,$D0      ; "+LOOP"

c_addl0 FDB     do_col                   ; : +LOOP
p_addl0 FDB     c_qmcom,c_lit_           ; ?COMP
        FDB     $0011                    ; 17
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_csp                    ; CSP
        FDB     c_exc                    ; !
        FDB     c_compi                  ; COMPILE
        FDB     c_addlo,c_from1          ; (+LOOP) --' <RESOLVE--^
        FDB     c_to_re                  ; >RESOLVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; COMPILE
; ==================================================================
l_compi FDB     l_addl0                  ; LFA -> +LOOP

n_compi FCB     $87                      ; NFA -> 7
        FCB     $43,$4F,$4D,$50,$49,$4C  ; "COMPIL"
        FCB     $C5                      ; "E"

c_compi FDB     do_col                   ; : COMPILE
p_compi FDB     c_from_                  ; R>
        FDB     c_dup                    ; DUP
        FDB     c_2add                   ; 2+
        FDB     c_to_r                   ; >R
        FDB     c_at                     ; @
        FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)

; ==================================================================
; LITERAL
; ==================================================================
l_liter FDB     l_loop                   ; LFA -> LOOP

n_liter FCB     $C7                      ; NFA -> 7 IMMEDIATE
        FCB     $4C,$49,$54,$45,$52,$41  ; "LITERA"
        FCB     $CC                      ; "L"

c_liter FDB     do_col                   ; : LITERAL
p_liter FDB     c_qmcom                  ; ?COMP
        FDB     c_compi,c_lit_           ; COMPILE
        FDB     $0A20                    ; ' ,
        FDB     c_semi_                  ; (;)

; ==================================================================
; (.")
; ==================================================================
l_dotqt FDB     l_liter                  ; LFA -> LITERAL

n_dotqt FCB     $84                      ; NFA -> 4
        FCB     $28,$2E,$22,$A9          ; '(.\")'

c_dotqt FDB     do_col                   ; : (.")
p_dotqt FDB     c_from_                  ; R>
        FDB     c_count                  ; COUNT
        FDB     c_2dup                   ; 2DUP
        FDB     c_add                    ; +
        FDB     c_to_r                   ; >R
        FDB     c_type                   ; TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; ,"
; ==================================================================
l_komqt FDB     l_dotqt                  ; LFA -> (.")

n_komqt FCB     $82                      ; NFA -> 2
        FCB     $2C,$A2                  ; ',"'

c_komqt FDB     do_col                   ; : ,"
p_komqt FDB     c_lit_
        FDB     $0022                    ; 34
        FDB     c_strin                  ; STRING
        FDB     c_dup                    ; DUP
        FDB     c_ckom                   ; C,
        FDB     c_here                   ; HERE
        FDB     c_swap                   ; SWAP
        FDB     c_dup                    ; DUP
        FDB     c_allot                  ; ALLOT
        FDB     c_cmove                  ; CMOVE
        FDB     c_semi_                  ; (;)
        FCB     $72

; ==================================================================
; ."
; ==================================================================
l_dotq0 FDB     l_repea                  ; LFA -> REPEAT

n_dotq0 FCB     $C2                      ; NFA -> 2 IMMEDIATE
        FCB     $2E,$A2                  ; '."'

c_dotq0 FDB     do_col                   ; : ."
p_dotq0 FDB     c_dotq2                  ; ."F
        FDB     c_exit                   ; EXIT
        FDB     c_dotqt                  ; (.")
        FDB     c_komqt                  ; ,"
        FDB     c_semi_                  ; (;)

; ==================================================================
; 'REPORT
; ==================================================================
l_tckre FDB     l_compi                  ; LFA -> COMPILE

n_tckre FCB     $87                      ; NFA -> 7
        FCB     $27,$52,$45,$50,$4F,$52  ; "'REPOR"
        FCB     $D4                      ; "T"

c_tckre FDB     do_var                   ; VARIABLE 'REPORT
p_tckre FDB     $223C                    ; 8764

; ==================================================================
; (ABORT")
; ==================================================================
l_abort FDB     l_komqt                  ; LFA -> ,"

n_abort FCB     $88                      ; NFA -> 8
        FDB     $2841,$424F,$5254,$22A9  ; '(ABORT\")'

c_abort FDB     do_col                   ; : (ABORT")
p_abort FDB     c_from_                  ; R>
        FDB     c_count                  ; COUNT
        FDB     c_rot                    ; ROT
        FDB     c_qmbra,Z0961            ; ?BRANCH --Z0961--v
        FDB     c_tckre                  ; 'REPORT
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_abor1                  ; ABORT
Z0961   FDB     c_add                    ; +
        FDB     c_to_r                   ; >R
        FDB     c_semi_                  ; (;)

; ==================================================================
; ABORT"
; ==================================================================
l_abor0 FDB     l_else                   ; LFA -> ELSE

n_abor0 FCB     $C6                      ; NFA -> 6 IMMEDIATE
        FCB     $41,$42,$4F,$52,$54,$A2  ; 'ABORT"'

c_abor0 FDB     do_col                   ; : ABORT"
p_abor0 FDB     c_qmcom                  ; ?COMP
        FDB     c_compi                  ; COMPILE
        FDB     c_abort                  ; (ABORT")
        FDB     c_komqt                  ; ,"
        FDB     c_semi_                  ; (;)
        FCB     $65

; ==================================================================
; >TIB
; ==================================================================
l_to_ti FDB     l_dotq0                  ; LFA -> ."

n_to_ti FCB     $84                      ; NFA -> 4
        FCB     $3E,$54,$49,$C2          ; ">TIB"

c_to_ti FDB     do_var                   ; VARIABLE >TIB
p_to_ti FDB     $B2F0                    ; 45808

; ==================================================================
; #TIB
; ==================================================================
l_n_tib FDB     l_tckre                  ; LFA -> 'REPORT

n_n_tib FCB     $84                      ; NFA -> 4
        FCB     $23,$54,$49,$C2          ; "#TIB"

c_n_tib FDB     do_var                   ; VARIABLE #TIB
p_n_tib FDB     $0004                    ; 4

; ==================================================================
; SPAN
; ==================================================================
l_span  FDB     l_n_tib                  ; LFA -> #TIB

n_span  FCB     $84                      ; NFA -> 4
        FCB     $53,$50,$41,$CE          ; "SPAN"

c_span  FDB     do_var                   ; VARIABLE SPAN
p_span  FDB     $0004                    ; 4
        FCB     $64

; ==================================================================
; BLK
; ==================================================================
l_blk   FDB     l_to_ti                  ; LFA -> >TIB

n_blk   FCB     $83                      ; NFA -> 3
        FCB     $42,$4C,$CB              ; "BLK"

c_blk   FDB     do_var                   ; VARIABLE BLK
p_blk   FDB     $0000                    ; 0
        FCB     $65

; ==================================================================
; >IN
; ==================================================================
l_to_in FDB     l_blk                    ; LFA -> BLK

n_to_in FCB     $83                      ; NFA -> 3
        FCB     $3E,$49,$CE              ; ">IN"

c_to_in FDB     do_var                   ; VARIABLE >IN
p_to_in FDB     $0004                    ; 4

; ==================================================================
; STATE
; ==================================================================
l_state FDB     l_span                   ; LFA -> SPAN

n_state FCB     $85                      ; NFA -> 5
        FCB     $53,$54,$41,$54,$C5      ; "STATE"

c_state FDB     do_var                   ; VARIABLE STATE
p_state FDB     $0000                    ; 0

; ==================================================================
; DPL
; ==================================================================
l_dpl   FDB     l_abort                  ; LFA -> (ABORT")

n_dpl   FCB     $83                      ; NFA -> 3
        FCB     $44,$50,$CC              ; "DPL"

c_dpl   FDB     do_var                   ; VARIABLE DPL
p_dpl   FDB     $FFFF                    ; 65535

; ==================================================================
; CSP
; ==================================================================
l_csp   FDB     l_state                  ; LFA -> STATE

n_csp   FCB     $83                      ; NFA -> 3
        FCB     $43,$53,$D0              ; "CSP"

c_csp   FDB     do_var                   ; VARIABLE CSP
p_csp   FDB     $B2EE                    ; 45806

; ==================================================================
; SCR
; ==================================================================
l_scr   FDB     l_csp                    ; LFA -> CSP

n_scr   FCB     $83                      ; NFA -> 3
        FCB     $53,$43,$D2              ; "SCR"

c_scr   FDB     do_var                   ; VARIABLE SCR
p_scr   FDB     $0060                    ; 96

; ==================================================================
; TIB
; ==================================================================
l_tib   FDB     l_dpl                    ; LFA -> DPL

n_tib   FCB     $83                      ; NFA -> 3
        FCB     $54,$49,$C2              ; "TIB"

c_tib   FDB     do_col                   ; : TIB
p_tib   FDB     c_to_ti                  ; >TIB
        FDB     c_at                     ; @
        FDB     c_semi_                  ; (;)

; ==================================================================
; HERE
; ==================================================================
l_here  FDB     l_tib                    ; LFA -> TIB

n_here  FCB     $84                      ; NFA -> 4
        FCB     $48,$45,$52,$C5          ; "HERE"

c_here  FDB     do_col                   ; : HERE
p_here  FDB     c_dp                     ; DP
        FDB     c_at                     ; @
        FDB     c_semi_                  ; (;)

; ==================================================================
; ALLOT
; ==================================================================
l_allot FDB     l_abor0                  ; LFA -> ABORT"

n_allot FCB     $85                      ; NFA -> 5
        FCB     $41,$4C,$4C,$4F,$D4      ; "ALLOT"

c_allot FDB     do_col                   ; : ALLOT
p_allot FDB     c_dp                     ; DP
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; C,
; ==================================================================
l_ckom  FDB     l_scr                    ; LFA -> SCR

n_ckom  FCB     $82                      ; NFA -> 2
        FCB     $43,$AC                  ; "C,"

c_ckom  FDB     do_col                   ; : C,
p_ckom  FDB     c_here                   ; HERE
        FDB     c_cexc                   ; C!
        FDB     c_1                      ; 1
        FDB     c_allot                  ; ALLOT
        FDB     c_semi_                  ; (;)

; ==================================================================
; ,
; ==================================================================
l_kom   FDB     l_here                   ; LFA -> HERE

n_kom   FCB     $81                      ; NFA -> 1
        FCB     $AC                      ; ","

c_kom   FDB     do_col                   ; : ,
p_kom   FDB     c_here                   ; HERE
        FDB     c_exc                    ; !
        FDB     c_2                      ; 2
        FDB     c_allot                  ; ALLOT
        FDB     c_semi_                  ; (;)

; ==================================================================
; DECIMAL
; ==================================================================
l_decim FDB     l_kom                    ; LFA -> ,

n_decim FCB     $87                      ; NFA -> 7
        FCB     $44,$45,$43,$49,$4D,$41  ; "DECIMA"
        FCB     $CC                      ; "L"

c_decim FDB     do_col                   ; : DECIMAL
p_decim FDB     c_lit_
        FDB     $000A                    ; 10
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; HEX
; ==================================================================
l_hex   FDB     l_decim                  ; LFA -> DECIMAL

n_hex   FCB     $83                      ; NFA -> 3
        FCB     $48,$45,$D8              ; "HEX"

c_hex   FDB     do_col                   ; : HEX
p_hex   FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; OCTAL
; ==================================================================
l_octal FDB     l_ckom                   ; LFA -> C,

n_octal FCB     $85                      ; NFA -> 5
        FCB     $4F,$43,$54,$41,$CC      ; "OCTAL"

c_octal FDB     do_col                   ; : OCTAL
p_octal FDB     c_lit_
        FDB     $0008                    ; 8
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; [
; ==================================================================
l_lsq   FDB     l_octal                  ; LFA -> OCTAL

n_lsq   FCB     $C1                      ; NFA -> 1 IMMEDIATE
        FCB     $DB                      ; "["

c_lsq   FDB     do_col                   ; : [
p_lsq   FDB     c_0                      ; 0
        FDB     c_state                  ; STATE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; ]
; ==================================================================
l_rsq   FDB     l_allot                  ; LFA -> ALLOT

n_rsq   FCB     $81                      ; NFA -> 1
        FCB     $DD                      ; "]"

c_rsq   FDB     do_col                   ; : ]
p_rsq   FDB     c_sub1                   ; -1
        FDB     c_state                  ; STATE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?COMP
; ==================================================================
l_qmcom FDB     l_lsq                    ; LFA -> [

n_qmcom FCB     $85                      ; NFA -> 5
        FCB     $3F,$43,$4F,$4D,$D0      ; "?COMP"

c_qmcom FDB     do_col                   ; : ?COMP
p_qmcom FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_abort                  ; (ABORT") len=19
        FCB     $13
        FDB     $5368,$6F75,$6C64,$2062  ; "Should b"
        FDB     $6520,$636F,$6D70,$696C  ; "e compil"
        FCB     $69,$6E,$67              ; "ing"
        FDB     c_semi_                  ; (;)

; ==================================================================
; PAD
; ==================================================================
l_pad   FDB     l_hex                    ; LFA -> HEX

n_pad   FCB     $83                      ; NFA -> 3
        FCB     $50,$41,$C4              ; "PAD"

c_pad   FDB     do_col                   ; : PAD
p_pad   FDB     c_here,c_lit_            ; HERE
        FDB     $0050                    ; 80
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; HOLD
; ==================================================================
l_hold  FDB     l_pad                    ; LFA -> PAD

n_hold  FCB     $84                      ; NFA -> 4
        FCB     $48,$4F,$4C,$C4          ; "HOLD"

c_hold  FDB     do_col                   ; : HOLD
p_hold  FDB     c_sub1                   ; -1
        FDB     c_hld                    ; HLD
        FDB     c_addex                  ; +!
        FDB     c_hld                    ; HLD
        FDB     c_at                     ; @
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; <#
; ==================================================================
l_from2 FDB     l_hold                   ; LFA -> HOLD

n_from2 FCB     $82                      ; NFA -> 2
        FCB     $3C,$A3                  ; "<#"

c_from2 FDB     do_col                   ; : <#
p_from2 FDB     c_pad                    ; PAD
        FDB     c_hld                    ; HLD
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; #>
; ==================================================================
l_n_gt  FDB     l_qmcom                  ; LFA -> ?COMP

n_n_gt  FCB     $82                      ; NFA -> 2
        FCB     $23,$BE                  ; "#>"

c_n_gt  FDB     do_col                   ; : #>
p_n_gt  FDB     c_2drop                  ; 2DROP
        FDB     c_hld                    ; HLD
        FDB     c_at                     ; @
        FDB     c_pad                    ; PAD
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_semi_                  ; (;)

; ==================================================================
; SIGN
; ==================================================================
l_sign  FDB     l_n_gt                   ; LFA -> #>

n_sign  FCB     $84                      ; NFA -> 4
        FCB     $53,$49,$47,$CE          ; "SIGN"

c_sign  FDB     do_col                   ; : SIGN
p_sign  FDB     c_0lt                    ; 0<
        FDB     c_qmbra,Z0B0E,c_lit_     ; ?BRANCH --Z0B0E--v
        FDB     $002D                    ; 45
        FDB     c_hold                   ; HOLD
Z0B0E   FDB     c_semi_                  ; (;)

; ==================================================================
; #
; ==================================================================
l_n_    FDB     l_sign                   ; LFA -> SIGN

n_n_    FCB     $81                      ; NFA -> 1
        FCB     $A3                      ; "#"

c_n_    FDB     do_col                   ; : #
p_n_    FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_to_r                   ; >R
        FDB     c_0                      ; 0
        FDB     c_rat                    ; R@
        FDB     c_umdiv                  ; UM/MOD
        FDB     c_from_                  ; R>
        FDB     c_swap                   ; SWAP
        FDB     c_to_r                   ; >R
        FDB     c_umdiv                  ; UM/MOD
        FDB     c_from_                  ; R>
        FDB     c_rot,c_lit_             ; ROT
        FDB     $0009                    ; 9
        FDB     c_over                   ; OVER
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z0B40,c_lit_     ; ?BRANCH --Z0B40--v
        FDB     $0007                    ; 7
        FDB     c_add                    ; +
Z0B40   FDB     c_lit_
        FDB     $0030                    ; ' EXIT
        FDB     c_add                    ; +
        FDB     c_hold                   ; HOLD
        FDB     c_semi_                  ; (;)

; ==================================================================
; #S
; ==================================================================
l_n_s   FDB     l_n_                     ; LFA -> #

n_n_s   FCB     $82                      ; NFA -> 2
        FCB     $23,$D3                  ; "#S"

c_n_s   FDB     do_col                   ; : #S
p_n_s   FDB     c_n_                     ; #
        FDB     c_2dup                   ; 2DUP
        FDB     c_d0eq                   ; D0=
        FDB     c_qmbra,p_n_s            ; ?BRANCH --p_n_s--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; D.R
; ==================================================================
l_ddotr FDB     l_from2                  ; LFA -> <#

n_ddotr FCB     $83                      ; NFA -> 3
        FCB     $44,$2E,$D2              ; "D.R"

c_ddotr FDB     do_col                   ; : D.R
p_ddotr FDB     c_to_r                   ; >R
        FDB     c_swap                   ; SWAP
        FDB     c_over                   ; OVER
        FDB     c_dabs                   ; DABS
        FDB     c_from2                  ; <#
        FDB     c_n_s                    ; #S
        FDB     c_rot                    ; ROT
        FDB     c_sign                   ; SIGN
        FDB     c_n_gt                   ; #>
        FDB     c_from_                  ; R>
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_spac0                  ; SPACES
        FDB     c_type                   ; TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; D.
; ==================================================================
l_ddot  FDB     l_ddotr                  ; LFA -> D.R

n_ddot  FCB     $82                      ; NFA -> 2
        FCB     $44,$AE                  ; "D."

c_ddot  FDB     do_col                   ; : D.
p_ddot  FDB     c_0                      ; 0
        FDB     c_ddotr                  ; D.R
        FDB     c_space                  ; SPACE
        FDB     c_semi_                  ; (;)

; ==================================================================
; U.R
; ==================================================================
l_udotr FDB     l_rsq                    ; LFA -> ]

n_udotr FCB     $83                      ; NFA -> 3
        FCB     $55,$2E,$D2              ; "U.R"

c_udotr FDB     do_col                   ; : U.R
p_udotr FDB     c_0                      ; 0
        FDB     c_swap                   ; SWAP
        FDB     c_ddotr                  ; D.R
        FDB     c_semi_                  ; (;)

; ==================================================================
; U.
; ==================================================================
l_udot  FDB     l_udotr                  ; LFA -> U.R

n_udot  FCB     $82                      ; NFA -> 2
        FCB     $55,$AE                  ; "U."

c_udot  FDB     do_col                   ; : U.
p_udot  FDB     c_0                      ; 0
        FDB     c_ddot                   ; D.
        FDB     c_semi_                  ; (;)
        FCB     $46

; ==================================================================
; .R
; ==================================================================
l_dotr  FDB     l_to_in                  ; LFA -> >IN

n_dotr  FCB     $82                      ; NFA -> 2
        FCB     $2E,$D2                  ; ".R"

c_dotr  FDB     do_col                   ; : .R
p_dotr  FDB     c_over                   ; OVER
        FDB     c_0lt                    ; 0<
        FDB     c_swap                   ; SWAP
        FDB     c_ddotr                  ; D.R
        FDB     c_semi_                  ; (;)
        FCB     $20

; ==================================================================
; .
; ==================================================================
l_dot   FDB     l_dotr                   ; LFA -> .R

n_dot   FCB     $81                      ; NFA -> 1
        FCB     $AE                      ; "."

c_dot   FDB     do_col                   ; : .
p_dot   FDB     c_0                      ; 0
        FDB     c_dotr                   ; .R
        FDB     c_space                  ; SPACE
        FDB     c_semi_                  ; (;)

; ==================================================================
; DIGIT
; ==================================================================
l_digit FDB     l_ddot                   ; LFA -> D.

n_digit FCB     $85                      ; NFA -> 5
        FCB     $44,$49,$47,$49,$D4      ; "DIGIT"

c_digit FDB     p_digit                  ; ASSEMBLER
p_digit LDB     $03,U
        SUBB    #$30
        SEX
        CMPB    #$0A
        BLT     Z0BE9
        SUBB    #$07
        CMPB    #$0A
        SBCA    #$00
Z0BE9   STD     $02,U
        SUBD    ,U
        BCS     Z0BF2
        LEAU    $02,U
        CLRA
Z0BF2   STA     ,U
        STA     $01,U
        JMP     p_next

; ==================================================================
; CONVERT
; ==================================================================
l_conve FDB     l_n_s                    ; LFA -> #S

n_conve FCB     $87                      ; NFA -> 7
        FCB     $43,$4F,$4E,$56,$45,$52  ; "CONVER"
        FCB     $D4                      ; "T"

c_conve FDB     do_col                   ; : CONVERT
p_conve FDB     c_1add                   ; 1+
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_cat                    ; C@
        FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_digit                  ; DIGIT
        FDB     c_qmbra,Z0C40            ; ?BRANCH --Z0C40--v
        FDB     c_swap                   ; SWAP
        FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_umast                  ; UM*
        FDB     c_drop                   ; DROP
        FDB     c_rot                    ; ROT
        FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_umast                  ; UM*
        FDB     c_dadd                   ; D+
        FDB     c_dpl                    ; DPL
        FDB     c_at                     ; @
        FDB     c_1add                   ; 1+
        FDB     c_qmbra,Z0C3A            ; ?BRANCH --Z0C3A--v
        FDB     c_1                      ; 1
        FDB     c_dpl                    ; DPL
        FDB     c_addex                  ; +!
Z0C3A   FDB     c_from_                  ; R>
        FDB     c_branc,p_conve          ; BRANCH --p_conve--^
Z0C40   FDB     c_from_                  ; R>
        FDB     c_semi_                  ; (;)

; ==================================================================
; (SKP)
; ==================================================================
l_skp_  FDB     l_digit                  ; LFA -> DIGIT

n_skp_  FCB     $85                      ; NFA -> 5
        FCB     $28,$53,$4B,$50,$A9      ; "(SKP)"

c_skp_  FDB     p_skp_                   ; ASSEMBLER
p_skp_  PSHS    Y
        PULU    Y,X,D
        STX     ,--S
        BEQ     Z0C6A
Z0C56   CMPB    ,Y+
        BEQ     Z0C66
        LEAY    -$01,Y
Z0C5C   PSHU    Y,X
        PULS    Y,D
        SUBD    ,U
        PSHU    D
        JMP     p_next
Z0C66   LEAX    -$01,X
        BNE     Z0C56
Z0C6A   BRA     Z0C5C

; ==================================================================
; (SCN)
; ==================================================================
l_scn_  FDB     l_skp_                   ; LFA -> (SKP)

n_scn_  FCB     $85                      ; NFA -> 5
        FCB     $28,$53,$43,$4E,$A9      ; "(SCN)"

c_scn_  FDB     p_scn_                   ; ASSEMBLER
p_scn_  PSHS    Y
        LDA     $01,U
        CLRB
        LDY     $04,U
        LDX     $02,U
        BEQ     Z0C9E
Z0C82   CMPA    ,Y+
        BNE     Z0C9A
        INCB
Z0C87   CLRA
        STD     ,U
        LDD     $02,U
        STX     $02,U
        SUBD    $02,U
        STD     $02,U
        ADDD    ,U
        STD     ,U
        PULS    Y
        JMP     p_next
Z0C9A   LEAX    -$01,X
        BNE     Z0C82
Z0C9E   BRA     Z0C87

; ==================================================================
; STREAM
; ==================================================================
l_strea FDB     l_conve                  ; LFA -> CONVERT

n_strea FCB     $86                      ; NFA -> 6
        FCB     $53,$54,$52,$45,$41,$CD  ; "STREAM"

c_strea FDB     do_col                   ; : STREAM
p_strea FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z0CBF            ; ?BRANCH --Z0CBF--v
        FDB     c_block,c_lit_           ; BLOCK
        FDB     $0400                    ; 1024
        FDB     c_branc,Z0CC5            ; BRANCH --Z0CC5--v
Z0CBF   FDB     c_tib                    ; TIB
        FDB     c_n_tib                  ; #TIB
        FDB     c_at                     ; @
Z0CC5   FDB     c_to_in                  ; >IN
        FDB     c_at                     ; @
        FDB     c_over                   ; OVER
        FDB     c_min                    ; MIN
        FDB     c_swap                   ; SWAP
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_to_r                   ; >R
        FDB     c_add                    ; +
        FDB     c_from_                  ; R>
        FDB     c_semi_                  ; (;)

; ==================================================================
; WORD
; ==================================================================
l_word  FDB     l_strea                  ; LFA -> STREAM

n_word  FCB     $84                      ; NFA -> 4
        FCB     $57,$4F,$52,$C4          ; "WORD"

c_word  FDB     do_col                   ; : WORD
p_word  FDB     c_to_r                   ; >R
        FDB     c_strea                  ; STREAM
        FDB     c_rat                    ; R@
        FDB     c_skp_                   ; (SKP)
        FDB     c_to_in                  ; >IN
        FDB     c_addex                  ; +!
        FDB     c_from_                  ; R>
        FDB     c_scn_                   ; (SCN)
        FDB     c_to_in                  ; >IN
        FDB     c_addex                  ; +!
        FDB     c_here                   ; HERE
        FDB     c_cexc                   ; C!
        FDB     c_here                   ; HERE
        FDB     c_count                  ; COUNT
        FDB     c_cmove                  ; CMOVE
        FDB     c_here                   ; HERE
        FDB     c_bl                     ; BL
        FDB     c_over                   ; OVER
        FDB     c_count                  ; COUNT
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; STRING
; ==================================================================
l_strin FDB     l_word                   ; LFA -> WORD

n_strin FCB     $86                      ; NFA -> 6
        FCB     $53,$54,$52,$49,$4E,$C7  ; "STRING"

c_strin FDB     do_col                   ; : STRING
p_strin FDB     c_strea                  ; STREAM
        FDB     c_rot                    ; ROT
        FDB     c_scn_                   ; (SCN)
        FDB     c_to_in                  ; >IN
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; (
; ==================================================================
l_lp    FDB     l_scn_                   ; LFA -> (SCN)

n_lp    FCB     $C1                      ; NFA -> 1 IMMEDIATE
        FCB     $A8                      ; "("

c_lp    FDB     do_col                   ; : (
p_lp    FDB     c_lit_
        FDB     $0029                    ; 41
        FDB     c_strin                  ; STRING
        FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)
        FCB     $6E

; ==================================================================
; .(
; ==================================================================
l_dotlp FDB     l_dot                    ; LFA -> .

n_dotlp FCB     $C2                      ; NFA -> 2 IMMEDIATE
        FCB     $2E,$A8                  ; ".("

c_dotlp FDB     do_col                   ; : .(
p_dotlp FDB     c_lit_
        FDB     $0029                    ; 41
        FDB     c_strin                  ; STRING
        FDB     c_type                   ; TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; (BS)
; ==================================================================
l_bs_   FDB     l_lp                     ; LFA -> (

n_bs_   FCB     $84                      ; NFA -> 4
        FCB     $28,$42,$53,$A9          ; "(BS)"

c_bs_   FDB     do_col                   ; : (BS)
p_bs_   FDB     c_lit_
        FDB     $0008                    ; 8
        FDB     c_emit                   ; EMIT
        FDB     c_bl                     ; BL
        FDB     c_emit,c_lit_            ; EMIT
        FDB     $0008                    ; 8
        FDB     c_emit                   ; EMIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; (CHR)
; ==================================================================
l_chr_  FDB     l_bs_                    ; LFA -> (BS)

n_chr_  FCB     $85                      ; NFA -> 5
        FCB     $28,$43,$48,$52,$A9      ; "(CHR)"

c_chr_  FDB     do_col                   ; : (CHR)
p_chr_  FDB     c_key                    ; KEY
        FDB     c_dup,c_lit_             ; DUP
        FDB     $001F                    ; 31
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z0D8E            ; ?BRANCH --Z0D8E--v
        FDB     c_dup,c_lit_             ; DUP
        FDB     $007F                    ; 127
        FDB     c_sub                    ; -
        FDB     c_qmbra,Z0D8E            ; ?BRANCH --Z0D8E--v
        FDB     c_dup                    ; DUP
        FDB     c_emit                   ; EMIT
        FDB     c_exit                   ; EXIT
Z0D8E   FDB     c_dup,c_lit_             ; DUP
        FDB     $000D                    ; 13
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z0DA2            ; ?BRANCH --Z0DA2--v
        FDB     c_space                  ; SPACE
        FDB     c_drop                   ; DROP
        FDB     c_0                      ; 0
        FDB     c_exit                   ; EXIT
Z0DA2   FDB     c_dup,c_lit_             ; DUP
        FDB     $0008                    ; 8
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z0DBE            ; ?BRANCH --Z0DBE--v
        FDB     c_drop                   ; DROP
        FDB     c_dup                    ; DUP
        FDB     c_qmbra,Z0DBA            ; ?BRANCH --Z0DBA--v
        FDB     c_bs_                    ; (BS)
        FDB     c_1sub                   ; 1-
Z0DBA   FDB     c_branc,Z0DE0            ; BRANCH --Z0DE0--v
Z0DBE   FDB     c_lit_
        FDB     $0018                    ; 24
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z0DDA            ; ?BRANCH --Z0DDA--v
        FDB     c_0                      ; 0
        FDB     c_qmdo_,Z0DD4            ; (?DO) --Z0DD4--v
Z0DCE   FDB     c_bs_                    ; (BS)
        FDB     c_loop_,Z0DCE            ; (LOOP) --Z0DCE--^
Z0DD4   FDB     c_0                      ; 0
        FDB     c_branc,Z0DE0            ; BRANCH --Z0DE0--v
Z0DDA   FDB     c_lit_
        FDB     $0007                    ; 7
        FDB     c_emit                   ; EMIT
Z0DE0   FDB     c_branc,p_chr_           ; BRANCH --p_chr_--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; EXPECT
; ==================================================================
l_expec FDB     l_udot                   ; LFA -> U.

n_expec FCB     $86                      ; NFA -> 6
        FCB     $45,$58,$50,$45,$43,$D4  ; "EXPECT"

c_expec FDB     do_col                   ; : EXPECT
p_expec FDB     c_expsu                  ; EXP-FLEX
        FDB     c_exit                   ; EXIT
        FDB     c_0                      ; 0
        FDB     c_over                   ; OVER
        FDB     c_qmbra,Z0E19            ; ?BRANCH --Z0E19--v
Z0DFD   FDB     c_chr_                   ; (CHR)
        FDB     c_dup                    ; DUP
        FDB     c_qmbra,Z0E13            ; ?BRANCH --Z0E13--v
        FDB     c_over                   ; OVER
        FDB     c_rat                    ; R@
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_1add                   ; 1+
        FDB     c_2dup                   ; 2DUP
        FDB     c_sub                    ; -
Z0E13   FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z0DFD            ; ?BRANCH --Z0DFD--^
Z0E19   FDB     c_span                   ; SPAN
        FDB     c_exc                    ; !
        FDB     c_from_                  ; R>
        FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; SPACE
; ==================================================================
l_space FDB     l_strin                  ; LFA -> STRING

n_space FCB     $85                      ; NFA -> 5
        FCB     $53,$50,$41,$43,$C5      ; "SPACE"

c_space FDB     do_col                   ; : SPACE
p_space FDB     c_bl                     ; BL
        FDB     c_emit                   ; EMIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; SPACES
; ==================================================================
l_spac0 FDB     l_space                  ; LFA -> SPACE

n_spac0 FCB     $86                      ; NFA -> 6
        FCB     $53,$50,$41,$43,$45,$D3  ; "SPACES"

c_spac0 FDB     do_col                   ; : SPACES
p_spac0 FDB     c_0                      ; 0
        FDB     c_max                    ; MAX
        FDB     c_0                      ; 0
        FDB     c_qmdo_,Z0E4E            ; (?DO) --Z0E4E--v
Z0E48   FDB     c_space                  ; SPACE
        FDB     c_loop_,Z0E48            ; (LOOP) --Z0E48--^
Z0E4E   FDB     c_semi_                  ; (;)

; ==================================================================
; ERASE
; ==================================================================
l_erase FDB     l_expec                  ; LFA -> EXPECT

n_erase FCB     $85                      ; NFA -> 5
        FCB     $45,$52,$41,$53,$C5      ; "ERASE"

c_erase FDB     do_col                   ; : ERASE
p_erase FDB     c_0                      ; 0
        FDB     c_fill                   ; FILL
        FDB     c_semi_                  ; (;)
        FCB     $6F

; ==================================================================
; BLANK
; ==================================================================
l_blank FDB     l_dotlp                  ; LFA -> .(

n_blank FCB     $85                      ; NFA -> 5
        FCB     $42,$4C,$41,$4E,$CB      ; "BLANK"

c_blank FDB     do_col                   ; : BLANK
p_blank FDB     c_bl                     ; BL
        FDB     c_fill                   ; FILL
        FDB     c_semi_                  ; (;)

; ==================================================================
; CR
; ==================================================================
l_cr    FDB     l_spac0                  ; LFA -> SPACES

n_cr    FCB     $82                      ; NFA -> 2
        FCB     $43,$D2                  ; "CR"

c_cr    FDB     do_col                   ; : CR
p_cr    FDB     c_lit_
        FDB     $000D                    ; 13
        FDB     c_emit,c_lit_            ; EMIT
        FDB     $000A                    ; 10
        FDB     c_emit                   ; EMIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; TYPE
; ==================================================================
l_type  FDB     l_chr_                   ; LFA -> (CHR)

n_type  FCB     $84                      ; NFA -> 4
        FCB     $54,$59,$50,$C5          ; "TYPE"

c_type  FDB     do_col                   ; : TYPE
p_type  FDB     c_0                      ; 0
        FDB     c_max                    ; MAX
        FDB     c_0                      ; 0
        FDB     c_qmdo_,Z0EA1            ; (?DO) --Z0EA1--v
Z0E99   FDB     c_count                  ; COUNT
        FDB     c_emit                   ; EMIT
        FDB     c_loop_,Z0E99            ; (LOOP) --Z0E99--^
Z0EA1   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; -TRAILING
; ==================================================================
l_subtr FDB     l_erase                  ; LFA -> ERASE

n_subtr FCB     $89                      ; NFA -> 9
        FDB     $2D54,$5241,$494C,$494E  ; "-TRAILIN"
        FCB     $C7                      ; "G"

c_subtr FDB     do_col                   ; : -TRAILING
p_subtr FDB     c_0                      ; 0
        FDB     c_max                    ; MAX
        FDB     c_dup                    ; DUP
        FDB     c_0                      ; 0
        FDB     c_qmdo_,Z0ED3            ; (?DO) --Z0ED3--v
Z0EBF   FDB     c_2dup                   ; 2DUP
        FDB     c_add                    ; +
        FDB     c_1sub                   ; 1-
        FDB     c_cat                    ; C@
        FDB     c_bl                     ; BL
        FDB     c_sub                    ; -
        FDB     c_qmlea                  ; (?LEAVE)
        FDB     c_1sub                   ; 1-
        FDB     c_loop_,Z0EBF            ; (LOOP) --Z0EBF--^
Z0ED3   FDB     c_semi_                  ; (;)

; ==================================================================
; '
; ==================================================================
l_tck   FDB     l_cr                     ; LFA -> CR

n_tck   FCB     $81                      ; NFA -> 1
        FCB     $A7                      ; "'"

c_tck   FDB     do_col                   ; : '
p_tck   FDB     c_bl                     ; BL
        FDB     c_word                   ; WORD
        FDB     c_find                   ; FIND
        FDB     c_0eq                    ; 0=
        FDB     c_abort                  ; (ABORT") len=11
        FCB     $0B
        FDB     $4E6F,$7420,$6465,$6669  ; "Not defi"
        FCB     $6E,$65,$64              ; "ned"
        FDB     c_semi_                  ; (;)

; ==================================================================
; [']
; ==================================================================
l_lsqtc FDB     l_tck                    ; LFA -> '

n_lsqtc FCB     $C3                      ; NFA -> 3 IMMEDIATE
        FCB     $5B,$27,$DD              ; "[']"

c_lsqtc FDB     do_col                   ; : [']
p_lsqtc FDB     c_tck                    ; '
        FDB     c_liter                  ; LITERAL
        FDB     c_semi_                  ; (;)

; ==================================================================
; [COMPILE]
; ==================================================================
l_lsqco FDB     l_lsqtc                  ; LFA -> [']

n_lsqco FCB     $C9                      ; NFA -> 9 IMMEDIATE
        FDB     $5B43,$4F4D,$5049,$4C45  ; "[COMPILE"
        FCB     $DD                      ; "]"

c_lsqco FDB     do_col                   ; : [COMPILE]
p_lsqco FDB     c_qmcom                  ; ?COMP
        FDB     c_tck                    ; '
        FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)

; ==================================================================
; 0
; ==================================================================
l_0     FDB     l_type                   ; LFA -> TYPE

n_0     FCB     $81                      ; NFA -> 1
        FCB     $B0                      ; "0"

c_0     FDB     do_const                 ; CONSTANT 0
p_0     FDB     $0000                    ; 0

; ==================================================================
; 1
; ==================================================================
l_1     FDB     l_subtr                  ; LFA -> -TRAILING

n_1     FCB     $81                      ; NFA -> 1
        FCB     $B1                      ; "1"

c_1     FDB     do_const                 ; CONSTANT 1
p_1     FDB     $0001                    ; 1
        FCB     $73

; ==================================================================
; 2
; ==================================================================
l_2     FDB     l_blank                  ; LFA -> BLANK

n_2     FCB     $81                      ; NFA -> 1
        FCB     $B2                      ; "2"

c_2     FDB     do_const                 ; CONSTANT 2
p_2     FDB     $0002                    ; 2

; ==================================================================
; -1
; ==================================================================
l_sub1  FDB     l_1                      ; LFA -> 1

n_sub1  FCB     $82                      ; NFA -> 2
        FCB     $2D,$B1                  ; "-1"

c_sub1  FDB     do_const                 ; CONSTANT -1
p_sub1  FDB     $FFFF                    ; 65535
        FCB     $6C

; ==================================================================
; BL
; ==================================================================
l_bl    FDB     l_2                      ; LFA -> 2

n_bl    FCB     $82                      ; NFA -> 2
        FCB     $42,$CC                  ; "BL"

c_bl    FDB     do_const                 ; CONSTANT BL
p_bl    FDB     $0020                    ; 32

; ==================================================================
; ?STACK
; ==================================================================
l_qmsta FDB     l_lsqco                  ; LFA -> [COMPILE]

n_qmsta FCB     $86                      ; NFA -> 6
        FCB     $3F,$53,$54,$41,$43,$CB  ; "?STACK"

c_qmsta FDB     do_col                   ; : ?STACK
p_qmsta FDB     c_spat                   ; SP@
        FDB     c_s0                     ; S0
        FDB     c_at                     ; @
        FDB     c_swap                   ; SWAP
        FDB     c_ult                    ; U<
        FDB     c_abort                  ; (ABORT") len=15
        FCB     $0F
        FDB     $5374,$6163,$6B20,$756E  ; "Stack un"
        FCB     $64,$65,$72,$66,$6C,$6F  ; "derflo"
        FCB     $77                      ; "w"
        FDB     c_spat                   ; SP@
        FDB     c_pad,c_lit_             ; PAD
        FDB     $0080                    ; 128
        FDB     c_add                    ; +
        FDB     c_ult                    ; U<
        FDB     c_abort                  ; (ABORT") len=14
        FCB     $0E
        FDB     $5374,$6163,$6B20,$6F76  ; "Stack ov"
        FCB     $65,$72,$66,$6C,$6F,$77  ; "erflow"
        FDB     c_semi_                  ; (;)
        FCB     $69

; ==================================================================
; NUMBER
; ==================================================================
l_numbe FDB     l_bl                     ; LFA -> BL

n_numbe FCB     $86                      ; NFA -> 6
        FCB     $4E,$55,$4D,$42,$45,$D2  ; "NUMBER"

c_numbe FDB     do_col                   ; : NUMBER
p_numbe FDB     c_0                      ; 0
        FDB     c_0                      ; 0
        FDB     c_rot                    ; ROT
        FDB     c_dup                    ; DUP
        FDB     c_1add                   ; 1+
        FDB     c_cat,c_lit_             ; C@
        FDB     $002D                    ; 45
        FDB     c_eq                     ; =
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_sub                    ; -
        FDB     c_sub1                   ; -1
Z0FAF   FDB     c_dpl                    ; DPL
        FDB     c_exc                    ; !
        FDB     c_conve                  ; CONVERT
        FDB     c_dup                    ; DUP
        FDB     c_cat                    ; C@
        FDB     c_bl                     ; BL
        FDB     c_sub                    ; -
        FDB     c_qmbra,Z0FE2            ; ?BRANCH --Z0FE2--v
        FDB     c_dup                    ; DUP
        FDB     c_cat,c_lit_             ; C@
        FDB     $002E                    ; 46
        FDB     c_sub                    ; -
        FDB     c_abort                  ; (ABORT") len=14
        FCB     $0E
        FDB     $4E6F,$7420,$7265,$636F  ; "Not reco"
        FCB     $67,$6E,$69,$7A,$65,$64  ; "gnized"
        FDB     c_0                      ; 0
        FDB     c_branc,Z0FAF            ; BRANCH --Z0FAF--^
Z0FE2   FDB     c_drop                   ; DROP
        FDB     c_from_                  ; R>
        FDB     c_qmbra,Z0FEC            ; ?BRANCH --Z0FEC--v
        FDB     c_dnega                  ; DNEGATE
Z0FEC   FDB     c_semi_                  ; (;)

; ==================================================================
; INTERPRET
; ==================================================================
l_inter FDB     l_sub1                   ; LFA -> -1

n_inter FCB     $89                      ; NFA -> 9
        FDB     $494E,$5445,$5250,$5245  ; "INTERPRE"
        FCB     $D4                      ; "T"

c_inter FDB     do_col                   ; : INTERPRET
p_inter FDB     c_bl                     ; BL
        FDB     c_word                   ; WORD
        FDB     c_dup                    ; DUP
        FDB     c_cat                    ; C@
        FDB     c_qmbra,Z1058            ; ?BRANCH --Z1058--v
        FDB     c_find                   ; FIND
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z1026            ; ?BRANCH --Z1026--v
        FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z1020            ; ?BRANCH --Z1020--v
        FDB     c_kom                    ; ,
        FDB     c_branc,Z1022            ; BRANCH --Z1022--v
Z1020   FDB     c_execu                  ; EXECUTE
Z1022   FDB     c_branc,Z1052            ; BRANCH --Z1052--v
Z1026   FDB     c_numbe                  ; NUMBER
        FDB     c_dpl                    ; DPL
        FDB     c_at                     ; @
        FDB     c_1add                   ; 1+
        FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_qmbra,Z104A            ; ?BRANCH --Z104A--v
        FDB     c_qmbra,Z1042            ; ?BRANCH --Z1042--v
        FDB     c_swap                   ; SWAP
        FDB     c_liter                  ; LITERAL
        FDB     c_branc,Z1044            ; BRANCH --Z1044--v
Z1042   FDB     c_drop                   ; DROP
Z1044   FDB     c_liter                  ; LITERAL
        FDB     c_branc,Z1052            ; BRANCH --Z1052--v
Z104A   FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z1052            ; ?BRANCH --Z1052--v
        FDB     c_drop                   ; DROP
Z1052   FDB     c_qmsta                  ; ?STACK
        FDB     c_branc,p_inter          ; BRANCH --p_inter--^
Z1058   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; QUERY
; ==================================================================
l_query FDB     l_inter                  ; LFA -> INTERPRET

n_query FCB     $85                      ; NFA -> 5
        FCB     $51,$55,$45,$52,$D9      ; "QUERY"

c_query FDB     do_col                   ; : QUERY
p_query FDB     c_0                      ; 0
        FDB     c_blk                    ; BLK
        FDB     c_exc                    ; !
        FDB     c_tib,c_lit_             ; TIB
        FDB     $0050                    ; 80
        FDB     c_expec                  ; EXPECT
        FDB     c_span                   ; SPAN
        FDB     c_at                     ; @
        FDB     c_n_tib                  ; #TIB
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_to_in                  ; >IN
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; LOAD
; ==================================================================
l_load  FDB     l_0                      ; LFA -> 0

n_load  FCB     $84                      ; NFA -> 4
        FCB     $4C,$4F,$41,$C4          ; "LOAD"

c_load  FDB     do_col                   ; : LOAD
p_load  FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_to_r                   ; >R
        FDB     c_to_in                  ; >IN
        FDB     c_at                     ; @
        FDB     c_to_r                   ; >R
        FDB     c_0                      ; 0
        FDB     c_to_in                  ; >IN
        FDB     c_exc                    ; !
        FDB     c_blk                    ; BLK
        FDB     c_exc                    ; !
        FDB     c_inter                  ; INTERPRET
        FDB     c_from_                  ; R>
        FDB     c_to_in                  ; >IN
        FDB     c_exc                    ; !
        FDB     c_from_                  ; R>
        FDB     c_blk                    ; BLK
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; -->
; ==================================================================
l_subsu FDB     l_query                  ; LFA -> QUERY

n_subsu FCB     $C3                      ; NFA -> 3 IMMEDIATE
        FCB     $2D,$2D,$BE              ; "-->"

c_subsu FDB     do_col                   ; : -->
p_subsu FDB     c_1                      ; 1
        FDB     c_blk                    ; BLK
        FDB     c_addex                  ; +!
        FDB     c_0                      ; 0
        FDB     c_to_in                  ; >IN
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; THRU
; ==================================================================
l_thru  FDB     l_load                   ; LFA -> LOAD

n_thru  FCB     $84                      ; NFA -> 4
        FCB     $54,$48,$52,$D5          ; "THRU"

c_thru  FDB     do_col                   ; : THRU
p_thru  FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z10E2              ; (DO) --Z10E2--v
Z10DA   FDB     c_i                      ; I
        FDB     c_load                   ; LOAD
        FDB     c_loop_,Z10DA            ; (LOOP) --Z10DA--^
Z10E2   FDB     c_semi_                  ; (;)

; ==================================================================
; 'PROMPT
; ==================================================================
l_tckpr FDB     l_qmsta                  ; LFA -> ?STACK

n_tckpr FCB     $87                      ; NFA -> 7
        FCB     $27,$50,$52,$4F,$4D,$50  ; "'PROMP"
        FCB     $D4                      ; "T"

c_tckpr FDB     do_var                   ; VARIABLE 'PROMPT
p_tckpr FDB     $0E76                    ; 3702

; ==================================================================
; QUIT
; ==================================================================
l_quit  FDB     l_subsu                  ; LFA -> -->

n_quit  FCB     $84                      ; NFA -> 4
        FCB     $51,$55,$49,$D4          ; "QUIT"

c_quit  FDB     do_col                   ; : QUIT
p_quit  FDB     c_lsq                    ; [
        FDB     c_termi                  ; TERMINAL
Z10FF   FDB     c_rpexc                  ; RP!
        FDB     c_s0                     ; S0
        FDB     c_at                     ; @
        FDB     c_2add                   ; 2+
        FDB     c_to_ti                  ; >TIB
        FDB     c_exc                    ; !
        FDB     c_tckpr                  ; 'PROMPT
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_query                  ; QUERY
        FDB     c_inter                  ; INTERPRET
        FDB     c_termi                  ; TERMINAL
        FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z1127            ; ?BRANCH --Z1127--v
        FDB     c_dotqt                  ; (.") len=3
        FCB     $03
        FCB     $20,$6F,$6B              ; " ok"
Z1127   FDB     c_branc,Z10FF            ; BRANCH --Z10FF--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; 'ABORT
; ==================================================================
l_tckab FDB     l_tckpr                  ; LFA -> 'PROMPT

n_tckab FCB     $86                      ; NFA -> 6
        FCB     $27,$41,$42,$4F,$52,$D4  ; "'ABORT"

c_tckab FDB     do_var                   ; VARIABLE 'ABORT
p_tckab FDB     $001D                    ; 29

; ==================================================================
; ABORT
; ==================================================================
l_abor1 FDB     l_quit                   ; LFA -> QUIT

n_abor1 FCB     $85                      ; NFA -> 5
        FCB     $41,$42,$4F,$52,$D4      ; "ABORT"

c_abor1 FDB     do_col                   ; : ABORT
p_abor1 FDB     c_tckab                  ; 'ABORT
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_spexc                  ; SP!
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)
        FCB     $57

; ==================================================================
; FORTH-83
; ==================================================================
l_forth FDB     l_numbe                  ; LFA -> NUMBER

n_forth FCB     $88                      ; NFA -> 8
        FDB     $464F,$5254,$482D,$38B3  ; "FORTH-83"

c_forth FDB     do_col                   ; : FORTH-83
p_forth FDB     c_semi_                  ; (;)

; ==================================================================
; (;CODE)
; ==================================================================
l_semic FDB     l_thru                   ; LFA -> THRU

n_semic FCB     $87                      ; NFA -> 7
        FCB     $28,$3B,$43,$4F,$44,$45  ; "(;CODE"
        FCB     $A9                      ; ")"

c_semic FDB     do_col                   ; : (;CODE)
p_semic FDB     c_from_                  ; R>
        FDB     c_last                   ; LAST
        FDB     c_at                     ; @
        FDB     c_nameg                  ; NAME>
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; VARIABLE
; ==================================================================
l_varia FDB     l_forth                  ; LFA -> FORTH-83

n_varia FCB     $88                      ; NFA -> 8
        FDB     $5641,$5249,$4142,$4CC5  ; "VARIABLE"

c_varia FDB     do_col                   ; : VARIABLE
p_varia FDB     c_creat                  ; CREATE
        FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)

; ==================================================================
; 2VARIABLE
; ==================================================================
l_2vari FDB     l_varia                  ; LFA -> VARIABLE

n_2vari FCB     $89                      ; NFA -> 9
        FDB     $3256,$4152,$4941,$424C  ; "2VARIABL"
        FCB     $C5                      ; "E"

c_2vari FDB     do_col                   ; : 2VARIABLE
p_2vari FDB     c_varia                  ; VARIABLE
        FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)

; ==================================================================
; CONSTANT
; ==================================================================
l_const FDB     l_tckab                  ; LFA -> 'ABORT

n_const FCB     $88                      ; NFA -> 8
        FDB     $434F,$4E53,$5441,$4ED4  ; "CONSTANT"

c_const FDB     do_col                   ; : CONSTANT
p_const FDB     c_creat                  ; CREATE
        FDB     c_kom                    ; ,
        FDB     c_semic                  ; (;CODE)
do_const LDD     $02,X
        PSHU    D
        JMP     p_next

; ==================================================================
; 2CONSTANT
; ==================================================================
l_2cons FDB     l_2vari                  ; LFA -> 2VARIABLE

n_2cons FCB     $89                      ; NFA -> 9
        FDB     $3243,$4F4E,$5354,$414E  ; "2CONSTAN"
        FCB     $D4                      ; "T"

c_2cons FDB     do_col                   ; : 2CONSTANT
p_2cons FDB     c_creat                  ; CREATE
        FDB     c_kom                    ; ,
        FDB     c_kom                    ; ,
        FDB     c_semic                  ; (;CODE)
do_2cst JMP     do_does
        FDB     c_2at                    ; 2@
        FDB     c_semi_                  ; (;)

; ==================================================================
; :
; ==================================================================
l_col   FDB     l_2cons                  ; LFA -> 2CONSTANT

n_col   FCB     $81                      ; NFA -> 1
        FCB     $BA                      ; ":"

c_col   FDB     do_col                   ; : :
p_col   FDB     c_spat                   ; SP@
        FDB     c_csp                    ; CSP
        FDB     c_exc                    ; !
        FDB     c_curre                  ; CURRENT
        FDB     c_at                     ; @
        FDB     c_conte                  ; CONTEXT
        FDB     c_exc                    ; !
        FDB     c_creat                  ; CREATE
        FDB     c_hide                   ; HIDE
        FDB     c_rsq                    ; ]
        FDB     c_semic                  ; (;CODE)
do_col  PSHS    Y
        LEAY    $02,X
        JMP     p_next

; ==================================================================
; ?CSP
; ==================================================================
l_qmcsp FDB     l_const                  ; LFA -> CONSTANT

n_qmcsp FCB     $84                      ; NFA -> 4
        FCB     $3F,$43,$53,$D0          ; "?CSP"

c_qmcsp FDB     do_col                   ; : ?CSP
p_qmcsp FDB     c_spat                   ; SP@
        FDB     c_csp                    ; CSP
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_abort                  ; (ABORT") len=15
        FCB     $0F
        FDB     $5374,$6163,$6B20,$6578  ; "Stack ex"
        FCB     $63,$65,$70,$74,$69,$6F  ; "ceptio"
        FCB     $6E                      ; "n"
        FDB     c_semi_                  ; (;)

; ==================================================================
; ;
; ==================================================================
l_semi  FDB     l_qmcsp                  ; LFA -> ?CSP

n_semi  FCB     $C1                      ; NFA -> 1 IMMEDIATE
        FCB     $BB                      ; ";"

c_semi  FDB     do_col                   ; : ;
p_semi  FDB     c_qmcom                  ; ?COMP
        FDB     c_qmcsp                  ; ?CSP
        FDB     c_compi                  ; COMPILE
        FDB     c_semi_                  ; (;)
        FDB     c_revea                  ; REVEAL
        FDB     c_lsq                    ; [
        FDB     c_semi_                  ; (;)

; ==================================================================
; DOES>
; ==================================================================
l_doesg FDB     l_semic                  ; LFA -> (;CODE)

n_doesg FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $44,$4F,$45,$53,$BE      ; "DOES>"

c_doesg FDB     do_col                   ; : DOES>
p_doesg FDB     c_qmcom                  ; ?COMP
        FDB     c_compi                  ; COMPILE
        FDB     c_semic,c_lit_           ; (;CODE)
        FDB     $007E                    ; 126
        FDB     c_ckom,c_lit_            ; C,
        FDB     $1252                    ; 4690
        FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)
do_does PSHS    Y
        LDY     ,X++
        PSHU    X
        LEAY    $03,Y
        JMP     p_next

; ==================================================================
; LIST
; ==================================================================
l_list  FDB     l_doesg                  ; LFA -> DOES>

n_list  FCB     $84                      ; NFA -> 4
        FCB     $4C,$49,$53,$D4          ; "LIST"

c_list  FDB     do_col                   ; : LIST
p_list  FDB     c_decim                  ; DECIMAL
        FDB     c_cr                     ; CR
        FDB     c_dup                    ; DUP
        FDB     c_scr                    ; SCR
        FDB     c_exc                    ; !
        FDB     c_dotqt                  ; (.") len=6
        FCB     $06
        FCB     $53,$43,$52,$20,$23,$20  ; "SCR # "
        FDB     c_dot,c_lit_             ; .
        FDB     $0010                    ; 16
        FDB     c_0                      ; 0
        FDB     c_do_,Z12B9              ; (DO) --Z12B9--v
Z1285   FDB     c_cr                     ; CR
        FDB     c_i,c_lit_               ; I
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_space                  ; SPACE
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_block                  ; BLOCK
        FDB     c_i,c_lit_               ; I
        FDB     $0040                    ; 64
        FDB     c_ast                    ; *
        FDB     c_add,c_lit_             ; +
        FDB     $0040                    ; 64
        FDB     c_subtr                  ; -TRAILING
        FDB     c_to_r                   ; >R
        FDB     c_pad                    ; PAD
        FDB     c_rat                    ; R@
        FDB     c_cmove                  ; CMOVE
        FDB     c_pad                    ; PAD
        FDB     c_from_                  ; R>
        FDB     c_type                   ; TYPE
        FDB     c_loop_,Z1285            ; (LOOP) --Z1285--^
Z12B9   FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; CURRENT
; ==================================================================
l_curre FDB     l_semi                   ; LFA -> ;

n_curre FCB     $87                      ; NFA -> 7
        FCB     $43,$55,$52,$52,$45,$4E  ; "CURREN"
        FCB     $D4                      ; "T"

c_curre FDB     do_var                   ; VARIABLE CURRENT
p_curre FDB     $16F1                    ; 5873

; ==================================================================
; CONTEXT
; ==================================================================
l_conte FDB     l_curre                  ; LFA -> CURRENT

n_conte FCB     $87                      ; NFA -> 7
        FCB     $43,$4F,$4E,$54,$45,$58  ; "CONTEX"
        FCB     $D4                      ; "T"

c_conte FDB     do_var                   ; VARIABLE CONTEXT
p_conte FDB     $16F1                    ; 5873
        FCB     $16,$F1,$00,$00,$00,$00,$20,$12

; ==================================================================
; VOC-LINK
; ==================================================================
l_vocsu FDB     l_col                    ; LFA -> :

n_vocsu FCB     $88                      ; NFA -> 8
        FDB     $564F,$432D,$4C49,$4ECB  ; "VOC-LINK"

c_vocsu FDB     do_var                   ; VARIABLE VOC-LINK
p_vocsu FDB     $2DCF                    ; 11727

; ==================================================================
; LAST
; ==================================================================
l_last  FDB     l_list                   ; LFA -> LIST

n_last  FCB     $84                      ; NFA -> 4
        FCB     $4C,$41,$53,$D4          ; "LAST"

c_last  FDB     do_var                   ; VARIABLE LAST
p_last  FDB     $8A8D                    ; 35469

; ==================================================================
; FENCE
; ==================================================================
l_fence FDB     l_vocsu                  ; LFA -> VOC-LINK

n_fence FCB     $85                      ; NFA -> 5
        FCB     $46,$45,$4E,$43,$C5      ; "FENCE"

c_fence FDB     do_var                   ; VARIABLE FENCE
p_fence FDB     $2246                    ; 8774

; ==================================================================
; WIDTH
; ==================================================================
l_width FDB     l_conte                  ; LFA -> CONTEXT

n_width FCB     $85                      ; NFA -> 5
        FCB     $57,$49,$44,$54,$C8      ; "WIDTH"

c_width FDB     do_var                   ; VARIABLE WIDTH
p_width FDB     $001F                    ; 31

; ==================================================================
; WARNING
; ==================================================================
l_warni FDB     l_width                  ; LFA -> WIDTH

n_warni FCB     $87                      ; NFA -> 7
        FCB     $57,$41,$52,$4E,$49,$4E  ; "WARNIN"
        FCB     $C7                      ; "G"

c_warni FDB     do_var                   ; VARIABLE WARNING
p_warni FDB     $FFFF                    ; 65535

; ==================================================================
; (FIND)
; ==================================================================
l_find_ FDB     l_last                   ; LFA -> LAST

n_find_ FCB     $86                      ; NFA -> 6
        FCB     $28,$46,$49,$4E,$44,$A9  ; "(FIND)"

c_find_ FDB     p_find_                  ; ASSEMBLER
p_find_ PSHS    Y
Z132E   LDX     [,U]
        STX     ,U
        BNE     Z1338
Z1334   PULS    Y
        JMP     p_next
Z1338   LDY     $02,U
        LEAX    $02,X
        LDA     ,X+
        EORA    ,Y+
        BITA    #$3F
        BNE     Z135A
Z1345   LDB     ,X+
        EORB    ,Y+
        BEQ     Z1345
        ASLB
        BNE     Z135A
        DECB
        ROLA
        BPL     Z1353
        NEGB
Z1353   SEX
        STD     ,U
        STX     $02,U
        BRA     Z1334
Z135A   BRA     Z132E

; ==================================================================
; ?CAP
; ==================================================================
l_qmcap FDB     l_warni                  ; LFA -> WARNING

n_qmcap FCB     $84                      ; NFA -> 4
        FCB     $3F,$43,$41,$D0          ; "?CAP"

c_qmcap FDB     do_var                   ; VARIABLE ?CAP
p_qmcap FDB     $0001                    ; 1

; ==================================================================
; CAPS
; ==================================================================
l_caps  FDB     l_qmcap                  ; LFA -> ?CAP

n_caps  FCB     $84                      ; NFA -> 4
        FCB     $43,$41,$50,$D3          ; "CAPS"

c_caps  FDB     p_caps                   ; ASSEMBLER
p_caps  LDX     ,U++
        PSHS    Y
        PULU    Y
        BEQ     Z138A
Z1378   LDA     ,Y+
        CMPA    #$60
        BLS     Z1386
        CMPA    #$7A
        BHI     Z1386
        SUBA    #$20
        STA     -$01,Y
Z1386   LEAX    -$01,X
        BNE     Z1378
Z138A   PULS    Y
        JMP     p_next

; ==================================================================
; >THREAD
; ==================================================================
l_to_th FDB     l_fence                  ; LFA -> FENCE

n_to_th FCB     $87                      ; NFA -> 7
        FCB     $3E,$54,$48,$52,$45,$41  ; ">THREA"
        FCB     $C4                      ; "D"

c_to_th FDB     p_to_th                  ; ASSEMBLER
p_to_th LDX     $02,U
        CLRA
        LDB     $01,X
        ANDB    #$03
        ASLB
        ADDD    ,U
        STD     ,U
        JMP     p_next

; ==================================================================
; FIND
; ==================================================================
l_find  FDB     l_to_th                  ; LFA -> >THREAD

n_find  FCB     $84                      ; NFA -> 4
        FCB     $46,$49,$4E,$C4          ; "FIND"

c_find  FDB     do_col                   ; : FIND
p_find  FDB     c_qmcap                  ; ?CAP
        FDB     c_at                     ; @
        FDB     c_qmbra,Z13BF            ; ?BRANCH --Z13BF--v
        FDB     c_dup                    ; DUP
        FDB     c_count                  ; COUNT
        FDB     c_caps                   ; CAPS
Z13BF   FDB     c_0                      ; 0
        FDB     c_over                   ; OVER
        FDB     c_cat,c_lit_             ; C@
        FDB     $0020                    ; 32
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z1409            ; ?BRANCH --Z1409--v
        FDB     c_conte,c_lit_           ; CONTEXT
        FDB     $000A                    ; 10
        FDB     c_add                    ; +
        FDB     c_conte                  ; CONTEXT
        FDB     c_dup                    ; DUP
        FDB     c_at                     ; @
        FDB     c_over                   ; OVER
        FDB     c_2add                   ; 2+
        FDB     c_at                     ; @
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z13EB            ; ?BRANCH --Z13EB--v
        FDB     c_2add                   ; 2+
Z13EB   FDB     c_do_,Z1409              ; (DO) --Z1409--v
Z13EF   FDB     c_drop                   ; DROP
        FDB     c_i                      ; I
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_qmbra,Z1403            ; ?BRANCH --Z1403--v
        FDB     c_to_th                  ; >THREAD
        FDB     c_find_                  ; (FIND)
        FDB     c_dup                    ; DUP
        FDB     c_qmlea                  ; (?LEAVE)
Z1403   FDB     c_2                      ; 2
        FDB     c_addlo,Z13EF            ; (+LOOP) --Z13EF--^
Z1409   FDB     c_semi_                  ; (;)

; ==================================================================
; >BODY
; ==================================================================
l_to_bo FDB     l_find                   ; LFA -> FIND

n_to_bo FCB     $85                      ; NFA -> 5
        FCB     $3E,$42,$4F,$44,$D9      ; ">BODY"

c_to_bo FDB     do_col                   ; : >BODY
p_to_bo FDB     c_2add                   ; 2+
        FDB     c_semi_                  ; (;)

; ==================================================================
; TRAVERSE
; ==================================================================
l_trave FDB     l_find_                  ; LFA -> (FIND)

n_trave FCB     $88                      ; NFA -> 8
        FDB     $5452,$4156,$4552,$53C5  ; "TRAVERSE"

c_trave FDB     do_col                   ; : TRAVERSE
p_trave FDB     c_swap                   ; SWAP
Z1428   FDB     c_over                   ; OVER
        FDB     c_add,c_lit_             ; +
        FDB     $007F                    ; 127
        FDB     c_over                   ; OVER
        FDB     c_cat                    ; C@
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z1428            ; ?BRANCH --Z1428--^
        FDB     c_swap                   ; SWAP
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; BODY>
; ==================================================================
l_bodyg FDB     l_to_bo                  ; LFA -> >BODY

n_bodyg FCB     $85                      ; NFA -> 5
        FCB     $42,$4F,$44,$59,$BE      ; "BODY>"

c_bodyg FDB     do_col                   ; : BODY>
p_bodyg FDB     c_2sub                   ; 2-
        FDB     c_semi_                  ; (;)

; ==================================================================
; >NAME
; ==================================================================
l_to_na FDB     l_bodyg                  ; LFA -> BODY>

n_to_na FCB     $85                      ; NFA -> 5
        FCB     $3E,$4E,$41,$4D,$C5      ; ">NAME"

c_to_na FDB     do_col                   ; : >NAME
p_to_na FDB     c_1sub                   ; 1-
        FDB     c_sub1                   ; -1
        FDB     c_trave                  ; TRAVERSE
        FDB     c_semi_                  ; (;)

; ==================================================================
; NAME>
; ==================================================================
l_nameg FDB     l_to_na                  ; LFA -> >NAME

n_nameg FCB     $85                      ; NFA -> 5
        FCB     $4E,$41,$4D,$45,$BE      ; "NAME>"

c_nameg FDB     do_col                   ; : NAME>
p_nameg FDB     c_1                      ; 1
        FDB     c_trave                  ; TRAVERSE
        FDB     c_1add                   ; 1+
        FDB     c_semi_                  ; (;)

; ==================================================================
; N>LINK
; ==================================================================
l_ngtli FDB     l_nameg                  ; LFA -> NAME>

n_ngtli FCB     $86                      ; NFA -> 6
        FCB     $4E,$3E,$4C,$49,$4E,$CB  ; "N>LINK"

c_ngtli FDB     do_col                   ; : N>LINK
p_ngtli FDB     c_2sub                   ; 2-
        FDB     c_semi_                  ; (;)

; ==================================================================
; L>NAME
; ==================================================================
l_lgtna FDB     l_trave                  ; LFA -> TRAVERSE

n_lgtna FCB     $86                      ; NFA -> 6
        FCB     $4C,$3E,$4E,$41,$4D,$C5  ; "L>NAME"

c_lgtna FDB     do_col                   ; : L>NAME
p_lgtna FDB     c_2add                   ; 2+
        FDB     c_semi_                  ; (;)

; ==================================================================
; >LINK
; ==================================================================
l_to_li FDB     l_ngtli                  ; LFA -> N>LINK

n_to_li FCB     $85                      ; NFA -> 5
        FCB     $3E,$4C,$49,$4E,$CB      ; ">LINK"

c_to_li FDB     do_col                   ; : >LINK
p_to_li FDB     c_to_na                  ; >NAME
        FDB     c_ngtli                  ; N>LINK
        FDB     c_semi_                  ; (;)

; ==================================================================
; LINK>
; ==================================================================
l_linkg FDB     l_lgtna                  ; LFA -> L>NAME

n_linkg FCB     $85                      ; NFA -> 5
        FCB     $4C,$49,$4E,$4B,$BE      ; "LINK>"

c_linkg FDB     do_col                   ; : LINK>
p_linkg FDB     c_lgtna                  ; L>NAME
        FDB     c_nameg                  ; NAME>
        FDB     c_semi_                  ; (;)

; ==================================================================
; ID.
; ==================================================================
l_iddot FDB     l_abor1                  ; LFA -> ABORT

n_iddot FCB     $83                      ; NFA -> 3
        FCB     $49,$44,$AE              ; "ID."

c_iddot FDB     do_col                   ; : ID.
p_iddot FDB     c_pad,c_lit_             ; PAD
        FDB     $0020,c_lit_             ; 32
        FDB     $005F                    ; 95
        FDB     c_fill                   ; FILL
        FDB     c_dup                    ; DUP
        FDB     c_nameg                  ; NAME>
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_pad                    ; PAD
        FDB     c_swap                   ; SWAP
        FDB     c_2dup                   ; 2DUP
        FDB     c_add                    ; +
        FDB     c_1sub                   ; 1-
        FDB     c_to_r                   ; >R
        FDB     c_cmove                  ; CMOVE
        FDB     c_rat                    ; R@
        FDB     c_cat,c_lit_             ; C@
        FDB     $007F                    ; 127
        FDB     c_and                    ; AND
        FDB     c_from_                  ; R>
        FDB     c_cexc                   ; C!
        FDB     c_pad                    ; PAD
        FDB     c_count,c_lit_           ; COUNT
        FDB     $001F                    ; 31
        FDB     c_and                    ; AND
        FDB     c_type                   ; TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; WORDS
; ==================================================================
l_words FDB     l_caps                   ; LFA -> CAPS

n_words FCB     $85                      ; NFA -> 5
        FCB     $57,$4F,$52,$44,$D3      ; "WORDS"

c_words FDB     do_col                   ; : WORDS
p_words FDB     c_lit_
        FDB     $0041                    ; 65
        FDB     c_out                    ; OUT
        FDB     c_exc                    ; !
        FDB     c_conte                  ; CONTEXT
        FDB     c_at                     ; @
        FDB     c_here,c_lit_            ; HERE
        FDB     $0008                    ; 8
        FDB     c_cmove                  ; CMOVE
Z1514   FDB     c_here                   ; HERE
        FDB     c_0                      ; 0
        FDB     c_here,c_lit_            ; HERE
        FDB     $0008                    ; 8
        FDB     c_add                    ; +
        FDB     c_here                   ; HERE
        FDB     c_do_,Z1540              ; (DO) --Z1540--v
Z1526   FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_at                     ; @
        FDB     c_ult                    ; U<
        FDB     c_qmbra,Z153A            ; ?BRANCH --Z153A--v
        FDB     c_2drop                  ; 2DROP
        FDB     c_i                      ; I
        FDB     c_dup                    ; DUP
        FDB     c_at                     ; @
Z153A   FDB     c_2                      ; 2
        FDB     c_addlo,Z1526            ; (+LOOP) --Z1526--^
Z1540   FDB     c_dup                    ; DUP
        FDB     c_at                     ; @
        FDB     c_rot                    ; ROT
        FDB     c_exc                    ; !
        FDB     c_dup                    ; DUP
        FDB     c_qmkey                  ; ?KEY
        FDB     c_0eq                    ; 0=
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z1578            ; ?BRANCH --Z1578--v
        FDB     c_out                    ; OUT
        FDB     c_at,c_lit_              ; @
        FDB     $0040                    ; 64
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z156E            ; ?BRANCH --Z156E--v
        FDB     c_cr                     ; CR
        FDB     c_0                      ; 0
        FDB     c_out                    ; OUT
        FDB     c_exc                    ; !
        FDB     c_branc,Z1570            ; BRANCH --Z1570--v
Z156E   FDB     c_space                  ; SPACE
Z1570   FDB     c_lgtna                  ; L>NAME
        FDB     c_iddot                  ; ID.
        FDB     c_branc,Z1514            ; BRANCH --Z1514--^
Z1578   FDB     c_drop                   ; DROP
        FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; (FORGET)
; ==================================================================
l_forge FDB     l_linkg                  ; LFA -> LINK>

n_forge FCB     $88                      ; NFA -> 8
        FDB     $2846,$4F52,$4745,$54A9  ; "(FORGET)"

c_forge FDB     do_col                   ; : (FORGET)
p_forge FDB     c_dup                    ; DUP
        FDB     c_fence                  ; FENCE
        FDB     c_at                     ; @
        FDB     c_ult                    ; U<
        FDB     c_abort                  ; (ABORT") len=11
        FCB     $0B
        FDB     $4265,$6C6F,$7720,$6665  ; "Below fe"
        FCB     $6E,$63,$65              ; "nce"
        FDB     c_dup                    ; DUP
        FDB     c_curre                  ; CURRENT
        FDB     c_at                     ; @
        FDB     c_ult                    ; U<
        FDB     c_abort                  ; (ABORT") len=32
        FCB     $20
        FDB     $4361,$6E6E,$6F74,$2066  ; "Cannot f"
        FDB     $6F72,$6765,$7420,$4355  ; "orget CU"
        FDB     $5252,$454E,$5420,$766F  ; "RRENT vo"
        FDB     $6361,$6275,$6C61,$7279  ; "cabulary"
        FDB     c_conte                  ; CONTEXT
        FDB     c_dup,c_lit_             ; DUP
        FDB     $000A                    ; 10
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z15F4              ; (DO) --Z15F4--v
Z15DC   FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_at                     ; @
        FDB     c_ult                    ; U<
        FDB     c_qmbra,Z15EE            ; ?BRANCH --Z15EE--v
        FDB     c_0                      ; 0
        FDB     c_i                      ; I
        FDB     c_exc                    ; !
Z15EE   FDB     c_2                      ; 2
        FDB     c_addlo,Z15DC            ; (+LOOP) --Z15DC--^
Z15F4   FDB     c_vocsu                  ; VOC-LINK
        FDB     c_at                     ; @
Z15F8   FDB     c_2dup                   ; 2DUP
        FDB     c_ult                    ; U<
        FDB     c_qmbra,Z1606            ; ?BRANCH --Z1606--v
        FDB     c_at                     ; @
        FDB     c_branc,Z15F8            ; BRANCH --Z15F8--^
Z1606   FDB     c_dup                    ; DUP
        FDB     c_vocsu                  ; VOC-LINK
        FDB     c_exc                    ; !
Z160C   FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z1646            ; ?BRANCH --Z1646--v
        FDB     c_2dup                   ; 2DUP
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0008                    ; 8
        FDB     c_sub                    ; -
        FDB     c_do_,Z163E              ; (DO) --Z163E--v
Z1620   FDB     c_i                      ; I
        FDB     c_at                     ; @
Z1624   FDB     c_2dup                   ; 2DUP
        FDB     c_1add                   ; 1+
        FDB     c_ult                    ; U<
        FDB     c_qmbra,Z1634            ; ?BRANCH --Z1634--v
        FDB     c_at                     ; @
        FDB     c_branc,Z1624            ; BRANCH --Z1624--^
Z1634   FDB     c_i                      ; I
        FDB     c_exc                    ; !
        FDB     c_2                      ; 2
        FDB     c_addlo,Z1620            ; (+LOOP) --Z1620--^
Z163E   FDB     c_drop                   ; DROP
        FDB     c_at                     ; @
        FDB     c_branc,Z160C            ; BRANCH --Z160C--^
Z1646   FDB     c_dp                     ; DP
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; FORGET
; ==================================================================
l_forg0 FDB     l_to_li                  ; LFA -> >LINK

n_forg0 FCB     $86                      ; NFA -> 6
        FCB     $46,$4F,$52,$47,$45,$D4  ; "FORGET"

c_forg0 FDB     do_col                   ; : FORGET
p_forg0 FDB     c_bl                     ; BL
        FDB     c_word                   ; WORD
        FDB     c_qmcap                  ; ?CAP
        FDB     c_at                     ; @
        FDB     c_qmbra,Z1669            ; ?BRANCH --Z1669--v
        FDB     c_dup                    ; DUP
        FDB     c_count                  ; COUNT
        FDB     c_caps                   ; CAPS
Z1669   FDB     c_curre                  ; CURRENT
        FDB     c_at                     ; @
        FDB     c_to_th                  ; >THREAD
        FDB     c_find_                  ; (FIND)
        FDB     c_0eq                    ; 0=
        FDB     c_abort                  ; (ABORT") len=25
        FCB     $19
        FDB     $4E6F,$7420,$696E,$2043  ; "Not in C"
        FDB     $5552,$5245,$4E54,$2076  ; "URRENT v"
        FDB     $6F63,$6162,$756C,$6172  ; "ocabular"
        FCB     $79                      ; "y"
        FDB     c_to_li                  ; >LINK
        FDB     c_forge                  ; (FORGET)
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEFINITIONS
; ==================================================================
l_defin FDB     l_forge                  ; LFA -> (FORGET)

n_defin FCB     $8B                      ; NFA -> 11
        FDB     $4445,$4649,$4E49,$5449  ; "DEFINITI"
        FCB     $4F,$4E,$D3              ; "ONS"

c_defin FDB     do_col                   ; : DEFINITIONS
p_defin FDB     c_conte                  ; CONTEXT
        FDB     c_at                     ; @
        FDB     c_curre                  ; CURRENT
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; VOCABULARY
; ==================================================================
l_vocab FDB     l_forg0                  ; LFA -> FORGET

n_vocab FCB     $8A                      ; NFA -> 10
        FDB     $564F,$4341,$4255,$4C41  ; "VOCABULA"
        FCB     $52,$D9                  ; "RY"

c_vocab FDB     do_col                   ; : VOCABULARY
p_vocab FDB     c_creat                  ; CREATE
        FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_here                   ; HERE
        FDB     c_vocsu                  ; VOC-LINK
        FDB     c_at                     ; @
        FDB     c_kom                    ; ,
        FDB     c_vocsu                  ; VOC-LINK
        FDB     c_exc                    ; !
        FDB     c_semic                  ; (;CODE)
do_voc  JMP     do_does
        FDB     c_conte                  ; CONTEXT
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; FORTH
; ==================================================================
l_fort0 FDB     l_vocab                  ; LFA -> VOCABULARY

n_fort0 FCB     $85                      ; NFA -> 5
        FCB     $46,$4F,$52,$54,$C8      ; "FORTH"

c_fort0 FDB     do_voc
p_fort0 FDB     l_task,l_subok,l_to_dd,l_step,$201A

; ==================================================================
; HIDE
; ==================================================================
l_hide  FDB     l_defin                  ; LFA -> DEFINITIONS

n_hide  FCB     $84                      ; NFA -> 4
        FCB     $48,$49,$44,$C5          ; "HIDE"

c_hide  FDB     do_col                   ; : HIDE
p_hide  FDB     c_last                   ; LAST
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_cat,c_lit_             ; C@
        FDB     $0020                    ; 32
        FDB     c_or                     ; OR
        FDB     c_swap                   ; SWAP
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; REVEAL
; ==================================================================
l_revea FDB     l_fort0                  ; LFA -> FORTH

n_revea FCB     $86                      ; NFA -> 6
        FCB     $52,$45,$56,$45,$41,$CC  ; "REVEAL"

c_revea FDB     do_col                   ; : REVEAL
p_revea FDB     c_last                   ; LAST
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_cat,c_lit_             ; C@
        FDB     $00DF                    ; 223
        FDB     c_and                    ; AND
        FDB     c_swap                   ; SWAP
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; IMMEDIATE
; ==================================================================
l_immed FDB     l_iddot                  ; LFA -> ID.

n_immed FCB     $89                      ; NFA -> 9
        FDB     $494D,$4D45,$4449,$4154  ; "IMMEDIAT"
        FCB     $C5                      ; "E"

c_immed FDB     do_col                   ; : IMMEDIATE
p_immed FDB     c_last                   ; LAST
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_cat,c_lit_             ; C@
        FDB     $0040                    ; 64
        FDB     c_or                     ; OR
        FDB     c_swap                   ; SWAP
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; RECURSE
; ==================================================================
l_recur FDB     l_revea                  ; LFA -> REVEAL

n_recur FCB     $C7                      ; NFA -> 7 IMMEDIATE
        FCB     $52,$45,$43,$55,$52,$53  ; "RECURS"
        FCB     $C5                      ; "E"

c_recur FDB     do_col                   ; : RECURSE
p_recur FDB     c_qmcom                  ; ?COMP
        FDB     c_last                   ; LAST
        FDB     c_at                     ; @
        FDB     c_nameg                  ; NAME>
        FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)

; ==================================================================
; CREATE
; ==================================================================
l_creat FDB     l_words                  ; LFA -> WORDS

n_creat FCB     $86                      ; NFA -> 6
        FCB     $43,$52,$45,$41,$54,$C5  ; "CREATE"

c_creat FDB     do_col                   ; : CREATE
p_creat FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_bl                     ; BL
        FDB     c_word                   ; WORD
        FDB     c_dup                    ; DUP
        FDB     c_cat                    ; C@
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0020                    ; 32
        FDB     c_lt                     ; <
        FDB     c_and                    ; AND
        FDB     c_0eq                    ; 0=
        FDB     c_abort                  ; (ABORT") len=19
        FCB     $13
        FDB     $496E,$7661,$6C69,$6420  ; "Invalid "
        FDB     $6E61,$6D65,$206C,$656E  ; "name len"
        FCB     $67,$74,$68              ; "gth"
        FDB     c_warni                  ; WARNING
        FDB     c_at                     ; @
        FDB     c_qmbra,Z17D9            ; ?BRANCH --Z17D9--v
        FDB     c_dup                    ; DUP
        FDB     c_find                   ; FIND
        FDB     c_swap                   ; SWAP
        FDB     c_drop                   ; DROP
        FDB     c_qmbra,Z17D5            ; ?BRANCH --Z17D5--v
        FDB     c_cr                     ; CR
        FDB     c_dup                    ; DUP
        FDB     c_count                  ; COUNT
        FDB     c_type                   ; TYPE
        FDB     c_dotqt                  ; (.") len=12
        FCB     $0C
        FDB     $202D,$6E6F,$7420,$756E  ; " -not un"
        FCB     $69,$71,$75,$65          ; "ique"
Z17D5   FDB     c_branc,Z17E7            ; BRANCH --Z17E7--v
Z17D9   FDB     c_qmcap                  ; ?CAP
        FDB     c_at                     ; @
        FDB     c_qmbra,Z17E7            ; ?BRANCH --Z17E7--v
        FDB     c_dup                    ; DUP
        FDB     c_count                  ; COUNT
        FDB     c_caps                   ; CAPS
Z17E7   FDB     c_last                   ; LAST
        FDB     c_exc                    ; !
        FDB     c_here                   ; HERE
        FDB     c_curre                  ; CURRENT
        FDB     c_at                     ; @
        FDB     c_to_th                  ; >THREAD
        FDB     c_dup                    ; DUP
        FDB     c_at                     ; @
        FDB     c_here                   ; HERE
        FDB     c_2sub                   ; 2-
        FDB     c_exc                    ; !
        FDB     c_over                   ; OVER
        FDB     c_2sub                   ; 2-
        FDB     c_swap                   ; SWAP
        FDB     c_exc                    ; !
        FDB     c_cat                    ; C@
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0080                    ; 128
        FDB     c_or                     ; OR
        FDB     c_ckom                   ; C,
        FDB     c_width                  ; WIDTH
        FDB     c_at                     ; @
        FDB     c_min                    ; MIN
        FDB     c_allot                  ; ALLOT
        FDB     c_here                   ; HERE
        FDB     c_1sub                   ; 1-
        FDB     c_dup                    ; DUP
        FDB     c_cat,c_lit_             ; C@
        FDB     $0080                    ; 128
        FDB     c_or                     ; OR
        FDB     c_swap                   ; SWAP
        FDB     c_cexc                   ; C!
        FDB     c_0                      ; 0
        FDB     c_kom                    ; ,
        FDB     c_semic                  ; (;CODE)
do_var  LEAX    $02,X
        PSHU    X
        FDB     $0E12

; ==================================================================
; DSKERR
; ==================================================================
l_dsker FDB     l_hide                   ; LFA -> HIDE

n_dsker FCB     $86                      ; NFA -> 6
        FCB     $44,$53,$4B,$45,$52,$D2  ; "DSKERR"

c_dsker FDB     do_var                   ; VARIABLE DSKERR
p_dsker FDB     $0000                    ; 0

; ==================================================================
; R/W
; ==================================================================
l_rdivw FDB     l_recur                  ; LFA -> RECURSE

n_rdivw FCB     $83                      ; NFA -> 3
        FCB     $52,$2F,$D7              ; "R/W"

c_rdivw FDB     do_col                   ; : R/W
p_rdivw FDB     c_rdwt                   ; RDWT
        FDB     c_dup                    ; DUP
        FDB     c_dsker                  ; DSKERR
        FDB     c_exc                    ; !
        FDB     c_abort                  ; (ABORT") len=14
        FCB     $0E
        FDB     $4469,$736B,$2052,$2F57  ; "Disk R/W"
        FCB     $20,$65,$72,$72,$6F,$72  ; " error"
        FDB     c_semi_                  ; (;)

; ==================================================================
; >BUFS
; ==================================================================
l_to_bu FDB     l_rdivw                  ; LFA -> R/W

n_to_bu FCB     $85                      ; NFA -> 5
        FCB     $3E,$42,$55,$46,$D3      ; ">BUFS"

c_to_bu FDB     do_const                 ; CONSTANT >BUFS
p_to_bu FDB     $B3F0                    ; 46064

; ==================================================================
; BUFSIZ
; ==================================================================
l_bufsi FDB     l_to_bu                  ; LFA -> >BUFS

n_bufsi FCB     $86                      ; NFA -> 6
        FCB     $42,$55,$46,$53,$49,$DA  ; "BUFSIZ"

c_bufsi FDB     do_const                 ; CONSTANT BUFSIZ
p_bufsi FDB     $0405                    ; 1029

; ==================================================================
; #BUFS
; ==================================================================
l_n_buf FDB     l_creat                  ; LFA -> CREATE

n_n_buf FCB     $85                      ; NFA -> 5
        FCB     $23,$42,$55,$46,$D3      ; "#BUFS"

c_n_buf FDB     do_const                 ; CONSTANT #BUFS
p_n_buf FDB     WARM                     ; 3

; ==================================================================
; PREV
; ==================================================================
l_prev  FDB     l_dsker                  ; LFA -> DSKERR

n_prev  FCB     $84                      ; NFA -> 4
        FCB     $50,$52,$45,$D6          ; "PREV"

c_prev  FDB     do_var                   ; VARIABLE PREV
p_prev  FDB     $BBFA                    ; 48122

; ==================================================================
; (SV)
; ==================================================================
l_sv_   FDB     l_prev                   ; LFA -> PREV

n_sv_   FCB     $84                      ; NFA -> 4
        FCB     $28,$53,$56,$A9          ; "(SV)"

c_sv_   FDB     do_col                   ; : (SV)
p_sv_   FDB     c_2add                   ; 2+
        FDB     c_dup                    ; DUP
        FDB     c_2add                   ; 2+
        FDB     c_cat                    ; C@
        FDB     c_qmbra,Z18C4            ; ?BRANCH --Z18C4--v
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0003                    ; 3
        FDB     c_add                    ; +
        FDB     c_over                   ; OVER
        FDB     c_at                     ; @
        FDB     c_0                      ; 0
        FDB     c_rdivw                  ; R/W
        FDB     c_0                      ; 0
        FDB     c_over                   ; OVER
        FDB     c_2add                   ; 2+
        FDB     c_cexc                   ; C!
Z18C4   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; (LNK)
; ==================================================================
l_lnk_  FDB     l_sv_                    ; LFA -> (SV)

n_lnk_  FCB     $85                      ; NFA -> 5
        FCB     $28,$4C,$4E,$4B,$A9      ; "(LNK)"

c_lnk_  FDB     do_col                   ; : (LNK)
p_lnk_  FDB     c_dup                    ; DUP
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_at                     ; @
        FDB     c_rot                    ; ROT
        FDB     c_exc                    ; !
        FDB     c_prev                   ; PREV
        FDB     c_at                     ; @
        FDB     c_over                   ; OVER
        FDB     c_exc                    ; !
        FDB     c_dup                    ; DUP
        FDB     c_prev                   ; PREV
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; (BK)
; ==================================================================
l_bk_   FDB     l_lnk_                   ; LFA -> (LNK)

n_bk_   FCB     $84                      ; NFA -> 4
        FCB     $28,$42,$4B,$A9          ; "(BK)"

c_bk_   FDB     do_col                   ; : (BK)
p_bk_   FDB     c_pause                  ; PAUSE
        FDB     c_offse                  ; OFFSET
        FDB     c_at                     ; @
        FDB     c_add                    ; +
        FDB     c_prev                   ; PREV
        FDB     c_at                     ; @
        FDB     c_2add                   ; 2+
        FDB     c_at                     ; @
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z1921            ; ?BRANCH --Z1921--v
        FDB     c_drop                   ; DROP
        FDB     c_prev                   ; PREV
        FDB     c_at,c_lit_              ; @
        FDB     $0005                    ; 5
        FDB     c_add                    ; +
        FDB     c_from_                  ; R>
        FDB     c_drop                   ; DROP
        FDB     c_exit                   ; EXIT
Z1921   FDB     c_prev                   ; PREV
Z1923   FDB     c_dup                    ; DUP
        FDB     c_at                     ; @
        FDB     c_at                     ; @
        FDB     c_qmbra,Z1953            ; ?BRANCH --Z1953--v
        FDB     c_at                     ; @
        FDB     c_2dup                   ; 2DUP
        FDB     c_at                     ; @
        FDB     c_2add                   ; 2+
        FDB     c_at                     ; @
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z194F            ; ?BRANCH --Z194F--v
        FDB     c_swap                   ; SWAP
        FDB     c_drop                   ; DROP
        FDB     c_lnk_,c_lit_            ; (LNK)
        FDB     $0005                    ; 5
        FDB     c_add                    ; +
        FDB     c_from_                  ; R>
        FDB     c_drop                   ; DROP
        FDB     c_exit                   ; EXIT
Z194F   FDB     c_branc,Z1923            ; BRANCH --Z1923--^
Z1953   FDB     c_lnk_                   ; (LNK)
        FDB     c_semi_                  ; (;)

; ==================================================================
; BLOCK
; ==================================================================
l_block FDB     l_bufsi                  ; LFA -> BUFSIZ

n_block FCB     $85                      ; NFA -> 5
        FCB     $42,$4C,$4F,$43,$CB      ; "BLOCK"

c_block FDB     do_col                   ; : BLOCK
p_block FDB     c_bk_                    ; (BK)
        FDB     c_dup                    ; DUP
        FDB     c_sv_                    ; (SV)
        FDB     c_swap                   ; SWAP
        FDB     c_over,c_lit_            ; OVER
        FDB     $0005                    ; 5
        FDB     c_add                    ; +
        FDB     c_over                   ; OVER
        FDB     c_1                      ; 1
        FDB     c_rdivw                  ; R/W
        FDB     c_over                   ; OVER
        FDB     c_2add                   ; 2+
        FDB     c_exc,c_lit_             ; !
        FDB     $0005                    ; 5
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; BUFFER
; ==================================================================
l_buffe FDB     l_block                  ; LFA -> BLOCK

n_buffe FCB     $86                      ; NFA -> 6
        FCB     $42,$55,$46,$46,$45,$D2  ; "BUFFER"

c_buffe FDB     do_col                   ; : BUFFER
p_buffe FDB     c_bk_                    ; (BK)
        FDB     c_dup                    ; DUP
        FDB     c_sv_                    ; (SV)
        FDB     c_swap                   ; SWAP
        FDB     c_over                   ; OVER
        FDB     c_2add                   ; 2+
        FDB     c_exc,c_lit_             ; !
        FDB     $0005                    ; 5
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; UPDATE
; ==================================================================
l_updat FDB     l_immed                  ; LFA -> IMMEDIATE

n_updat FCB     $86                      ; NFA -> 6
        FCB     $55,$50,$44,$41,$54,$C5  ; "UPDATE"

c_updat FDB     do_col                   ; : UPDATE
p_updat FDB     c_1                      ; 1
        FDB     c_prev                   ; PREV
        FDB     c_at,c_lit_              ; @
        FDB     $0004                    ; 4
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; COPY
; ==================================================================
l_copy  FDB     l_n_buf                  ; LFA -> #BUFS

n_copy  FCB     $84                      ; NFA -> 4
        FCB     $43,$4F,$50,$D9          ; "COPY"

c_copy  FDB     do_col                   ; : COPY
p_copy  FDB     c_offse                  ; OFFSET
        FDB     c_at                     ; @
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_block,c_lit_           ; BLOCK
        FDB     $0005                    ; 5
        FDB     c_sub                    ; -
        FDB     c_sv_                    ; (SV)
        FDB     c_to_bu                  ; >BUFS
        FDB     c_n_buf                  ; #BUFS
        FDB     c_0                      ; 0
        FDB     c_do_,Z1A10              ; (DO) --Z1A10--v
Z19E6   FDB     c_2dup                   ; 2DUP
        FDB     c_2add                   ; 2+
        FDB     c_at                     ; @
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z1A08            ; ?BRANCH --Z1A08--v
        FDB     c_0                      ; 0
        FDB     c_over,c_lit_            ; OVER
        FDB     $0004                    ; 4
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_sub1                   ; -1
        FDB     c_over                   ; OVER
        FDB     c_2add                   ; 2+
        FDB     c_exc                    ; !
        FDB     c_leave                  ; (LEAVE)
Z1A08   FDB     c_bufsi                  ; BUFSIZ
        FDB     c_add                    ; +
        FDB     c_loop_,Z19E6            ; (LOOP) --Z19E6--^
Z1A10   FDB     c_drop                   ; DROP
        FDB     c_prev                   ; PREV
        FDB     c_at                     ; @
        FDB     c_2add                   ; 2+
        FDB     c_exc                    ; !
        FDB     c_updat                  ; UPDATE
        FDB     c_semi_                  ; (;)

; ==================================================================
; EMPTY-BUFFERS
; ==================================================================
l_empty FDB     l_updat                  ; LFA -> UPDATE

n_empty FCB     $8D                      ; NFA -> 13
        FDB     $454D,$5054,$592D,$4255  ; "EMPTY-BU"
        FCB     $46,$46,$45,$52,$D3      ; "FFERS"

c_empty FDB     do_col                   ; : EMPTY-BUFFERS
p_empty FDB     c_to_bu                  ; >BUFS
        FDB     c_0                      ; 0
        FDB     c_n_buf                  ; #BUFS
        FDB     c_0                      ; 0
        FDB     c_do_,Z1A5A              ; (DO) --Z1A5A--v
Z1A3C   FDB     c_over                   ; OVER
        FDB     c_bufsi                  ; BUFSIZ
        FDB     c_erase                  ; ERASE
        FDB     c_over                   ; OVER
        FDB     c_exc                    ; !
        FDB     c_sub1                   ; -1
        FDB     c_over                   ; OVER
        FDB     c_2add                   ; 2+
        FDB     c_exc                    ; !
        FDB     c_dup                    ; DUP
        FDB     c_bufsi                  ; BUFSIZ
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_loop_,Z1A3C            ; (LOOP) --Z1A3C--^
Z1A5A   FDB     c_prev                   ; PREV
        FDB     c_exc                    ; !
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; SAVE-BUFFERS
; ==================================================================
l_saves FDB     l_copy                   ; LFA -> COPY

n_saves FCB     $8C                      ; NFA -> 12
        FDB     $5341,$5645,$2D42,$5546  ; "SAVE-BUF"
        FCB     $46,$45,$52,$D3          ; "FERS"

c_saves FDB     do_col                   ; : SAVE-BUFFERS
p_saves FDB     c_prev                   ; PREV
Z1A75   FDB     c_at                     ; @
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z1A85            ; ?BRANCH --Z1A85--v
        FDB     c_dup                    ; DUP
        FDB     c_sv_                    ; (SV)
        FDB     c_branc,Z1A75            ; BRANCH --Z1A75--^
Z1A85   FDB     c_semi_                  ; (;)

; ==================================================================
; FLUSH
; ==================================================================
l_flush FDB     l_buffe                  ; LFA -> BUFFER

n_flush FCB     $85                      ; NFA -> 5
        FCB     $46,$4C,$55,$53,$C8      ; "FLUSH"

c_flush FDB     do_col                   ; : FLUSH
p_flush FDB     c_saves                  ; SAVE-BUFFERS
        FDB     c_empty                  ; EMPTY-BUFFERS
        FDB     c_semi_                  ; (;)

; ==================================================================
; VERSION
; ==================================================================
l_versi FDB     l_flush                  ; LFA -> FLUSH

n_versi FCB     $87                      ; NFA -> 7
        FCB     $56,$45,$52,$53,$49,$4F  ; "VERSIO"
        FCB     $CE                      ; "N"

c_versi FDB     do_var                   ; VARIABLE VERSION
p_versi FDB     $006E                    ; 110

; ==================================================================
; UP
; ==================================================================
l_up    FDB     l_empty                  ; LFA -> EMPTY-BUFFERS

n_up    FCB     $82                      ; NFA -> 2
        FCB     $55,$D0                  ; "UP"

c_up    FDB     do_var                   ; VARIABLE UP
p_up    FDB     $1B45                    ; 6981

; ==================================================================
; USER
; ==================================================================
l_user  FDB     l_up                     ; LFA -> UP

n_user  FCB     $84                      ; NFA -> 4
        FCB     $55,$53,$45,$D2          ; "USER"

c_user  FDB     do_col                   ; : USER
p_user  FDB     c_creat                  ; CREATE
        FDB     c_kom                    ; ,
        FDB     c_semic                  ; (;CODE)
getUVAR LDD     $02,X
        ADDD    p_up
        PSHU    D
        JMP     p_next

; ==================================================================
; S0
; ==================================================================
l_s0    FDB     l_saves                  ; LFA -> SAVE-BUFFERS

n_s0    FCB     $82                      ; NFA -> 2
        FCB     $53,$B0                  ; "S0"

c_s0    FDB     getUVAR
p_s0    FDB     COLD

; ==================================================================
; SP!
; ==================================================================
l_spexc FDB     l_s0                     ; LFA -> S0

n_spexc FCB     $83                      ; NFA -> 3
        FCB     $53,$50,$A1              ; "SP!"

c_spexc FDB     p_spexc                  ; ASSEMBLER
p_spexc LDU     [p_up]
        CLR     ,U
        CLR     $01,U
        JMP     p_next

; ==================================================================
; R0
; ==================================================================
l_r0    FDB     l_versi                  ; LFA -> VERSION

n_r0    FCB     $82                      ; NFA -> 2
        FCB     $52,$B0                  ; "R0"

c_r0    FDB     getUVAR
p_r0    FDB     $0002

; ==================================================================
; RP!
; ==================================================================
l_rpexc FDB     l_r0                     ; LFA -> R0

n_rpexc FCB     $83                      ; NFA -> 3
        FCB     $52,$50,$A1              ; "RP!"

c_rpexc FDB     p_rpexc                  ; ASSEMBLER
p_rpexc LDX     p_up
        LDS     $02,X
        JMP     p_next

; ==================================================================
; DP
; ==================================================================
l_dp    FDB     l_bk_                    ; LFA -> (BK)

n_dp    FCB     $82                      ; NFA -> 2
        FCB     $44,$D0                  ; "DP"

c_dp    FDB     getUVAR
p_dp    FDB     $0004

; ==================================================================
; OFFSET
; ==================================================================
l_offse FDB     l_spexc                  ; LFA -> SP!

n_offse FCB     $86                      ; NFA -> 6
        FCB     $4F,$46,$46,$53,$45,$D4  ; "OFFSET"

c_offse FDB     getUVAR
p_offse FDB     MEMLIM

; ==================================================================
; OUT
; ==================================================================
l_out   FDB     l_offse                  ; LFA -> OFFSET

n_out   FCB     $83                      ; NFA -> 3
        FCB     $4F,$55,$D4              ; "OUT"

c_out   FDB     getUVAR
p_out   FDB     l_next

; ==================================================================
; PRNT
; ==================================================================
l_prnt  FDB     l_dp                     ; LFA -> DP

n_prnt  FCB     $84                      ; NFA -> 4
        FCB     $50,$52,$4E,$D4          ; "PRNT"

c_prnt  FDB     getUVAR
p_prnt  FDB     n_next

; ==================================================================
; INPT
; ==================================================================
l_inpt  FDB     l_user                   ; LFA -> USER

n_inpt  FCB     $84                      ; NFA -> 4
        FCB     $49,$4E,$50,$D4          ; "INPT"

c_inpt  FDB     getUVAR
p_inpt  FDB     $000C

; ==================================================================
; HLD
; ==================================================================
l_hld   FDB     l_prnt                   ; LFA -> PRNT

n_hld   FCB     $83                      ; NFA -> 3
        FCB     $48,$4C,$C4              ; "HLD"

c_hld   FDB     getUVAR
p_hld   FDB     $000E

; ==================================================================
; BASE
; ==================================================================
l_base  FDB     l_rpexc                  ; LFA -> RP!

n_base  FCB     $84                      ; NFA -> 4
        FCB     $42,$41,$53,$C5          ; "BASE"

c_base  FDB     getUVAR
p_base  FDB     c_next,$B2EE,$B3F0       ; >NEXT
        FDB     $8A96,$0138,$0BD3,COLD,COLD,$8AE5,n_next,COLD,COLD
        FDB     COLD,COLD,COLD,COLD,COLD

; ==================================================================
; 'STARTUP
; ==================================================================
l_tckst FDB     l_out                    ; LFA -> OUT

n_tckst FCB     $88                      ; NFA -> 8
        FDB     $2753,$5441,$5254,$55D0  ; "'STARTUP"

c_tckst FDB     do_var                   ; VARIABLE 'STARTUP
p_tckst FDB     $8255                    ; 33365

; ==================================================================
; RESET
; ==================================================================
l_reset FDB     l_base                   ; LFA -> BASE

n_reset FCB     $85                      ; NFA -> 5
        FCB     $52,$45,$53,$45,$D4      ; "RESET"

c_reset FDB     p_reset                  ; ASSEMBLER
p_reset LDD     [MEMLIM]
        LDX     p_n_buf
Z1B85   SUBD    p_bufsi
        LEAX    -$01,X
        BNE     Z1B85
        STD     p_to_bu
        LDX     p_up
        TFR     D,S
        STS     $02,X
        LEAU    $FEFE,S
        STU     ,X
        CLRA
        TFR     A,DP
        LDY     #M1BA6
        JMP     p_next
M1BA6   FDB     c_rpexc,c_spexc,c_empty
M1BAC   FDB     c_rpexc,c_spexc,c_tckst,c_at,c_execu
Z1BB6   CLRA
        TFR     A,DP
        LDY     #M1BAC
        JMP     p_next

; ==================================================================
; TERMINAL
; ==================================================================
l_termi FDB     l_hld                    ; LFA -> HLD

n_termi FCB     $88                      ; NFA -> 8
        FDB     $5445,$524D,$494E,$41CC  ; "TERMINAL"

c_termi FDB     do_col                   ; : TERMINAL
p_termi FDB     c_visu                   ; VISU
        FDB     c_exit                   ; EXIT
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_inpt                   ; INPT
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; PRINTER
; ==================================================================
l_print FDB     l_termi                  ; LFA -> TERMINAL

n_print FCB     $87                      ; NFA -> 7
        FCB     $50,$52,$49,$4E,$54,$45  ; "PRINTE"
        FCB     $D2                      ; "R"

c_print FDB     do_col                   ; : PRINTER
p_print FDB     c_sub1                   ; -1
        FDB     c_prnt                   ; PRNT
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; 'IO
; ==================================================================
l_tckio FDB     l_tckst                  ; LFA -> 'STARTUP

n_tckio FCB     $83                      ; NFA -> 3
        FCB     $27,$49,$CF              ; "'IO"

c_tckio FDB     do_var                   ; VARIABLE 'IO
p_tckio FDB     $1CB3                    ; 7347
        FCB     $1C,$CA,$0F,$35,$1D,$1D,$0F,$35,$1C,$E0,$1C,$F3,$1D
        FCB     $09

; ==================================================================
; IO
; ==================================================================
l_io    FDB     l_inpt                   ; LFA -> INPT

n_io    FCB     $82                      ; NFA -> 2
        FCB     $49,$CF                  ; "IO"

c_io    FDB     do_col                   ; : IO
p_io    FDB     c_tckio                  ; 'IO
        FDB     c_add                    ; +
        FDB     c_2at                    ; 2@
Z1C13   FDB     c_pause                  ; PAUSE
        FDB     c_dup                    ; DUP
        FDB     c_execu                  ; EXECUTE
        FDB     c_qmbra,Z1C13            ; ?BRANCH --Z1C13--^
        FDB     c_drop                   ; DROP
        FDB     c_execu                  ; EXECUTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; IOSTAT
; ==================================================================
l_iosta FDB     l_io                     ; LFA -> IO

n_iosta FCB     $86                      ; NFA -> 6
        FCB     $49,$4F,$53,$54,$41,$D4  ; "IOSTAT"

c_iosta FDB     do_col                   ; : IOSTAT
p_iosta FDB     c_tckio                  ; 'IO
        FDB     c_add                    ; +
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; EMIT
; ==================================================================
l_emit  FDB     l_iosta                  ; LFA -> IOSTAT

n_emit  FCB     $84                      ; NFA -> 4
        FCB     $45,$4D,$49,$D4          ; "EMIT"

c_emit  FDB     do_col                   ; : EMIT
p_emit  FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z1C51,c_lit_     ; ?BRANCH --Z1C51--v
        FDB     $000C                    ; 12
        FDB     c_branc,Z1C55            ; BRANCH --Z1C55--v
Z1C51   FDB     c_lit_
        FDB     $0008                    ; 8
Z1C55   FDB     c_io                     ; IO
        FDB     c_1                      ; 1
        FDB     c_out                    ; OUT
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; KEY
; ==================================================================
l_key   FDB     l_tckio                  ; LFA -> 'IO

n_key   FCB     $83                      ; NFA -> 3
        FCB     $4B,$45,$D9              ; "KEY"

c_key   FDB     do_col                   ; : KEY
p_key   FDB     c_0                      ; 0
        FDB     c_inpt                   ; INPT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z1C77,c_lit_     ; ?BRANCH --Z1C77--v
        FDB     $0004                    ; 4
        FDB     c_add                    ; +
Z1C77   FDB     c_io                     ; IO
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?KEY
; ==================================================================
l_qmkey FDB     l_key                    ; LFA -> KEY

n_qmkey FCB     $84                      ; NFA -> 4
        FCB     $3F,$4B,$45,$D9          ; "?KEY"

c_qmkey FDB     do_col                   ; : ?KEY
p_qmkey FDB     c_0                      ; 0
        FDB     c_inpt                   ; INPT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z1C94,c_lit_     ; ?BRANCH --Z1C94--v
        FDB     $0004                    ; 4
        FDB     c_add                    ; +
Z1C94   FDB     c_dup                    ; DUP
        FDB     c_iosta                  ; IOSTAT
        FDB     c_qmbra,Z1CA2            ; ?BRANCH --Z1CA2--v
        FDB     c_io                     ; IO
        FDB     c_branc,Z1CA8            ; BRANCH --Z1CA8--v
Z1CA2   FDB     c_drop                   ; DROP
        FDB     c_pause                  ; PAUSE
        FDB     c_0                      ; 0
Z1CA8   FDB     c_semi_                  ; (;)

; ==================================================================
; TCHECK
; ==================================================================
l_tchec FDB     l_print                  ; LFA -> PRINTER

n_tchec FCB     $86                      ; NFA -> 6
        FCB     $54,$43,$48,$45,$43,$CB  ; "TCHECK"

c_tchec FDB     p_tchec                  ; ASSEMBLER
p_tchec CLRB
        JSR     [CSTAT]
        BEQ     Z1CBD
        DECB
Z1CBD   SEX
        PSHU    D
        JMP     p_next

; ==================================================================
; TINCH
; ==================================================================
l_tinch FDB     l_tchec                  ; LFA -> TCHECK

n_tinch FCB     $85                      ; NFA -> 5
        FCB     $54,$49,$4E,$43,$C8      ; "TINCH"

c_tinch FDB     p_tinch                  ; ASSEMBLER
p_tinch JSR     [CINCHNE]
        PSHU    A
        CLRA
        PSHU    A
        JMP     p_next

; ==================================================================
; TOUTCH
; ==================================================================
l_toutc FDB     l_tinch                  ; LFA -> TINCH

n_toutc FCB     $86                      ; NFA -> 6
        FCB     $54,$4F,$55,$54,$43,$C8  ; "TOUTCH"

c_toutc FDB     p_toutc                  ; ASSEMBLER
p_toutc LDA     $01,U
        LEAU    $02,U
        JSR     [COUTCH]
        JMP     p_next

; ==================================================================
; PCHK
; ==================================================================
l_pchk  FDB     l_toutc                  ; LFA -> TOUTCH

n_pchk  FCB     $84                      ; NFA -> 4
        FCB     $50,$43,$48,$CB          ; "PCHK"

c_pchk  FDB     p_pchk                   ; ASSEMBLER
p_pchk  LDB     #$FF
        JSR     PRTCHK
        BMI     Z1CFD
        CLRB
Z1CFD   SEX
        PSHU    D
        JMP     p_next

; ==================================================================
; POUT
; ==================================================================
l_pout  FDB     l_pchk                   ; LFA -> PCHK

n_pout  FCB     $84                      ; NFA -> 4
        FCB     $50,$4F,$55,$D4          ; "POUT"

c_pout  FDB     p_pout                   ; ASSEMBLER
p_pout  LDA     $01,U
        LEAU    $02,U
        JSR     PRTOUT
        JMP     p_next

; ==================================================================
; GETCHR
; ==================================================================
l_getch FDB     l_qmkey                  ; LFA -> ?KEY

n_getch FCB     $86                      ; NFA -> 6
        FCB     $47,$45,$54,$43,$48,$D2  ; "GETCHR"

c_getch FDB     p_getch                  ; ASSEMBLER
p_getch JSR     GETCHR
        PSHU    A
        CLRA
        PSHU    A
        JMP     p_next

; ==================================================================
; PUTCHR
; ==================================================================
l_putch FDB     l_pout                   ; LFA -> POUT

n_putch FCB     $86                      ; NFA -> 6
        FCB     $50,$55,$54,$43,$48,$D2  ; "PUTCHR"

c_putch FDB     p_putch                  ; ASSEMBLER
p_putch LDA     $01,U
        LEAU    $02,U
        JSR     PUTCHR
        JMP     p_next

; ==================================================================
; FLEX
; ==================================================================
l_flex  FDB     l_reset                  ; LFA -> RESET

n_flex  FCB     $84                      ; NFA -> 4
        FCB     $46,$4C,$45,$D8          ; "FLEX"

c_flex  FDB     p_flex                   ; ASSEMBLER
p_flex  JMP     WARMS

; ==================================================================
; TRK/DRV
; ==================================================================
l_trkdi FDB     l_putch                  ; LFA -> PUTCHR

n_trkdi FCB     $87                      ; NFA -> 7
        FCB     $54,$52,$4B,$2F,$44,$52  ; "TRK/DR"
        FCB     $D6                      ; "V"

c_trkdi FDB     do_const                 ; CONSTANT TRK/DRV
p_trkdi FDB     $0028                    ; 40

; ==================================================================
; BASETRK
; ==================================================================
l_baset FDB     l_flex                   ; LFA -> FLEX

n_baset FCB     $87                      ; NFA -> 7
        FCB     $42,$41,$53,$45,$54,$52  ; "BASETR"
        FCB     $CB                      ; "K"

c_baset FDB     do_const                 ; CONSTANT BASETRK
p_baset FDB     $0001                    ; 1

; ==================================================================
; SEC/TRK
; ==================================================================
l_secdi FDB     l_getch                  ; LFA -> GETCHR

n_secdi FCB     $87                      ; NFA -> 7
        FCB     $53,$45,$43,$2F,$54,$52  ; "SEC/TR"
        FCB     $CB                      ; "K"

c_secdi FDB     do_const                 ; CONSTANT SEC/TRK
p_secdi FDB     $0020                    ; 32

; ==================================================================
; BASEDRV
; ==================================================================
l_based FDB     l_baset                  ; LFA -> BASETRK

n_based FCB     $87                      ; NFA -> 7
        FCB     $42,$41,$53,$45,$44,$52  ; "BASEDR"
        FCB     $D6                      ; "V"

c_based FDB     do_const                 ; CONSTANT BASEDRV
p_based FDB     $0000                    ; 0

; ==================================================================
; SEC/BLK
; ==================================================================
l_secd0 FDB     l_secdi                  ; LFA -> SEC/TRK

n_secd0 FCB     $87                      ; NFA -> 7
        FCB     $53,$45,$43,$2F,$42,$4C  ; "SEC/BL"
        FCB     $CB                      ; "K"

c_secd0 FDB     do_const                 ; CONSTANT SEC/BLK
p_secd0 FDB     $0004                    ; 4

; ==================================================================
; B/SEC
; ==================================================================
l_bdivs FDB     l_based                  ; LFA -> BASEDRV

n_bdivs FCB     $85                      ; NFA -> 5
        FCB     $42,$2F,$53,$45,$C3      ; "B/SEC"

c_bdivs FDB     do_const                 ; CONSTANT B/SEC
p_bdivs FDB     l_rat                    ; 256

; ==================================================================
; BLK/DRV
; ==================================================================
l_blkdi FDB     l_bdivs                  ; LFA -> B/SEC

n_blkdi FCB     $87                      ; NFA -> 7
        FCB     $42,$4C,$4B,$2F,$44,$52  ; "BLK/DR"
        FCB     $D6                      ; "V"

c_blkdi FDB     do_col                   ; : BLK/DRV
p_blkdi FDB     c_secdi                  ; SEC/TRK
        FDB     c_trkdi                  ; TRK/DRV
        FDB     c_baset                  ; BASETRK
        FDB     c_sub                    ; -
        FDB     c_ast                    ; *
        FDB     c_secd0                  ; SEC/BLK
        FDB     c_div                    ; /
        FDB     c_semi_                  ; (;)

; ==================================================================
; DR0
; ==================================================================
l_dr0   FDB     l_trkdi                  ; LFA -> TRK/DRV

n_dr0   FCB     $83                      ; NFA -> 3
        FCB     $44,$52,$B0              ; "DR0"

c_dr0   FDB     do_col                   ; : DR0
p_dr0   FDB     c_0                      ; 0
        FDB     c_offse                  ; OFFSET
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; DR1
; ==================================================================
l_dr1   FDB     l_dr0                    ; LFA -> DR0

n_dr1   FCB     $83                      ; NFA -> 3
        FCB     $44,$52,$B1              ; "DR1"

c_dr1   FDB     do_col                   ; : DR1
p_dr1   FDB     c_blkdi                  ; BLK/DRV
        FDB     c_offse                  ; OFFSET
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; (CB)
; ==================================================================
l_cb_   FDB     l_dr1                    ; LFA -> DR1

n_cb_   FCB     $84                      ; NFA -> 4
        FCB     $28,$43,$42,$A9          ; "(CB)"

c_cb_   FDB     do_var                   ; VARIABLE (CB)
p_cb_   FDB     $0900                    ; 2304
        FCB     $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        FCB     $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        FCB     $00,$00,$0E,$0D,$00,$00,$00,$00,$00,$00,$00,$00,$00
        FCB     $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        FCB     $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$2E,$22,$20,$44,$45,$43,$4F,$4D
        FCB     $50,$49,$4C,$41,$54,$45,$55,$52,$20,$22,$20,$74,$72
        FCB     $61,$69,$74,$20,$32,$20,$61,$6C,$20,$24,$24,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$2E,$22,$20,$4D,$54,$2D,$38
        FCB     $30,$20,$65,$6E,$20,$6F,$72,$64,$72,$65,$20,$64,$65
        FCB     $20,$6D,$61,$72,$63,$68,$65,$3F,$22,$20,$79,$2F,$6E
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$69,$66,$20
        FCB     $24,$24,$20,$70,$72,$65,$73,$65,$6E,$74,$65,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$74,$68
        FCB     $65,$6E,$20,$3B,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$31,$32,$39,$20,$4C,$4F,$41
        FCB     $44,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20

; ==================================================================
; (RW)
; ==================================================================
l_rw_   FDB     l_cb_                    ; LFA -> (CB)

n_rw_   FCB     $84                      ; NFA -> 4
        FCB     $28,$52,$57,$A9          ; "(RW)"

c_rw_   FDB     p_rw_                    ; ASSEMBLER
p_rw_   LDX     #p_cb_
        JSR     FMS
        JMP     p_next

; ==================================================================
; (NS)
; ==================================================================
l_ns_   FDB     l_rw_                    ; LFA -> (RW)

n_ns_   FCB     $84                      ; NFA -> 4
        FCB     $28,$4E,$53,$A9          ; "(NS)"

c_ns_   FDB     do_col                   ; : (NS)
p_ns_   FDB     c_lit_
        FDB     $1DFF                    ; 7679
        FDB     c_cat                    ; C@
        FDB     c_1add                   ; 1+
        FDB     c_dup                    ; DUP
        FDB     c_secdi                  ; SEC/TRK
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z1F5E            ; ?BRANCH --Z1F5E--v
        FDB     c_drop                   ; DROP
        FDB     c_1,c_lit_               ; 1
        FDB     $1DFE                    ; 7678
        FDB     c_cat                    ; C@
        FDB     c_1add,c_lit_            ; 1+
        FDB     $1DFE                    ; 7678
        FDB     c_cexc                   ; C!
Z1F5E   FDB     c_lit_
        FDB     $1DFF                    ; 7679
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; RDWT
; ==================================================================
l_rdwt  FDB     l_blkdi                  ; LFA -> BLK/DRV

n_rdwt  FCB     $84                      ; NFA -> 4
        FCB     $52,$44,$57,$D4          ; "RDWT"

c_rdwt  FDB     do_col                   ; : RDWT
p_rdwt  FDB     c_to_r                   ; >R
        FDB     c_blkdi                  ; BLK/DRV
        FDB     c_divmo                  ; /MOD
        FDB     c_based                  ; BASEDRV
        FDB     c_add,c_lit_             ; +
        FDB     $1DE3                    ; 7651
        FDB     c_cexc                   ; C!
        FDB     c_secd0                  ; SEC/BLK
        FDB     c_ast                    ; *
        FDB     c_secdi                  ; SEC/TRK
        FDB     c_divmo                  ; /MOD
        FDB     c_baset                  ; BASETRK
        FDB     c_add,c_lit_             ; +
        FDB     $1DFE                    ; 7678
        FDB     c_cexc                   ; C!
        FDB     c_1add,c_lit_            ; 1+
        FDB     $1DFF                    ; 7679
        FDB     c_cexc                   ; C!
        FDB     c_from_                  ; R>
        FDB     c_qmbra,Z1FD1,c_lit_     ; ?BRANCH --Z1FD1--v
        FDB     $0009                    ; 9
        FDB     c_cb_                    ; (CB)
        FDB     c_cexc                   ; C!
        FDB     c_secd0                  ; SEC/BLK
        FDB     c_0                      ; 0
        FDB     c_do_,Z1FCD              ; (DO) --Z1FCD--v
Z1FAF   FDB     c_rw_,c_lit_             ; (RW)
        FDB     $1DE1                    ; 7649
        FDB     c_cat                    ; C@
        FDB     c_qmlea,c_lit_           ; (?LEAVE)
        FDB     $1E20                    ; 7712
        FDB     c_over                   ; OVER
        FDB     c_bdivs                  ; B/SEC
        FDB     c_cmove                  ; CMOVE
        FDB     c_bdivs                  ; B/SEC
        FDB     c_add                    ; +
        FDB     c_ns_                    ; (NS)
        FDB     c_loop_,Z1FAF            ; (LOOP) --Z1FAF--^
Z1FCD   FDB     c_branc,Z1FFF            ; BRANCH --Z1FFF--v
Z1FD1   FDB     c_lit_
        FDB     $000A                    ; 10
        FDB     c_cb_                    ; (CB)
        FDB     c_cexc                   ; C!
        FDB     c_secd0                  ; SEC/BLK
        FDB     c_0                      ; 0
        FDB     c_do_,Z1FFF              ; (DO) --Z1FFF--v
Z1FE1   FDB     c_dup,c_lit_             ; DUP
        FDB     $1E20                    ; 7712
        FDB     c_bdivs                  ; B/SEC
        FDB     c_cmove                  ; CMOVE
        FDB     c_rw_,c_lit_             ; (RW)
        FDB     $1DE1                    ; 7649
        FDB     c_cat                    ; C@
        FDB     c_qmlea                  ; (?LEAVE)
        FDB     c_bdivs                  ; B/SEC
        FDB     c_add                    ; +
        FDB     c_ns_                    ; (NS)
        FDB     c_loop_,Z1FE1            ; (LOOP) --Z1FE1--^
Z1FFF   FDB     c_drop,c_lit_            ; DROP
        FDB     $1DE1                    ; 7649
        FDB     c_cat                    ; C@
        FDB     c_semi_                  ; (;)

; ==================================================================
; ONLY
; ==================================================================
l_only  FDB     $0000

n_only  FCB     $84                      ; NFA -> 4
        FCB     $4F,$4E,$4C,$D9          ; "ONLY"

c_only  FDB     Z201C
p_only  FDB     l_defi0,l_also,l_voca0,l_seal,COLD
Z201C   JMP     do_does
        FDB     c_conte,c_lit_,l_next,c_erase,c_dup,c_conte,c_exc
        FDB     c_conte,c_lit_,l_next,c_add,c_exc
        FDB     c_semi_

; ==================================================================
; ALSO
; ==================================================================
l_also  FDB     $0000

n_also  FCB     $84                      ; NFA -> 4
        FCB     $41,$4C,$53,$CF          ; "ALSO"

c_also  FDB     do_col                   ; : ALSO
p_also  FDB     c_conte                  ; CONTEXT
        FDB     c_dup                    ; DUP
        FDB     c_2add,c_lit_            ; 2+
        FDB     $0006                    ; 6
        FDB     c_cmov0                  ; CMOVE>
        FDB     c_semi_                  ; (;)

; ==================================================================
; FORTH0
; ==================================================================
l_fort1 FDB     $0000

n_fort1 FCB     $85                      ; NFA -> 5
        FCB     $46,$4F,$52,$54,$C8      ; "FORTH"

c_fort1 FDB     do_col                   ; : FORTH0
p_fort1 FDB     c_fort0                  ; FORTH
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEFINITIONS0
; ==================================================================
l_defi0 FDB     $0000

n_defi0 FCB     $8B                      ; NFA -> 11
        FDB     $4445,$4649,$4E49,$5449  ; "DEFINITI"
        FCB     $4F,$4E,$D3              ; "ONS"

c_defi0 FDB     do_col                   ; : DEFINITIONS0
p_defi0 FDB     c_defin                  ; DEFINITIONS
        FDB     c_semi_                  ; (;)

; ==================================================================
; WORDS0
; ==================================================================
l_word0 FDB     l_only                   ; LFA -> ONLY

n_word0 FCB     $85                      ; NFA -> 5
        FCB     $57,$4F,$52,$44,$D3      ; "WORDS"

c_word0 FDB     do_col                   ; : WORDS0
p_word0 FDB     c_words                  ; WORDS
        FDB     c_semi_                  ; (;)

; ==================================================================
; ORDER
; ==================================================================
l_order FDB     l_word0                  ; LFA -> WORDS0

n_order FCB     $85                      ; NFA -> 5
        FCB     $4F,$52,$44,$45,$D2      ; "ORDER"

c_order FDB     do_col                   ; : ORDER
p_order FDB     c_cr                     ; CR
        FDB     c_dotqt                  ; (.") len=9
        FCB     $09
        FDB     $436F,$6E74,$6578,$742D  ; "Context-"
        FCB     $20                      ; " "
        FDB     c_conte,c_lit_           ; CONTEXT
        FDB     $000A                    ; 10
        FDB     c_add                    ; +
        FDB     c_conte                  ; CONTEXT
        FDB     c_do_,Z20BE              ; (DO) --Z20BE--v
Z20A6   FDB     c_i                      ; I
        FDB     c_at                     ; @
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z20B8            ; ?BRANCH --Z20B8--v
        FDB     c_bodyg                  ; BODY>
        FDB     c_to_na                  ; >NAME
        FDB     c_iddot                  ; ID.
        FDB     c_space                  ; SPACE
Z20B8   FDB     c_2                      ; 2
        FDB     c_addlo,Z20A6            ; (+LOOP) --Z20A6--^
Z20BE   FDB     c_cr                     ; CR
        FDB     c_dotqt                  ; (.") len=9
        FCB     $09
        FDB     $4375,$7272,$656E,$742D  ; "Current-"
        FCB     $20                      ; " "
        FDB     c_curre                  ; CURRENT
        FDB     c_at                     ; @
        FDB     c_bodyg                  ; BODY>
        FDB     c_to_na                  ; >NAME
        FDB     c_iddot                  ; ID.
        FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; VOCABULARIES
; ==================================================================
l_voca0 FDB     l_fort1                  ; LFA -> FORTH0

n_voca0 FCB     $8C                      ; NFA -> 12
        FDB     $564F,$4341,$4255,$4C41  ; "VOCABULA"
        FCB     $52,$49,$45,$D3          ; "RIES"

c_voca0 FDB     do_col                   ; : VOCABULARIES
p_voca0 FDB     c_cr                     ; CR
        FDB     c_vocsu                  ; VOC-LINK
Z20EF   FDB     c_at                     ; @
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z2109            ; ?BRANCH --Z2109--v
        FDB     c_dup,c_lit_             ; DUP
        FDB     $000C                    ; 12
        FDB     c_sub                    ; -
        FDB     c_to_na                  ; >NAME
        FDB     c_iddot                  ; ID.
        FDB     c_space                  ; SPACE
        FDB     c_branc,Z20EF            ; BRANCH --Z20EF--^
Z2109   FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; SEAL
; ==================================================================
l_seal  FDB     l_order                  ; LFA -> ORDER

n_seal  FCB     $84                      ; NFA -> 4
        FCB     $53,$45,$41,$CC          ; "SEAL"

c_seal  FDB     do_col                   ; : SEAL
p_seal  FDB     c_lit_
        FDB     $2012                    ; 8210
        FDB     c_conte,c_lit_           ; CONTEXT
        FDB     $000A                    ; 10
        FDB     c_add                    ; +
        FDB     c_conte                  ; CONTEXT
        FDB     c_do_,Z2140              ; (DO) --Z2140--v
Z2128   FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_at                     ; @
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z213A            ; ?BRANCH --Z213A--v
        FDB     c_0                      ; 0
        FDB     c_i                      ; I
        FDB     c_exc                    ; !
Z213A   FDB     c_2                      ; 2
        FDB     c_addlo,Z2128            ; (+LOOP) --Z2128--^
Z2140   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; STARTUP
; ==================================================================
l_start FDB     l_secd0                  ; LFA -> SEC/BLK

n_start FCB     $87                      ; NFA -> 7
        FCB     $53,$54,$41,$52,$54,$55  ; "STARTU"
        FCB     $D0                      ; "P"

c_start FDB     do_col                   ; : STARTUP
p_start FDB     c_only                   ; ONLY
        FDB     c_fort0                  ; FORTH
        FDB     c_also                   ; ALSO
        FDB     c_defin                  ; DEFINITIONS
        FDB     c_decim                  ; DECIMAL
        FDB     c_termi                  ; TERMINAL
        FDB     c_cr                     ; CR
        FDB     c_dotqt                  ; (.") len=31
        FCB     $1F
        FDB     $466F,$7274,$682D,$3833  ; "Forth-83"
        FDB     $2F46,$6C65,$7839,$2020  ; "/Flex9  "
        FDB     $776D,$6620,$322F,$3835  ; "wmf 2/85"
        FCB     $20,$20,$56,$65,$72,$2E  ; "  Ver."
        FCB     $20                      ; " "
        FDB     c_versi                  ; VERSION
        FDB     c_at                     ; @
        FDB     c_0                      ; 0
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_,c_lit_              ; #
        FDB     $002E                    ; 46
        FDB     c_hold                   ; HOLD
        FDB     c_n_s                    ; #S
        FDB     c_n_gt                   ; #>
        FDB     c_type                   ; TYPE
        FDB     c_cr                     ; CR
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; WHERE
; ==================================================================
l_where FDB     l_start                  ; LFA -> STARTUP

n_where FCB     $85                      ; NFA -> 5
        FCB     $57,$48,$45,$52,$C5      ; "WHERE"

c_where FDB     do_col                   ; : WHERE
p_where FDB     c_wher0                  ; WHERE>
        FDB     c_exit                   ; EXIT
        FDB     c_at                     ; @
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z2231            ; ?BRANCH --Z2231--v
        FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_qmbra,Z2209            ; ?BRANCH --Z2209--v
        FDB     c_0                      ; 0
        FDB     c_to_in                  ; >IN
        FDB     c_exc                    ; !
        FDB     c_dotqt                  ; (.") len=6
        FCB     $06
        FCB     $42,$6C,$6F,$63,$6B,$20  ; "Block "
        FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_dot                    ; .
        FDB     c_dotqt                  ; (.") len=7
        FCB     $07
        FCB     $2D,$20,$6C,$69,$6E,$65  ; "- line"
        FCB     $20                      ; " "
        FDB     c_lit_
        FDB     $0040                    ; 64
        FDB     c_divmo                  ; /MOD
        FDB     c_dup                    ; DUP
        FDB     c_dot                    ; .
        FDB     c_cr,c_lit_              ; CR
        FDB     $0040                    ; 64
        FDB     c_ast                    ; *
        FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_block                  ; BLOCK
        FDB     c_add                    ; +
        FDB     c_pad,c_lit_             ; PAD
        FDB     $0040                    ; 64
        FDB     c_cmove                  ; CMOVE
        FDB     c_pad,c_lit_             ; PAD
        FDB     $0040                    ; 64
        FDB     c_subtr                  ; -TRAILING
        FDB     c_branc,Z220F            ; BRANCH --Z220F--v
Z2209   FDB     c_tib                    ; TIB
        FDB     c_n_tib                  ; #TIB
        FDB     c_at                     ; @
Z220F   FDB     c_type                   ; TYPE
        FDB     c_cr                     ; CR
        FDB     c_1sub                   ; 1-
        FDB     c_0                      ; 0
        FDB     c_max                    ; MAX
        FDB     c_0                      ; 0
        FDB     c_qmdo_,Z2229            ; (?DO) --Z2229--v
Z221F   FDB     c_lit_
        FDB     $002E                    ; 46
        FDB     c_emit                   ; EMIT
        FDB     c_loop_,Z221F            ; (LOOP) --Z221F--^
Z2229   FDB     c_lit_
        FDB     $005E                    ; 94
        FDB     c_emit                   ; EMIT
        FDB     c_cr                     ; CR
Z2231   FDB     c_semi_                  ; (;)

; ==================================================================
; REPORT
; ==================================================================
l_repor FDB     l_rdwt                   ; LFA -> RDWT

n_repor FCB     $86                      ; NFA -> 6
        FCB     $52,$45,$50,$4F,$52,$D4  ; "REPORT"

c_repor FDB     do_col                   ; : REPORT
p_repor FDB     c_termi                  ; TERMINAL
        FDB     c_where                  ; WHERE
        FDB     c_type                   ; TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; +THRU
; ==================================================================
l_addth FDB     l_where                  ; LFA -> WHERE

n_addth FCB     $85                      ; NFA -> 5
        FCB     $2B,$54,$48,$52,$D5      ; "+THRU"

c_addth FDB     do_col                   ; : +THRU
p_addth FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_thru                   ; THRU
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?
; ==================================================================
l_qm    FDB     l_addth                  ; LFA -> +THRU

n_qm    FCB     $81                      ; NFA -> 1
        FCB     $BF                      ; "?"

c_qm    FDB     do_col                   ; : ?
p_qm    FDB     c_at                     ; @
        FDB     c_dot                    ; .
        FDB     c_semi_                  ; (;)

; ==================================================================
; .S
; ==================================================================
l_dots  FDB     l_repor                  ; LFA -> REPORT

n_dots  FCB     $82                      ; NFA -> 2
        FCB     $2E,$D3                  ; ".S"

c_dots  FDB     do_col                   ; : .S
p_dots  FDB     c_depth                  ; DEPTH
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z2297            ; ?BRANCH --Z2297--v
        FDB     c_cr                     ; CR
        FDB     c_0                      ; 0
        FDB     c_swap                   ; SWAP
        FDB     c_1sub                   ; 1-
        FDB     c_do_,Z2297              ; (DO) --Z2297--v
Z228B   FDB     c_i                      ; I
        FDB     c_pick                   ; PICK
        FDB     c_dot                    ; .
        FDB     c_sub1                   ; -1
        FDB     c_addlo,Z228B            ; (+LOOP) --Z228B--^
Z2297   FDB     c_semi_                  ; (;)

; ==================================================================
; >TYPE
; ==================================================================
l_to_ty FDB     l_dots                   ; LFA -> .S

n_to_ty FCB     $85                      ; NFA -> 5
        FCB     $3E,$54,$59,$50,$C5      ; ">TYPE"

c_to_ty FDB     do_col                   ; : >TYPE
p_to_ty FDB     c_to_r                   ; >R
        FDB     c_pad                    ; PAD
        FDB     c_rat                    ; R@
        FDB     c_cmove                  ; CMOVE
        FDB     c_pad                    ; PAD
        FDB     c_from_                  ; R>
        FDB     c_type                   ; TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?BRK
; ==================================================================
l_qmbrk FDB     l_qm                     ; LFA -> ?

n_qmbrk FCB     $84                      ; NFA -> 4
        FCB     $3F,$42,$52,$CB          ; "?BRK"

c_qmbrk FDB     do_col                   ; : ?BRK
p_qmbrk FDB     c_qmkey                  ; ?KEY
        FDB     c_dup                    ; DUP
        FDB     c_qmbra,Z22CE            ; ?BRANCH --Z22CE--v
        FDB     c_drop                   ; DROP
        FDB     c_key,c_lit_             ; KEY
        FDB     $000D                    ; 13
        FDB     c_eq                     ; =
Z22CE   FDB     c_semi_                  ; (;)

; ==================================================================
; 2H.
; ==================================================================
l_2hdot FDB     l_to_ty                  ; LFA -> >TYPE

n_2hdot FCB     $83                      ; NFA -> 3
        FCB     $32,$48,$AE              ; "2H."

c_2hdot FDB     do_col                   ; : 2H.
p_2hdot FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_swap                   ; SWAP
        FDB     c_0                      ; 0
        FDB     c_hex                    ; HEX
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_gt                   ; #>
        FDB     c_type                   ; TYPE
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; 4H.
; ==================================================================
l_4hdot FDB     l_ns_                    ; LFA -> (NS)

n_4hdot FCB     $83                      ; NFA -> 3
        FCB     $34,$48,$AE              ; "4H."

c_4hdot FDB     do_col                   ; : 4H.
p_4hdot FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_swap                   ; SWAP
        FDB     c_0                      ; 0
        FDB     c_hex                    ; HEX
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_gt                   ; #>
        FDB     c_type                   ; TYPE
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; CH.
; ==================================================================
l_chdot FDB     l_qmbrk                  ; LFA -> ?BRK

n_chdot FCB     $83                      ; NFA -> 3
        FCB     $43,$48,$AE              ; "CH."

c_chdot FDB     do_col                   ; : CH.
p_chdot FDB     c_lit_
        FDB     $007F                    ; 127
        FDB     c_and                    ; AND
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0020                    ; 32
        FDB     c_lt                     ; <
        FDB     c_over,c_lit_            ; OVER
        FDB     $007E                    ; 126
        FDB     c_gt                     ; >
        FDB     c_or                     ; OR
        FDB     c_qmbra,Z2342            ; ?BRANCH --Z2342--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $002E                    ; 46
Z2342   FDB     c_emit                   ; EMIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; NCH.
; ==================================================================
l_nchdo FDB     l_2hdot                  ; LFA -> 2H.

n_nchdo FCB     $84                      ; NFA -> 4
        FCB     $4E,$43,$48,$AE          ; "NCH."

c_nchdo FDB     do_col                   ; : NCH.
p_nchdo FDB     c_over                   ; OVER
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z2363              ; (DO) --Z2363--v
Z2359   FDB     c_i                      ; I
        FDB     c_cat                    ; C@
        FDB     c_chdot                  ; CH.
        FDB     c_loop_,Z2359            ; (LOOP) --Z2359--^
Z2363   FDB     c_semi_                  ; (;)

; ==================================================================
; N2H.
; ==================================================================
l_n2hdo FDB     l_nchdo                  ; LFA -> NCH.

n_n2hdo FCB     $84                      ; NFA -> 4
        FCB     $4E,$32,$48,$AE          ; "N2H."

c_n2hdo FDB     do_col                   ; : N2H.
p_n2hdo FDB     c_over                   ; OVER
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z2384              ; (DO) --Z2384--v
Z2378   FDB     c_space                  ; SPACE
        FDB     c_i                      ; I
        FDB     c_cat                    ; C@
        FDB     c_2hdot                  ; 2H.
        FDB     c_loop_,Z2378            ; (LOOP) --Z2378--^
Z2384   FDB     c_semi_                  ; (;)

; ==================================================================
; HDR
; ==================================================================
l_hdr   FDB     l_4hdot                  ; LFA -> 4H.

n_hdr   FCB     $83                      ; NFA -> 3
        FCB     $48,$44,$D2              ; "HDR"

c_hdr   FDB     do_col                   ; : HDR
p_hdr   FDB     c_cr                     ; CR
        FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_hex,c_lit_             ; HEX
        FDB     $0004                    ; 4
        FDB     c_spac0,c_lit_           ; SPACES
        FDB     $0010                    ; 16
        FDB     c_0                      ; 0
        FDB     c_do_,Z23B2              ; (DO) --Z23B2--v
Z23A6   FDB     c_i,c_lit_               ; I
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_loop_,Z23A6            ; (LOOP) --Z23A6--^
Z23B2   FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_spac0                  ; SPACES
        FDB     c_dotqt                  ; (.") len=16
        FCB     $10
        FDB     $3031,$3233,$3435,$3637  ; "01234567"
        FDB     $3839,$4142,$4344,$4546  ; "89ABCDEF"
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; 1LN
; ==================================================================
l_1ln   FDB     l_emit                   ; LFA -> EMIT

n_1ln   FCB     $83                      ; NFA -> 3
        FCB     $31,$4C,$CE              ; "1LN"

c_1ln   FDB     do_col                   ; : 1LN
p_1ln   FDB     c_cr                     ; CR
        FDB     c_2                      ; 2
        FDB     c_pick                   ; PICK
        FDB     c_4hdot,c_lit_           ; 4H.
        FDB     $0010                    ; 16
        FDB     c_over                   ; OVER
        FDB     c_sub,c_lit_             ; -
        FDB     $0003                    ; 3
        FDB     c_ast                    ; *
        FDB     c_2                      ; 2
        FDB     c_pick                   ; PICK
        FDB     c_add,c_lit_             ; +
        FDB     $0003                    ; 3
        FDB     c_add                    ; +
        FDB     c_to_r                   ; >R
        FDB     c_over,c_lit_            ; OVER
        FDB     $0003                    ; 3
        FDB     c_ast                    ; *
        FDB     c_spac0                  ; SPACES
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_to_r                   ; >R
        FDB     c_add                    ; +
        FDB     c_from_                  ; R>
        FDB     c_2dup                   ; 2DUP
        FDB     c_n2hdo                  ; N2H.
        FDB     c_from_                  ; R>
        FDB     c_spac0                  ; SPACES
        FDB     c_nchdo                  ; NCH.
        FDB     c_semi_                  ; (;)

; ==================================================================
; DUMP
; ==================================================================
l_dump  FDB     l_hdr                    ; LFA -> HDR

n_dump  FCB     $84                      ; NFA -> 4
        FCB     $44,$55,$4D,$D0          ; "DUMP"

c_dump  FDB     do_col                   ; : DUMP
p_dump  FDB     c_cr                     ; CR
        FDB     c_hdr                    ; HDR
        FDB     c_swap                   ; SWAP
        FDB     c_dup,c_lit_             ; DUP
        FDB     $FFF0                    ; -16
        FDB     c_and                    ; AND
        FDB     c_swap,c_lit_            ; SWAP
        FDB     $000F                    ; 15
        FDB     c_and                    ; AND
        FDB     c_rot                    ; ROT
        FDB     c_over                   ; OVER
        FDB     c_add,c_lit_             ; +
        FDB     $0010                    ; 16
        FDB     c_divmo                  ; /MOD
        FDB     c_swap                   ; SWAP
        FDB     c_to_r                   ; >R
        FDB     c_0                      ; 0
        FDB     c_qmdo_,Z247E            ; (?DO) --Z247E--v
Z2452   FDB     c_2dup,c_lit_            ; 2DUP
        FDB     $0010                    ; 16
        FDB     c_1ln                    ; 1LN
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0010                    ; 16
        FDB     c_add                    ; +
        FDB     c_qmbrk                  ; ?BRK
        FDB     c_qmbra,Z2478            ; ?BRANCH --Z2478--v
        FDB     c_drop                   ; DROP
        FDB     c_cr                     ; CR
        FDB     c_dotqt                  ; (.") len=7
        FCB     $07
        FCB     $2D,$62,$72,$65,$61,$6B  ; "-break"
        FCB     $2D                      ; "-"
        FDB     c_quit                   ; QUIT
Z2478   FDB     c_0                      ; 0
        FDB     c_loop_,Z2452            ; (LOOP) --Z2452--^
Z247E   FDB     c_from_                  ; R>
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z248C            ; ?BRANCH --Z248C--v
        FDB     c_1ln                    ; 1LN
        FDB     c_branc,Z248E            ; BRANCH --Z248E--v
Z248C   FDB     c_2drop                  ; 2DROP
Z248E   FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; CASE
; ==================================================================
l_case  FDB     l_chdot                  ; LFA -> CH.

n_case  FCB     $C4                      ; NFA -> 4 IMMEDIATE
        FCB     $43,$41,$53,$C5          ; "CASE"

c_case  FDB     do_col                   ; : CASE
p_case  FDB     c_qmcom                  ; ?COMP
        FDB     c_csp                    ; CSP
        FDB     c_at                     ; @
        FDB     c_spat                   ; SP@
        FDB     c_csp                    ; CSP
        FDB     c_exc,c_lit_             ; !
        FDB     $0004                    ; 4
        FDB     c_semi_                  ; (;)

; ==================================================================
; OF
; ==================================================================
l_of    FDB     l_case                   ; LFA -> CASE

n_of    FCB     $C2                      ; NFA -> 2 IMMEDIATE
        FCB     $4F,$C6                  ; "OF"

c_of    FDB     do_col                   ; : OF
p_of    FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_over                   ; OVER
        FDB     c_compi                  ; COMPILE
        FDB     c_eq                     ; =
        FDB     c_compi                  ; COMPILE
        FDB     c_qmbra,c_to_ma          ; ?BRANCH --' >MARK--^
        FDB     c_compi                  ; COMPILE
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0005                    ; 5
        FDB     c_semi_                  ; (;)

; ==================================================================
; ENDOF
; ==================================================================
l_endof FDB     l_1ln                    ; LFA -> 1LN

n_endof FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $45,$4E,$44,$4F,$C6      ; "ENDOF"

c_endof FDB     do_col                   ; : ENDOF
p_endof FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_branc,c_to_ma          ; BRANCH --' >MARK--^
        FDB     c_swap                   ; SWAP
        FDB     c_to_re,c_lit_           ; >RESOLVE
        FDB     $0004                    ; 4
        FDB     c_semi_                  ; (;)

; ==================================================================
; ENDCASE
; ==================================================================
l_endca FDB     l_endof                  ; LFA -> ENDOF

n_endca FCB     $C7                      ; NFA -> 7 IMMEDIATE
        FCB     $45,$4E,$44,$43,$41,$53  ; "ENDCAS"
        FCB     $C5                      ; "E"

c_endca FDB     do_col                   ; : ENDCASE
p_endca FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_drop                   ; DROP
Z2508   FDB     c_spat                   ; SP@
        FDB     c_csp                    ; CSP
        FDB     c_at                     ; @
        FDB     c_eq                     ; =
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z251C            ; ?BRANCH --Z251C--v
        FDB     c_to_re                  ; >RESOLVE
        FDB     c_branc,Z2508            ; BRANCH --Z2508--^
Z251C   FDB     c_csp                    ; CSP
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; <IP
; ==================================================================
l_from3 FDB     l_dump                   ; LFA -> DUMP

n_from3 FCB     $83                      ; NFA -> 3
        FCB     $3C,$49,$D0              ; "<IP"

c_from3 FDB     do_var                   ; VARIABLE <IP
p_from3 FDB     $0000                    ; 0

; ==================================================================
; IP>
; ==================================================================
l_ipgt  FDB     l_endca                  ; LFA -> ENDCASE

n_ipgt  FCB     $83                      ; NFA -> 3
        FCB     $49,$50,$BE              ; "IP>"

c_ipgt  FDB     do_var                   ; VARIABLE IP>
p_ipgt  FDB     $0000                    ; 0

; ==================================================================
; 'DEBUG
; ==================================================================
l_tckde FDB     l_of                     ; LFA -> OF

n_tckde FCB     $86                      ; NFA -> 6
        FCB     $27,$44,$45,$42,$55,$C7  ; "'DEBUG"

c_tckde FDB     do_var                   ; VARIABLE 'DEBUG
p_tckde FDB     $26AA                    ; 9898

; ==================================================================
; >DNEXT
; ==================================================================
l_to_dn FDB     l_n2hdo                  ; LFA -> N2H.

n_to_dn FCB     $86                      ; NFA -> 6
        FCB     $3E,$44,$4E,$45,$58,$D4  ; ">DNEXT"

c_to_dn FDB     do_var                   ; VARIABLE >DNEXT
p_to_dn FDB     $10BC                    ; 4284
        FCB     $25,$2A,$23,$0B,$10,$BC,$25,$34,$22,$05,$BE,$25,$41
        FCB     $6E,$94,$AE,$A1,$6E,$94

; ==================================================================
; GOBUG
; ==================================================================
l_gobug FDB     l_tckde                  ; LFA -> 'DEBUG

n_gobug FCB     $85                      ; NFA -> 5
        FCB     $47,$4F,$42,$55,$C7      ; "GOBUG"

c_gobug FDB     p_gobug                  ; ASSEMBLER
p_gobug PULS    Y
        LDX     ,Y++
        FCB     $6E
        FCB     $94

; ==================================================================
; UNBUG
; ==================================================================
l_unbug FDB     l_ipgt                   ; LFA -> IP>

n_unbug FCB     $85                      ; NFA -> 5
        FCB     $55,$4E,$42,$55,$C7      ; "UNBUG"

c_unbug FDB     do_col                   ; : UNBUG
p_unbug FDB     c_lit_
        FDB     $6E94,c_lit_             ; 28308
        FDB     $AEA1                    ; -20831
        FDB     c_next                   ; >NEXT
        FDB     c_2exc                   ; 2!
        FDB     c_semi_                  ; (;)

; ==================================================================
; BUG
; ==================================================================
l_bug   FDB     l_to_dn                  ; LFA -> >DNEXT

n_bug   FCB     $83                      ; NFA -> 3
        FCB     $42,$55,$C7              ; "BUG"

c_bug   FDB     do_col                   ; : BUG
p_bug   FDB     c_to_dn,c_lit_           ; >DNEXT
        FDB     $127E                    ; 4734
        FDB     c_next                   ; >NEXT
        FDB     c_2exc                   ; 2!
        FDB     c_semi_                  ; (;)

; ==================================================================
; (RSM)
; ==================================================================
l_rsm_  FDB     l_from3                  ; LFA -> <IP

n_rsm_  FCB     $85                      ; NFA -> 5
        FCB     $28,$52,$53,$4D,$A9      ; "(RSM)"

c_rsm_  FDB     do_var                   ; VARIABLE (RSM)
p_rsm_  FDB     $0000                    ; 0

; ==================================================================
; (RUN)
; ==================================================================
l_run_  FDB     l_rsm_                   ; LFA -> (RSM)

n_run_  FCB     $85                      ; NFA -> 5
        FCB     $28,$52,$55,$4E,$A9      ; "(RUN)"

c_run_  FDB     do_var                   ; VARIABLE (RUN)
p_run_  FDB     $0000                    ; 0

; ==================================================================
; (ST)
; ==================================================================
l_st_   FDB     l_run_                   ; LFA -> (RUN)

n_st_   FCB     $84                      ; NFA -> 4
        FCB     $28,$53,$54,$A9          ; "(ST)"

c_st_   FDB     do_var                   ; VARIABLE (ST)
p_st_   FDB     $0000                    ; 0

; ==================================================================
; RESUME
; ==================================================================
l_resum FDB     l_bug                    ; LFA -> BUG

n_resum FCB     $86                      ; NFA -> 6
        FCB     $52,$45,$53,$55,$4D,$C5  ; "RESUME"

c_resum FDB     do_col                   ; : RESUME
p_resum FDB     c_sub1                   ; -1
        FDB     c_rsm_                   ; (RSM)
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEBUG
; ==================================================================
l_debug FDB     l_st_                    ; LFA -> (ST)

n_debug FCB     $85                      ; NFA -> 5
        FCB     $44,$45,$42,$55,$C7      ; "DEBUG"

c_debug FDB     do_col                   ; : DEBUG
p_debug FDB     c_unbug                  ; UNBUG
        FDB     c_0                      ; 0
        FDB     c_run_                   ; (RUN)
        FDB     c_exc                    ; !
        FDB     c_tck                    ; '
        FDB     c_dup                    ; DUP
        FDB     c_from3                  ; <IP
        FDB     c_exc                    ; !
Z25EF   FDB     c_1add                   ; 1+
        FDB     c_dup                    ; DUP
        FDB     c_at,c_lit_              ; @
        FDB     $003C                    ; ' (;)
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z25EF            ; ?BRANCH --Z25EF--^
        FDB     c_ipgt                   ; IP>
        FDB     c_exc                    ; !
        FDB     c_bug                    ; BUG
        FDB     c_semi_                  ; (;)

; ==================================================================
; (4H)
; ==================================================================
l_4h_   FDB     l_debug                  ; LFA -> DEBUG

n_4h_   FCB     $84                      ; NFA -> 4
        FCB     $28,$34,$48,$A9          ; "(4H)"

c_4h_   FDB     do_col                   ; : (4H)
p_4h_   FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_swap                   ; SWAP
        FDB     c_hex                    ; HEX
        FDB     c_0                      ; 0
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_gt                   ; #>
        FDB     c_type                   ; TYPE
        FDB     c_space                  ; SPACE
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; (DSPLY)
; ==================================================================
l_dsply FDB     l_4h_                    ; LFA -> (4H)

n_dsply FCB     $87                      ; NFA -> 7
        FCB     $28,$44,$53,$50,$4C,$59  ; "(DSPLY"
        FCB     $A9                      ; ")"

c_dsply FDB     do_col                   ; : (DSPLY)
p_dsply FDB     c_decds                  ; DECDSPLY3
        FDB     c_exit                   ; EXIT
        FDB     c_0                      ; 0
        FDB     c_out                    ; OUT
        FDB     c_exc                    ; !
        FDB     c_depth,c_lit_           ; DEPTH
        FDB     $000A                    ; 10
        FDB     c_min                    ; MIN
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z266A            ; ?BRANCH --Z266A--v
        FDB     c_1sub                   ; 1-
        FDB     c_0                      ; 0
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z266A              ; (DO) --Z266A--v
Z265E   FDB     c_i                      ; I
        FDB     c_pick                   ; PICK
        FDB     c_4h_                    ; (4H)
        FDB     c_sub1                   ; -1
        FDB     c_addlo,Z265E            ; (+LOOP) --Z265E--^
Z266A   FDB     c_lit_
        FDB     $0032                    ; 50
        FDB     c_out                    ; OUT
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_spac0                  ; SPACES
        FDB     c_from_                  ; R>
        FDB     c_at                     ; @
        FDB     c_to_na                  ; >NAME
        FDB     c_iddot                  ; ID.
        FDB     c_semi_                  ; (;)

; ==================================================================
; (EXEC)
; ==================================================================
l_exec_ FDB     l_dsply                  ; LFA -> (DSPLY)

n_exec_ FCB     $86                      ; NFA -> 6
        FCB     $28,$45,$58,$45,$43,$A9  ; "(EXEC)"

c_exec_ FDB     do_col                   ; : (EXEC)
p_exec_ FDB     c_cr                     ; CR
        FDB     c_query                  ; QUERY
        FDB     c_inter                  ; INTERPRET
        FDB     c_dotqt                  ; (.") len=4
        FCB     $04
        FCB     $28,$6F,$6B,$29          ; "(ok)"
        FDB     c_rsm_                   ; (RSM)
        FDB     c_at                     ; @
        FDB     c_qmbra,p_exec_          ; ?BRANCH --p_exec_--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; TRACE
; ==================================================================
l_trace FDB     l_exec_                  ; LFA -> (EXEC)

n_trace FCB     $85                      ; NFA -> 5
        FCB     $54,$52,$41,$43,$C5      ; "TRACE"

c_trace FDB     do_col                   ; : TRACE
p_trace FDB     c_unbug                  ; UNBUG
        FDB     c_qmsta                  ; ?STACK
        FDB     c_rat                    ; R@
        FDB     c_dsply                  ; (DSPLY)
        FDB     c_qmkey                  ; ?KEY
        FDB     c_0eq                    ; 0=
        FDB     c_run_                   ; (RUN)
        FDB     c_at                     ; @
        FDB     c_and                    ; AND
        FDB     c_dup                    ; DUP
        FDB     c_run_                   ; (RUN)
        FDB     c_exc                    ; !
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z26D0            ; ?BRANCH --Z26D0--v
        FDB     c_bug                    ; BUG
        FDB     c_branc,Z2739            ; BRANCH --Z2739--v
Z26D0   FDB     c_key,c_lit_             ; KEY
        FDB     $0020                    ; 32
        FDB     c_or                     ; OR
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0071                    ; 113
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z26EF            ; ?BRANCH --Z26EF--v
        FDB     c_cr                     ; CR
        FDB     c_dotqt                  ; (.") len=4
        FCB     $04
        FCB     $51,$75,$69,$74          ; "Quit"
        FDB     c_quit                   ; QUIT
Z26EF   FDB     c_dup,c_lit_             ; DUP
        FDB     $0066                    ; 102
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z2719            ; ?BRANCH --Z2719--v
        FDB     c_drop                   ; DROP
        FDB     c_0                      ; 0
        FDB     c_rsm_                   ; (RSM)
        FDB     c_exc                    ; !
        FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_st_                    ; (ST)
        FDB     c_exc                    ; !
        FDB     c_lsq                    ; [
        FDB     c_exec_                  ; (EXEC)
        FDB     c_st_                    ; (ST)
        FDB     c_at                     ; @
        FDB     c_state                  ; STATE
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
Z2719   FDB     c_dup,c_lit_             ; DUP
        FDB     $0074                    ; 116
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z272B            ; ?BRANCH --Z272B--v
        FDB     c_dup                    ; DUP
        FDB     c_run_                   ; (RUN)
        FDB     c_exc                    ; !
Z272B   FDB     c_dup,c_lit_             ; DUP
        FDB     $0075                    ; 117
        FDB     c_sub                    ; -
        FDB     c_qmbra,Z2739            ; ?BRANCH --Z2739--v
        FDB     c_bug                    ; BUG
Z2739   FDB     c_qmbra,p_trace          ; ?BRANCH --p_trace--^
        FDB     c_gobug                  ; GOBUG
        FDB     c_semi_                  ; (;)

; ==================================================================
; EDITOR
; ==================================================================
l_edito FDB     l_unbug                  ; LFA -> UNBUG

n_edito FCB     $86                      ; NFA -> 6
        FCB     $45,$44,$49,$54,$4F,$D2  ; "EDITOR"

c_edito FDB     do_voc
p_edito FDB     l_l_,l_under,l_new,l_g,$16F9

; ==================================================================
; (MATCH)
; ==================================================================
l_match FDB     $0000

n_match FCB     $87                      ; NFA -> 7
        FCB     $28,$4D,$41,$54,$43,$48  ; "(MATCH"
        FCB     $A9                      ; ")"

c_match FDB     do_col                   ; : (MATCH)
p_match FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z278E            ; ?BRANCH --Z278E--v
        FDB     c_over                   ; OVER
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z278A              ; (DO) --Z278A--v
Z2772   FDB     c_dup                    ; DUP
        FDB     c_cat                    ; C@
        FDB     c_i                      ; I
        FDB     c_cat                    ; C@
        FDB     c_sub                    ; -
        FDB     c_qmbra,Z2784            ; ?BRANCH --Z2784--v
        FDB     c_0eq                    ; 0=
        FDB     c_leave                  ; (LEAVE)
Z2784   FDB     c_1add                   ; 1+
        FDB     c_loop_,Z2772            ; (LOOP) --Z2772--^
Z278A   FDB     c_branc,Z2792            ; BRANCH --Z2792--v
Z278E   FDB     c_drop                   ; DROP
        FDB     c_0eq                    ; 0=
Z2792   FDB     c_semi_                  ; (;)

; ==================================================================
; MATCH
; ==================================================================
l_matc0 FDB     $0000

n_matc0 FCB     $85                      ; NFA -> 5
        FCB     $4D,$41,$54,$43,$C8      ; "MATCH"

c_matc0 FDB     do_col                   ; : MATCH
p_matc0 FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_over                   ; OVER
        FDB     c_over                   ; OVER
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_rot                    ; ROT
        FDB     c_to_r                   ; >R
        FDB     c_rot                    ; ROT
        FDB     c_from_                  ; R>
        FDB     c_over                   ; OVER
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z27E8              ; (DO) --Z27E8--v
Z27BC   FDB     c_over                   ; OVER
        FDB     c_over                   ; OVER
        FDB     c_i                      ; I
        FDB     c_swap                   ; SWAP
        FDB     c_match                  ; (MATCH)
        FDB     c_qmbra,Z27E4            ; ?BRANCH --Z27E4--v
        FDB     c_to_r                   ; >R
        FDB     c_drop                   ; DROP
        FDB     c_drop                   ; DROP
        FDB     c_from_                  ; R>
        FDB     c_sub                    ; -
        FDB     c_i                      ; I
        FDB     c_swap                   ; SWAP
        FDB     c_sub                    ; -
        FDB     c_0                      ; 0
        FDB     c_swap                   ; SWAP
        FDB     c_0                      ; 0
        FDB     c_0                      ; 0
        FDB     c_leave                  ; (LEAVE)
Z27E4   FDB     c_loop_,Z27BC            ; (LOOP) --Z27BC--^
Z27E8   FDB     c_drop                   ; DROP
        FDB     c_drop                   ; DROP
        FDB     c_swap                   ; SWAP
        FDB     c_0eq                    ; 0=
        FDB     c_swap                   ; SWAP
        FDB     c_semi_                  ; (;)

; ==================================================================
; R#
; ==================================================================
l_rn_   FDB     $0000

n_rn_   FCB     $82                      ; NFA -> 2
        FCB     $52,$A3                  ; "R#"

c_rn_   FDB     do_var                   ; VARIABLE R#
p_rn_   FDB     $0000                    ; 0

; ==================================================================
; C/L
; ==================================================================
l_cdivl FDB     $0000

n_cdivl FCB     $83                      ; NFA -> 3
        FCB     $43,$2F,$CC              ; "C/L"

c_cdivl FDB     do_const                 ; CONSTANT C/L
p_cdivl FDB     n_execu                  ; 64

; ==================================================================
; CRS
; ==================================================================
l_crs   FDB     l_cdivl                  ; LFA -> C/L

n_crs   FCB     $83                      ; NFA -> 3
        FCB     $43,$52,$D3              ; "CRS"

c_crs   FDB     do_const                 ; CONSTANT CRS
p_crs   FDB     l_spat                   ; 94

; ==================================================================
; TEXT
; ==================================================================
l_text  FDB     l_match                  ; LFA -> (MATCH)

n_text  FCB     $84                      ; NFA -> 4
        FCB     $54,$45,$58,$D4          ; "TEXT"

c_text  FDB     do_col                   ; : TEXT
p_text  FDB     c_pad                    ; PAD
        FDB     c_cdivl                  ; C/L
        FDB     c_1add                   ; 1+
        FDB     c_blank                  ; BLANK
        FDB     c_word                   ; WORD
        FDB     c_pad                    ; PAD
        FDB     c_over                   ; OVER
        FDB     c_cat                    ; C@
        FDB     c_cdivl                  ; C/L
        FDB     c_min                    ; MIN
        FDB     c_1add                   ; 1+
        FDB     c_cmove                  ; CMOVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; LINE
; ==================================================================
l_line  FDB     l_text                   ; LFA -> TEXT

n_line  FCB     $84                      ; NFA -> 4
        FCB     $4C,$49,$4E,$C5          ; "LINE"

c_line  FDB     do_col                   ; : LINE
p_line  FDB     c_dup,c_lit_             ; DUP
        FDB     $FFF0                    ; -16
        FDB     c_and                    ; AND
        FDB     c_abort                  ; (ABORT") len=10
        FCB     $0A
        FDB     $4F66,$6620,$7363,$7265  ; "Off scre"
        FCB     $65,$6E                  ; "en"
        FDB     c_cdivl                  ; C/L
        FDB     c_ast                    ; *
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_block                  ; BLOCK
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; #LOCATE
; ==================================================================
l_n_loc FDB     l_crs                    ; LFA -> CRS

n_n_loc FCB     $87                      ; NFA -> 7
        FCB     $23,$4C,$4F,$43,$41,$54  ; "#LOCAT"
        FCB     $C5                      ; "E"

c_n_loc FDB     do_col                   ; : #LOCATE
p_n_loc FDB     c_rn_                    ; R#
        FDB     c_at                     ; @
        FDB     c_cdivl                  ; C/L
        FDB     c_divmo                  ; /MOD
        FDB     c_semi_                  ; (;)

; ==================================================================
; #LEAD
; ==================================================================
l_n_lea FDB     l_n_loc                  ; LFA -> #LOCATE

n_n_lea FCB     $85                      ; NFA -> 5
        FCB     $23,$4C,$45,$41,$C4      ; "#LEAD"

c_n_lea FDB     do_col                   ; : #LEAD
p_n_lea FDB     c_n_loc                  ; #LOCATE
        FDB     c_line                   ; LINE
        FDB     c_swap                   ; SWAP
        FDB     c_semi_                  ; (;)

; ==================================================================
; #LAG
; ==================================================================
l_n_lag FDB     l_n_lea                  ; LFA -> #LEAD

n_n_lag FCB     $84                      ; NFA -> 4
        FCB     $23,$4C,$41,$C7          ; "#LAG"

c_n_lag FDB     do_col                   ; : #LAG
p_n_lag FDB     c_n_lea                  ; #LEAD
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_add                    ; +
        FDB     c_cdivl                  ; C/L
        FDB     c_from_                  ; R>
        FDB     c_sub                    ; -
        FDB     c_semi_                  ; (;)

; ==================================================================
; -MOVE
; ==================================================================
l_submo FDB     l_matc0                  ; LFA -> MATCH

n_submo FCB     $85                      ; NFA -> 5
        FCB     $2D,$4D,$4F,$56,$C5      ; "-MOVE"

c_submo FDB     do_col                   ; : -MOVE
p_submo FDB     c_line                   ; LINE
        FDB     c_cdivl                  ; C/L
        FDB     c_cmove                  ; CMOVE
        FDB     c_updat                  ; UPDATE
        FDB     c_semi_                  ; (;)

; ==================================================================
; BUF-MOVE
; ==================================================================
l_bufsu FDB     l_rn_                    ; LFA -> R#

n_bufsu FCB     $88                      ; NFA -> 8
        FDB     $4255,$462D,$4D4F,$56C5  ; "BUF-MOVE"

c_bufsu FDB     do_col                   ; : BUF-MOVE
p_bufsu FDB     c_pad                    ; PAD
        FDB     c_cat                    ; C@
        FDB     c_qmbra,Z28D8            ; ?BRANCH --Z28D8--v
        FDB     c_pad                    ; PAD
        FDB     c_swap                   ; SWAP
        FDB     c_cdivl                  ; C/L
        FDB     c_1add                   ; 1+
        FDB     c_cmove                  ; CMOVE
        FDB     c_branc,Z28DA            ; BRANCH --Z28DA--v
Z28D8   FDB     c_drop                   ; DROP
Z28DA   FDB     c_semi_                  ; (;)

; ==================================================================
; >LINE#
; ==================================================================
l_to_l0 FDB     l_bufsu                  ; LFA -> BUF-MOVE

n_to_l0 FCB     $86                      ; NFA -> 6
        FCB     $3E,$4C,$49,$4E,$45,$A3  ; ">LINE#"

c_to_l0 FDB     do_col                   ; : >LINE#
p_to_l0 FDB     c_n_loc                  ; #LOCATE
        FDB     c_swap                   ; SWAP
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; FIND-BUF
; ==================================================================
l_finds FDB     l_to_l0                  ; LFA -> >LINE#

n_finds FCB     $88                      ; NFA -> 8
        FDB     $4649,$4E44,$2D42,$55C6  ; "FIND-BUF"

c_finds FDB     do_col                   ; : FIND-BUF
p_finds FDB     c_pad                    ; PAD
        FDB     c_cdivl                  ; C/L
        FDB     c_add                    ; +
        FDB     c_1add                   ; 1+
        FDB     c_semi_                  ; (;)

; ==================================================================
; INSERT-BUF
; ==================================================================
l_inser FDB     l_submo                  ; LFA -> -MOVE

n_inser FCB     $8A                      ; NFA -> 10
        FDB     $494E,$5345,$5254,$2D42  ; "INSERT-B"
        FCB     $55,$C6                  ; "UF"

c_inser FDB     do_col                   ; : INSERT-BUF
p_inser FDB     c_finds                  ; FIND-BUF
        FDB     c_cdivl                  ; C/L
        FDB     c_add                    ; +
        FDB     c_1add                   ; 1+
        FDB     c_semi_                  ; (;)

; ==================================================================
; (HOLD)
; ==================================================================
l_hold_ FDB     l_line                   ; LFA -> LINE

n_hold_ FCB     $86                      ; NFA -> 6
        FCB     $28,$48,$4F,$4C,$44,$A9  ; "(HOLD)"

c_hold_ FDB     do_col                   ; : (HOLD)
p_hold_ FDB     c_line                   ; LINE
        FDB     c_inser                  ; INSERT-BUF
        FDB     c_1add                   ; 1+
        FDB     c_cdivl                  ; C/L
        FDB     c_dup                    ; DUP
        FDB     c_inser                  ; INSERT-BUF
        FDB     c_cexc                   ; C!
        FDB     c_cmove                  ; CMOVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; (KILL)
; ==================================================================
l_kill_ FDB     l_hold_                  ; LFA -> (HOLD)

n_kill_ FCB     $86                      ; NFA -> 6
        FCB     $28,$4B,$49,$4C,$4C,$A9  ; "(KILL)"

c_kill_ FDB     do_col                   ; : (KILL)
p_kill_ FDB     c_line                   ; LINE
        FDB     c_cdivl                  ; C/L
        FDB     c_blank                  ; BLANK
        FDB     c_updat                  ; UPDATE
        FDB     c_semi_                  ; (;)

; ==================================================================
; (SPREAD)
; ==================================================================
l_sprea FDB     l_kill_                  ; LFA -> (KILL)

n_sprea FCB     $88                      ; NFA -> 8
        FDB     $2853,$5052,$4541,$44A9  ; "(SPREAD)"

c_sprea FDB     do_col                   ; : (SPREAD)
p_sprea FDB     c_to_l0                  ; >LINE#
        FDB     c_dup                    ; DUP
        FDB     c_line                   ; LINE
        FDB     c_dup                    ; DUP
        FDB     c_cdivl                  ; C/L
        FDB     c_add,c_lit_             ; +
        FDB     $000F,c_lit_             ; 15
        FDB     $0003                    ; 3
        FDB     c_pick                   ; PICK
        FDB     c_sub                    ; -
        FDB     c_cdivl                  ; C/L
        FDB     c_ast                    ; *
        FDB     c_cmov0                  ; CMOVE>
        FDB     c_kill_                  ; (KILL)
        FDB     c_semi_                  ; (;)

; ==================================================================
; X
; ==================================================================
l_x     FDB     l_sprea                  ; LFA -> (SPREAD)

n_x     FCB     $81                      ; NFA -> 1
        FCB     $D8                      ; "X"

c_x     FDB     do_col                   ; : X
p_x     FDB     c_to_l0                  ; >LINE#
        FDB     c_dup                    ; DUP
        FDB     c_hold_                  ; (HOLD)
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_line                   ; LINE
        FDB     c_dup                    ; DUP
        FDB     c_cdivl                  ; C/L
        FDB     c_add                    ; +
        FDB     c_swap,c_lit_            ; SWAP
        FDB     $000F                    ; 15
        FDB     c_from_                  ; R>
        FDB     c_sub                    ; -
        FDB     c_cdivl                  ; C/L
        FDB     c_ast                    ; *
        FDB     c_cmove,c_lit_           ; CMOVE
        FDB     $000F                    ; 15
        FDB     c_kill_                  ; (KILL)
        FDB     c_semi_                  ; (;)

; ==================================================================
; DISPLAY-CURSOR
; ==================================================================
l_displ FDB     l_x                      ; LFA -> X

n_displ FCB     $8E                      ; NFA -> 14
        FDB     $4449,$5350,$4C41,$592D  ; "DISPLAY-"
        FCB     $43,$55,$52,$53,$4F,$D2  ; "CURSOR"

c_displ FDB     do_col                   ; : DISPLAY-CURSOR
p_displ FDB     c_cr                     ; CR
        FDB     c_space                  ; SPACE
        FDB     c_n_lea                  ; #LEAD
        FDB     c_to_ty                  ; >TYPE
        FDB     c_crs                    ; CRS
        FDB     c_emit                   ; EMIT
        FDB     c_n_lag                  ; #LAG
        FDB     c_to_ty                  ; >TYPE
        FDB     c_n_loc                  ; #LOCATE
        FDB     c_dot                    ; .
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; T
; ==================================================================
l_t     FDB     l_displ                  ; LFA -> DISPLAY-CURSOR

n_t     FCB     $81                      ; NFA -> 1
        FCB     $D4                      ; "T"

c_t     FDB     do_col                   ; : T
p_t     FDB     c_cdivl                  ; C/L
        FDB     c_ast                    ; *
        FDB     c_rn_                    ; R#
        FDB     c_exc                    ; !
        FDB     c_displ                  ; DISPLAY-CURSOR
        FDB     c_semi_                  ; (;)

; ==================================================================
; L
; ==================================================================
l_l     FDB     l_t                      ; LFA -> T

n_l     FCB     $81                      ; NFA -> 1
        FCB     $CC                      ; "L"

c_l     FDB     do_col                   ; : L
p_l     FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_list                   ; LIST
        FDB     c_semi_                  ; (;)

; ==================================================================
; N
; ==================================================================
l_n     FDB     l_finds                  ; LFA -> FIND-BUF

n_n     FCB     $81                      ; NFA -> 1
        FCB     $CE                      ; "N"

c_n     FDB     do_col                   ; : N
p_n     FDB     c_1                      ; 1
        FDB     c_scr                    ; SCR
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; B
; ==================================================================
l_b     FDB     l_n                      ; LFA -> N

n_b     FCB     $81                      ; NFA -> 1
        FCB     $C2                      ; "B"

c_b     FDB     do_col                   ; : B
p_b     FDB     c_sub1                   ; -1
        FDB     c_scr                    ; SCR
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; (TOP)
; ==================================================================
l_top_  FDB     l_l                      ; LFA -> L

n_top_  FCB     $85                      ; NFA -> 5
        FCB     $28,$54,$4F,$50,$A9      ; "(TOP)"

c_top_  FDB     do_col                   ; : (TOP)
p_top_  FDB     c_0                      ; 0
        FDB     c_rn_                    ; R#
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; SEEK-ERROR
; ==================================================================
l_seeks FDB     l_n_lag                  ; LFA -> #LAG

n_seeks FCB     $8A                      ; NFA -> 10
        FDB     $5345,$454B,$2D45,$5252  ; "SEEK-ERR"
        FCB     $4F,$D2                  ; "OR"

c_seeks FDB     do_col                   ; : SEEK-ERROR
p_seeks FDB     c_top_                   ; (TOP)
        FDB     c_finds                  ; FIND-BUF
        FDB     c_count                  ; COUNT
        FDB     c_type                   ; TYPE
        FDB     c_dotqt                  ; (.") len=4
        FCB     $04
        FCB     $4E,$6F,$6E,$65          ; "None"
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; (R)
; ==================================================================
l_r_    FDB     l_top_                   ; LFA -> (TOP)

n_r_    FCB     $83                      ; NFA -> 3
        FCB     $28,$52,$A9              ; "(R)"

c_r_    FDB     do_col                   ; : (R)
p_r_    FDB     c_to_l0                  ; >LINE#
        FDB     c_inser                  ; INSERT-BUF
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_submo                  ; -MOVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; P
; ==================================================================
l_p     FDB     l_r_                     ; LFA -> (R)

n_p     FCB     $81                      ; NFA -> 1
        FCB     $D0                      ; "P"

c_p     FDB     do_col                   ; : P
p_p     FDB     c_crs                    ; CRS
        FDB     c_text                   ; TEXT
        FDB     c_inser                  ; INSERT-BUF
        FDB     c_bufsu                  ; BUF-MOVE
        FDB     c_r_                     ; (R)
        FDB     c_semi_                  ; (;)

; ==================================================================
; WIPE
; ==================================================================
l_wipe  FDB     l_seeks                  ; LFA -> SEEK-ERROR

n_wipe  FCB     $84                      ; NFA -> 4
        FCB     $57,$49,$50,$C5          ; "WIPE"

c_wipe  FDB     do_col                   ; : WIPE
p_wipe  FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_block,c_lit_           ; BLOCK
        FDB     $0400                    ; 1024
        FDB     c_blank                  ; BLANK
        FDB     c_updat                  ; UPDATE
        FDB     c_semi_                  ; (;)

; ==================================================================
; 1LINE
; ==================================================================
l_1line FDB     l_inser                  ; LFA -> INSERT-BUF

n_1line FCB     $85                      ; NFA -> 5
        FCB     $31,$4C,$49,$4E,$C5      ; "1LINE"

c_1line FDB     do_col                   ; : 1LINE
p_1line FDB     c_n_lag                  ; #LAG
        FDB     c_finds                  ; FIND-BUF
        FDB     c_count                  ; COUNT
        FDB     c_matc0                  ; MATCH
        FDB     c_rn_                    ; R#
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; (SEEK)
; ==================================================================
l_seek_ FDB     l_p                      ; LFA -> P

n_seek_ FCB     $86                      ; NFA -> 6
        FCB     $28,$53,$45,$45,$4B,$A9  ; "(SEEK)"

c_seek_ FDB     do_col                   ; : (SEEK)
p_seek_ FDB     c_lit_
        FDB     $03FF                    ; 1023
        FDB     c_rn_                    ; R#
        FDB     c_at                     ; @
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z2ABD            ; ?BRANCH --Z2ABD--v
        FDB     c_seeks                  ; SEEK-ERROR
Z2ABD   FDB     c_1line                  ; 1LINE
        FDB     c_qmbra,p_seek_          ; ?BRANCH --p_seek_--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; (DELETE)
; ==================================================================
l_delet FDB     l_seek_                  ; LFA -> (SEEK)

n_delet FCB     $88                      ; NFA -> 8
        FDB     $2844,$454C,$4554,$45A9  ; "(DELETE)"

c_delet FDB     do_col                   ; : (DELETE)
p_delet FDB     c_to_r                   ; >R
        FDB     c_n_lag                  ; #LAG
        FDB     c_add                    ; +
        FDB     c_rat                    ; R@
        FDB     c_sub                    ; -
        FDB     c_n_lag                  ; #LAG
        FDB     c_rat                    ; R@
        FDB     c_negat                  ; NEGATE
        FDB     c_rn_                    ; R#
        FDB     c_addex                  ; +!
        FDB     c_n_lea                  ; #LEAD
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_from_                  ; R>
        FDB     c_blank                  ; BLANK
        FDB     c_updat                  ; UPDATE
        FDB     c_semi_                  ; (;)

; ==================================================================
; (F)
; ==================================================================
l_f_    FDB     l_delet                  ; LFA -> (DELETE)

n_f_    FCB     $83                      ; NFA -> 3
        FCB     $28,$46,$A9              ; "(F)"

c_f_    FDB     do_col                   ; : (F)
p_f_    FDB     c_crs                    ; CRS
        FDB     c_text                   ; TEXT
        FDB     c_finds                  ; FIND-BUF
        FDB     c_bufsu                  ; BUF-MOVE
        FDB     c_seek_                  ; (SEEK)
        FDB     c_semi_                  ; (;)

; ==================================================================
; F
; ==================================================================
l_f     FDB     l_b                      ; LFA -> B

n_f     FCB     $81                      ; NFA -> 1
        FCB     $C6                      ; "F"

c_f     FDB     do_col                   ; : F
p_f     FDB     c_f_                     ; (F)
        FDB     c_displ                  ; DISPLAY-CURSOR
        FDB     c_semi_                  ; (;)

; ==================================================================
; (E)
; ==================================================================
l_e_    FDB     l_f_                     ; LFA -> (F)

n_e_    FCB     $83                      ; NFA -> 3
        FCB     $28,$45,$A9              ; "(E)"

c_e_    FDB     do_col                   ; : (E)
p_e_    FDB     c_finds                  ; FIND-BUF
        FDB     c_cat                    ; C@
        FDB     c_delet                  ; (DELETE)
        FDB     c_semi_                  ; (;)

; ==================================================================
; E
; ==================================================================
l_e     FDB     l_1line                  ; LFA -> 1LINE

n_e     FCB     $81                      ; NFA -> 1
        FCB     $C5                      ; "E"

c_e     FDB     do_col                   ; : E
p_e     FDB     c_e_                     ; (E)
        FDB     c_displ                  ; DISPLAY-CURSOR
        FDB     c_semi_                  ; (;)

; ==================================================================
; D
; ==================================================================
l_d     FDB     l_e_                     ; LFA -> (E)

n_d     FCB     $81                      ; NFA -> 1
        FCB     $C4                      ; "D"

c_d     FDB     do_col                   ; : D
p_d     FDB     c_f_                     ; (F)
        FDB     c_e                      ; E
        FDB     c_semi_                  ; (;)

; ==================================================================
; TILL
; ==================================================================
l_till  FDB     l_d                      ; LFA -> D

n_till  FCB     $84                      ; NFA -> 4
        FCB     $54,$49,$4C,$CC          ; "TILL"

c_till  FDB     do_col                   ; : TILL
p_till  FDB     c_n_lea                  ; #LEAD
        FDB     c_add                    ; +
        FDB     c_crs                    ; CRS
        FDB     c_text                   ; TEXT
        FDB     c_finds                  ; FIND-BUF
        FDB     c_bufsu                  ; BUF-MOVE
        FDB     c_1line                  ; 1LINE
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z2B5D            ; ?BRANCH --Z2B5D--v
        FDB     c_seeks                  ; SEEK-ERROR
Z2B5D   FDB     c_n_lea                  ; #LEAD
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_sub                    ; -
        FDB     c_delet                  ; (DELETE)
        FDB     c_displ                  ; DISPLAY-CURSOR
        FDB     c_semi_                  ; (;)

; ==================================================================
; S
; ==================================================================
l_s     FDB     l_wipe                   ; LFA -> WIPE

n_s     FCB     $81                      ; NFA -> 1
        FCB     $D3                      ; "S"

c_s     FDB     do_col                   ; : S
p_s     FDB     c_crs                    ; CRS
        FDB     c_text                   ; TEXT
        FDB     c_finds                  ; FIND-BUF
        FDB     c_bufsu                  ; BUF-MOVE
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_do_,Z2BAF              ; (DO) --Z2BAF--v
Z2B85   FDB     c_i                      ; I
        FDB     c_scr                    ; SCR
        FDB     c_exc                    ; !
        FDB     c_top_                   ; (TOP)
Z2B8D   FDB     c_1line                  ; 1LINE
        FDB     c_qmbra,Z2B99            ; ?BRANCH --Z2B99--v
        FDB     c_displ                  ; DISPLAY-CURSOR
        FDB     c_scr                    ; SCR
        FDB     c_qm                     ; ?
Z2B99   FDB     c_qmkey                  ; ?KEY
        FDB     c_qmlea,c_lit_           ; (?LEAVE)
        FDB     $03FF                    ; 1023
        FDB     c_rn_                    ; R#
        FDB     c_at                     ; @
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z2B8D            ; ?BRANCH --Z2B8D--^
        FDB     c_loop_,Z2B85            ; (LOOP) --Z2B85--^
Z2BAF   FDB     c_from_                  ; R>
        FDB     c_scr                    ; SCR
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; I0
; ==================================================================
l_i0    FDB     l_e                      ; LFA -> E

n_i0    FCB     $81                      ; NFA -> 1
        FCB     $C9                      ; "I"

c_i0    FDB     do_col                   ; : I0
p_i0    FDB     c_crs                    ; CRS
        FDB     c_text                   ; TEXT
        FDB     c_inser                  ; INSERT-BUF
        FDB     c_bufsu                  ; BUF-MOVE
        FDB     c_inser                  ; INSERT-BUF
        FDB     c_count                  ; COUNT
        FDB     c_n_lag                  ; #LAG
        FDB     c_rot                    ; ROT
        FDB     c_over                   ; OVER
        FDB     c_min                    ; MIN
        FDB     c_to_r                   ; >R
        FDB     c_rat                    ; R@
        FDB     c_rn_                    ; R#
        FDB     c_addex                  ; +!
        FDB     c_rat                    ; R@
        FDB     c_sub                    ; -
        FDB     c_to_r                   ; >R
        FDB     c_dup                    ; DUP
        FDB     c_here                   ; HERE
        FDB     c_rat                    ; R@
        FDB     c_cmove                  ; CMOVE
        FDB     c_here                   ; HERE
        FDB     c_n_lea                  ; #LEAD
        FDB     c_add                    ; +
        FDB     c_from_                  ; R>
        FDB     c_cmove                  ; CMOVE
        FDB     c_from_                  ; R>
        FDB     c_cmove                  ; CMOVE
        FDB     c_updat                  ; UPDATE
        FDB     c_displ                  ; DISPLAY-CURSOR
        FDB     c_semi_                  ; (;)

; ==================================================================
; U
; ==================================================================
l_u     FDB     l_i0                     ; LFA -> I0

n_u     FCB     $81                      ; NFA -> 1
        FCB     $D5                      ; "U"

c_u     FDB     do_col                   ; : U
p_u     FDB     c_cdivl                  ; C/L
        FDB     c_rn_                    ; R#
        FDB     c_addex                  ; +!
        FDB     c_sprea                  ; (SPREAD)
        FDB     c_p                      ; P
        FDB     c_semi_                  ; (;)

; ==================================================================
; R
; ==================================================================
l_r     FDB     l_f                      ; LFA -> F

n_r     FCB     $81                      ; NFA -> 1
        FCB     $D2                      ; "R"

c_r     FDB     do_col                   ; : R
p_r     FDB     c_e_                     ; (E)
        FDB     c_i0                     ; I0
        FDB     c_semi_                  ; (;)

; ==================================================================
; M
; ==================================================================
l_m     FDB     l_u                      ; LFA -> U

n_m     FCB     $81                      ; NFA -> 1
        FCB     $CD                      ; "M"

c_m     FDB     do_col                   ; : M
p_m     FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_to_r                   ; >R
        FDB     c_rn_                    ; R#
        FDB     c_at                     ; @
        FDB     c_to_r                   ; >R
        FDB     c_to_l0                  ; >LINE#
        FDB     c_hold_                  ; (HOLD)
        FDB     c_swap                   ; SWAP
        FDB     c_scr                    ; SCR
        FDB     c_exc                    ; !
        FDB     c_1add                   ; 1+
        FDB     c_cdivl                  ; C/L
        FDB     c_ast                    ; *
        FDB     c_rn_                    ; R#
        FDB     c_exc                    ; !
        FDB     c_sprea                  ; (SPREAD)
        FDB     c_r_                     ; (R)
        FDB     c_from_                  ; R>
        FDB     c_cdivl                  ; C/L
        FDB     c_add                    ; +
        FDB     c_rn_                    ; R#
        FDB     c_exc                    ; !
        FDB     c_from_                  ; R>
        FDB     c_scr                    ; SCR
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; G
; ==================================================================
l_g     FDB     l_s                      ; LFA -> S

n_g     FCB     $81                      ; NFA -> 1
        FCB     $C7                      ; "G"

c_g     FDB     do_col                   ; : G
p_g     FDB     c_dup,c_lit_             ; DUP
        FDB     $FFF0                    ; -16
        FDB     c_and                    ; AND
        FDB     c_abort                  ; (ABORT") len=13
        FCB     $0D
        FDB     $496C,$6C65,$6761,$6C20  ; "Illegal "
        FCB     $6C,$69,$6E,$65,$23      ; "line#"
        FDB     c_cdivl                  ; C/L
        FDB     c_ast                    ; *
        FDB     c_swap                   ; SWAP
        FDB     c_block                  ; BLOCK
        FDB     c_add                    ; +
        FDB     c_inser                  ; INSERT-BUF
        FDB     c_1add                   ; 1+
        FDB     c_cdivl                  ; C/L
        FDB     c_cmove                  ; CMOVE
        FDB     c_cdivl                  ; C/L
        FDB     c_inser                  ; INSERT-BUF
        FDB     c_cexc                   ; C!
        FDB     c_sprea                  ; (SPREAD)
        FDB     c_r_                     ; (R)
        FDB     c_cdivl                  ; C/L
        FDB     c_rn_                    ; R#
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; BRING
; ==================================================================
l_bring FDB     l_r                      ; LFA -> R

n_bring FCB     $85                      ; NFA -> 5
        FCB     $42,$52,$49,$4E,$C7      ; "BRING"

c_bring FDB     do_col                   ; : BRING
p_bring FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z2CB3              ; (DO) --Z2CB3--v
Z2CA9   FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_g                      ; G
        FDB     c_loop_,Z2CA9            ; (LOOP) --Z2CA9--^
Z2CB3   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; (ENT)
; ==================================================================
l_ent_  FDB     l_till                   ; LFA -> TILL

n_ent_  FCB     $85                      ; NFA -> 5
        FCB     $28,$45,$4E,$54,$A9      ; "(ENT)"

c_ent_  FDB     do_col                   ; : (ENT)
p_ent_  FDB     c_pad                    ; PAD
        FDB     c_cdivl                  ; C/L
        FDB     c_1add                   ; 1+
        FDB     c_blank                  ; BLANK
        FDB     c_pad                    ; PAD
        FDB     c_1add                   ; 1+
        FDB     c_cdivl                  ; C/L
        FDB     c_expec                  ; EXPECT
        FDB     c_span                   ; SPAN
        FDB     c_at                     ; @
        FDB     c_pad                    ; PAD
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; (L)
; ==================================================================
l_l_    FDB     l_ent_                   ; LFA -> (ENT)

n_l_    FCB     $83                      ; NFA -> 3
        FCB     $28,$4C,$A9              ; "(L)"

c_l_    FDB     do_col                   ; : (L)
p_l_    FDB     c_lit_
        FDB     $000D                    ; 13
        FDB     c_emit                   ; EMIT
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_space                  ; SPACE
        FDB     c_line                   ; LINE
        FDB     c_cdivl                  ; C/L
        FDB     c_subtr                  ; -TRAILING
        FDB     c_to_ty                  ; >TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; NEW
; ==================================================================
l_new   FDB     l_bring                  ; LFA -> BRING

n_new   FCB     $83                      ; NFA -> 3
        FCB     $4E,$45,$D7              ; "NEW"

c_new   FDB     do_col                   ; : NEW
p_new   FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_0                      ; 0
        FDB     c_do_,Z2D4D              ; (DO) --Z2D4D--v
Z2D0F   FDB     c_cr                     ; CR
        FDB     c_i,c_lit_               ; I
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_space                  ; SPACE
        FDB     c_i                      ; I
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z2D45            ; ?BRANCH --Z2D45--v
        FDB     c_ent_                   ; (ENT)
        FDB     c_pad                    ; PAD
        FDB     c_cat                    ; C@
        FDB     c_qmbra,Z2D3D            ; ?BRANCH --Z2D3D--v
        FDB     c_pad                    ; PAD
        FDB     c_1add                   ; 1+
        FDB     c_i                      ; I
        FDB     c_submo                  ; -MOVE
        FDB     c_1add                   ; 1+
        FDB     c_branc,Z2D41            ; BRANCH --Z2D41--v
Z2D3D   FDB     c_i                      ; I
        FDB     c_l_                     ; (L)
Z2D41   FDB     c_branc,Z2D49            ; BRANCH --Z2D49--v
Z2D45   FDB     c_i                      ; I
        FDB     c_l_                     ; (L)
Z2D49   FDB     c_loop_,Z2D0F            ; (LOOP) --Z2D0F--^
Z2D4D   FDB     c_drop                   ; DROP
        FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; UNDER
; ==================================================================
l_under FDB     l_m                      ; LFA -> M

n_under FCB     $85                      ; NFA -> 5
        FCB     $55,$4E,$44,$45,$D2      ; "UNDER"

c_under FDB     do_col                   ; : UNDER
p_under FDB     c_1add,c_lit_            ; 1+
        FDB     $0010                    ; 16
        FDB     c_0                      ; 0
        FDB     c_do_,Z2DB3              ; (DO) --Z2DB3--v
Z2D69   FDB     c_cr                     ; CR
        FDB     c_i,c_lit_               ; I
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_space                  ; SPACE
        FDB     c_i                      ; I
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z2DAB            ; ?BRANCH --Z2DAB--v
        FDB     c_ent_                   ; (ENT)
        FDB     c_pad                    ; PAD
        FDB     c_cat                    ; C@
        FDB     c_qmbra,Z2DA3            ; ?BRANCH --Z2DA3--v
        FDB     c_i                      ; I
        FDB     c_cdivl                  ; C/L
        FDB     c_ast                    ; *
        FDB     c_rn_                    ; R#
        FDB     c_exc                    ; !
        FDB     c_sprea                  ; (SPREAD)
        FDB     c_pad                    ; PAD
        FDB     c_1add                   ; 1+
        FDB     c_i                      ; I
        FDB     c_submo                  ; -MOVE
        FDB     c_1add                   ; 1+
        FDB     c_branc,Z2DA7            ; BRANCH --Z2DA7--v
Z2DA3   FDB     c_i                      ; I
        FDB     c_l_                     ; (L)
Z2DA7   FDB     c_branc,Z2DAF            ; BRANCH --Z2DAF--v
Z2DAB   FDB     c_i                      ; I
        FDB     c_l_                     ; (L)
Z2DAF   FDB     c_loop_,Z2D69            ; (LOOP) --Z2D69--^
Z2DB3   FDB     c_drop                   ; DROP
        FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; ASSEMBLER
; ==================================================================
l_assem FDB     l_edito                  ; LFA -> EDITOR

n_assem FCB     $89                      ; NFA -> 9
        FDB     $4153,$5345,$4D42,$4C45  ; "ASSEMBLE"
        FCB     $D2                      ; "R"

c_assem FDB     do_voc
p_assem FDB     l_thenk,l_endsu,l_nextk,l_whil0,$2754

; ==================================================================
; %#
; ==================================================================
l_pern_ FDB     $0000

n_pern_ FCB     $82                      ; NFA -> 2
        FCB     $25,$A3                  ; "%#"

c_pern_ FDB     do_var                   ; VARIABLE %#
p_pern_ FDB     $0000                    ; 0

; ==================================================================
; %<<
; ==================================================================
l_perlt FDB     l_pern_                  ; LFA -> %#

n_perlt FCB     $83                      ; NFA -> 3
        FCB     $25,$3C,$BC              ; "%<<"

c_perlt FDB     do_var                   ; VARIABLE %<<
p_perlt FDB     $0000                    ; 0

; ==================================================================
; %I
; ==================================================================
l_peri  FDB     l_perlt                  ; LFA -> %<<

n_peri  FCB     $82                      ; NFA -> 2
        FCB     $25,$C9                  ; "%I"

c_peri  FDB     do_var                   ; VARIABLE %I
p_peri  FDB     $0000                    ; 0

; ==================================================================
; %P
; ==================================================================
l_perp  FDB     l_peri                   ; LFA -> %I

n_perp  FCB     $82                      ; NFA -> 2
        FCB     $25,$D0                  ; "%P"

c_perp  FDB     do_var                   ; VARIABLE %P
p_perp  FDB     $0000                    ; 0

; ==================================================================
; %T
; ==================================================================
l_pert  FDB     l_perp                   ; LFA -> %P

n_pert  FCB     $82                      ; NFA -> 2
        FCB     $25,$D4                  ; "%T"

c_pert  FDB     do_var                   ; VARIABLE %T
p_pert  FDB     $0000                    ; 0

; ==================================================================
; %B
; ==================================================================
l_perb  FDB     l_pert                   ; LFA -> %T

n_perb  FCB     $82                      ; NFA -> 2
        FCB     $25,$C2                  ; "%B"

c_perb  FDB     do_var                   ; VARIABLE %B
p_perb  FDB     $0000                    ; 0

; ==================================================================
; %INIT
; ==================================================================
l_perin FDB     l_perb                   ; LFA -> %B

n_perin FCB     $85                      ; NFA -> 5
        FCB     $25,$49,$4E,$49,$D4      ; "%INIT"

c_perin FDB     do_col                   ; : %INIT
p_perin FDB     c_0                      ; 0
        FDB     c_pern_                  ; %#
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_perlt                  ; %<<
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_peri                   ; %I
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_perp                   ; %P
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_pert                   ; %T
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_perb                   ; %B
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; %CHK
; ==================================================================
l_perch FDB     l_perin                  ; LFA -> %INIT

n_perch FCB     $84                      ; NFA -> 4
        FCB     $25,$43,$48,$CB          ; "%CHK"

c_perch FDB     do_col                   ; : %CHK
p_perch FDB     c_pern_                  ; %#
        FDB     c_at                     ; @
        FDB     c_perlt                  ; %<<
        FDB     c_at                     ; @
        FDB     c_or                     ; OR
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_or                     ; OR
        FDB     c_perp                   ; %P
        FDB     c_at                     ; @
        FDB     c_or                     ; OR
        FDB     c_pert                   ; %T
        FDB     c_at                     ; @
        FDB     c_or                     ; OR
        FDB     c_perb                   ; %B
        FDB     c_at                     ; @
        FDB     c_or                     ; OR
        FDB     c_perin                  ; %INIT
        FDB     c_abort                  ; (ABORT") len=18
        FCB     $12
        FDB     $4164,$6472,$6573,$732D  ; "Address-"
        FDB     $6D6F,$6465,$2065,$7272  ; "mode err"
        FCB     $6F,$72                  ; "or"
        FDB     c_semi_                  ; (;)

; ==================================================================
; #0
; ==================================================================
l_n_0   FDB     $0000

n_n_0   FCB     $81                      ; NFA -> 1
        FCB     $A3                      ; "#"

c_n_0   FDB     do_col                   ; : #0
p_n_0   FDB     c_1                      ; 1
        FDB     c_pern_                  ; %#
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; <<
; ==================================================================
l_from4 FDB     $0000

n_from4 FCB     $82                      ; NFA -> 2
        FCB     $3C,$BC                  ; "<<"

c_from4 FDB     do_col                   ; : <<
p_from4 FDB     c_1                      ; 1
        FDB     c_perlt                  ; %<<
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; %Z
; ==================================================================
l_perz  FDB     l_perch                  ; LFA -> %CHK

n_perz  FCB     $82                      ; NFA -> 2
        FCB     $25,$DA                  ; "%Z"

c_perz  FDB     do_col                   ; : %Z
p_perz  FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
Z2EA6   JMP     do_does
        FDB     c_cat                    ; C@
        FDB     c_ckom                   ; C,
        FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; NOP
; ==================================================================
l_nop   FDB     $0000

n_nop   FCB     $83                      ; NFA -> 3
        FCB     $4E,$4F,$D0              ; "NOP"

c_nop   FDB     Z2EA6
p_nop   FCB     $12

; ==================================================================
; SYNC
; ==================================================================
l_sync  FDB     l_n_0                    ; LFA -> #0

n_sync  FCB     $84                      ; NFA -> 4
        FCB     $53,$59,$4E,$C3          ; "SYNC"

c_sync  FDB     Z2EA6
p_sync  FCB     $13

; ==================================================================
; DAA
; ==================================================================
l_daa   FDB     l_from4                  ; LFA -> <<

n_daa   FCB     $83                      ; NFA -> 3
        FCB     $44,$41,$C1              ; "DAA"

c_daa   FDB     Z2EA6
p_daa   FCB     $19

; ==================================================================
; SEX
; ==================================================================
l_sex   FDB     l_sync                   ; LFA -> SYNC

n_sex   FCB     $83                      ; NFA -> 3
        FCB     $53,$45,$D8              ; "SEX"

c_sex   FDB     Z2EA6
p_sex   FCB     $1D

; ==================================================================
; NEGA
; ==================================================================
l_nega  FDB     l_nop                    ; LFA -> NOP

n_nega  FCB     $84                      ; NFA -> 4
        FCB     $4E,$45,$47,$C1          ; "NEGA"

c_nega  FDB     Z2EA6
p_nega  FCB     $40

; ==================================================================
; COMA
; ==================================================================
l_coma  FDB     l_sex                    ; LFA -> SEX

n_coma  FCB     $84                      ; NFA -> 4
        FCB     $43,$4F,$4D,$C1          ; "COMA"

c_coma  FDB     Z2EA6
p_coma  FCB     $43

; ==================================================================
; LSRA
; ==================================================================
l_lsra  FDB     l_daa                    ; LFA -> DAA

n_lsra  FCB     $84                      ; NFA -> 4
        FCB     $4C,$53,$52,$C1          ; "LSRA"

c_lsra  FDB     Z2EA6
p_lsra  FCB     $44

; ==================================================================
; RORA
; ==================================================================
l_rora  FDB     l_nega                   ; LFA -> NEGA

n_rora  FCB     $84                      ; NFA -> 4
        FCB     $52,$4F,$52,$C1          ; "RORA"

c_rora  FDB     Z2EA6
p_rora  FCB     $46

; ==================================================================
; ASRA
; ==================================================================
l_asra  FDB     l_perz                   ; LFA -> %Z

n_asra  FCB     $84                      ; NFA -> 4
        FCB     $41,$53,$52,$C1          ; "ASRA"

c_asra  FDB     Z2EA6
p_asra  FCB     $47

; ==================================================================
; ASLA
; ==================================================================
l_asla  FDB     l_asra                   ; LFA -> ASRA

n_asla  FCB     $84                      ; NFA -> 4
        FCB     $41,$53,$4C,$C1          ; "ASLA"

c_asla  FDB     Z2EA6
p_asla  FCB     $48

; ==================================================================
; ROLA
; ==================================================================
l_rola  FDB     l_rora                   ; LFA -> RORA

n_rola  FCB     $84                      ; NFA -> 4
        FCB     $52,$4F,$4C,$C1          ; "ROLA"

c_rola  FDB     Z2EA6
p_rola  FCB     $49

; ==================================================================
; DECA
; ==================================================================
l_deca  FDB     l_lsra                   ; LFA -> LSRA

n_deca  FCB     $84                      ; NFA -> 4
        FCB     $44,$45,$43,$C1          ; "DECA"

c_deca  FDB     Z2EA6
p_deca  FCB     $4A

; ==================================================================
; INCA
; ==================================================================
l_inca  FDB     l_asla                   ; LFA -> ASLA

n_inca  FCB     $84                      ; NFA -> 4
        FCB     $49,$4E,$43,$C1          ; "INCA"

c_inca  FDB     Z2EA6
p_inca  FCB     $4C

; ==================================================================
; TSTA
; ==================================================================
l_tsta  FDB     l_deca                   ; LFA -> DECA

n_tsta  FCB     $84                      ; NFA -> 4
        FCB     $54,$53,$54,$C1          ; "TSTA"

c_tsta  FDB     Z2EA6
p_tsta  FCB     $4D

; ==================================================================
; CLRA
; ==================================================================
l_clra  FDB     l_coma                   ; LFA -> COMA

n_clra  FCB     $84                      ; NFA -> 4
        FCB     $43,$4C,$52,$C1          ; "CLRA"

c_clra  FDB     Z2EA6
p_clra  FCB     $4F

; ==================================================================
; NEGB
; ==================================================================
l_negb  FDB     l_rola                   ; LFA -> ROLA

n_negb  FCB     $84                      ; NFA -> 4
        FCB     $4E,$45,$47,$C2          ; "NEGB"

c_negb  FDB     Z2EA6
p_negb  FCB     $50

; ==================================================================
; COMB
; ==================================================================
l_comb  FDB     l_clra                   ; LFA -> CLRA

n_comb  FCB     $84                      ; NFA -> 4
        FCB     $43,$4F,$4D,$C2          ; "COMB"

c_comb  FDB     Z2EA6
p_comb  FCB     $53

; ==================================================================
; LSRB
; ==================================================================
l_lsrb  FDB     l_tsta                   ; LFA -> TSTA

n_lsrb  FCB     $84                      ; NFA -> 4
        FCB     $4C,$53,$52,$C2          ; "LSRB"

c_lsrb  FDB     Z2EA6
p_lsrb  FCB     $54

; ==================================================================
; RORB
; ==================================================================
l_rorb  FDB     l_negb                   ; LFA -> NEGB

n_rorb  FCB     $84                      ; NFA -> 4
        FCB     $52,$4F,$52,$C2          ; "RORB"

c_rorb  FDB     Z2EA6
p_rorb  FCB     $56

; ==================================================================
; ASRB
; ==================================================================
l_asrb  FDB     l_inca                   ; LFA -> INCA

n_asrb  FCB     $84                      ; NFA -> 4
        FCB     $41,$53,$52,$C2          ; "ASRB"

c_asrb  FDB     Z2EA6
p_asrb  FCB     $57

; ==================================================================
; ASLB
; ==================================================================
l_aslb  FDB     l_asrb                   ; LFA -> ASRB

n_aslb  FCB     $84                      ; NFA -> 4
        FCB     $41,$53,$4C,$C2          ; "ASLB"

c_aslb  FDB     Z2EA6
p_aslb  FCB     $58

; ==================================================================
; ROLB
; ==================================================================
l_rolb  FDB     l_rorb                   ; LFA -> RORB

n_rolb  FCB     $84                      ; NFA -> 4
        FCB     $52,$4F,$4C,$C2          ; "ROLB"

c_rolb  FDB     Z2EA6
p_rolb  FCB     $59

; ==================================================================
; DECB
; ==================================================================
l_decb  FDB     l_lsrb                   ; LFA -> LSRB

n_decb  FCB     $84                      ; NFA -> 4
        FCB     $44,$45,$43,$C2          ; "DECB"

c_decb  FDB     Z2EA6
p_decb  FCB     $5A

; ==================================================================
; INCB
; ==================================================================
l_incb  FDB     l_aslb                   ; LFA -> ASLB

n_incb  FCB     $84                      ; NFA -> 4
        FCB     $49,$4E,$43,$C2          ; "INCB"

c_incb  FDB     Z2EA6
p_incb  FCB     $5C

; ==================================================================
; TSTB
; ==================================================================
l_tstb  FDB     l_decb                   ; LFA -> DECB

n_tstb  FCB     $84                      ; NFA -> 4
        FCB     $54,$53,$54,$C2          ; "TSTB"

c_tstb  FDB     Z2EA6
p_tstb  FCB     $5D

; ==================================================================
; CLRB
; ==================================================================
l_clrb  FDB     l_comb                   ; LFA -> COMB

n_clrb  FCB     $84                      ; NFA -> 4
        FCB     $43,$4C,$52,$C2          ; "CLRB"

c_clrb  FDB     Z2EA6
p_clrb  FCB     $5F

; ==================================================================
; RTS
; ==================================================================
l_rts   FDB     l_rolb                   ; LFA -> ROLB

n_rts   FCB     $83                      ; NFA -> 3
        FCB     $52,$54,$D3              ; "RTS"

c_rts   FDB     Z2EA6
p_rts   FCB     $39

; ==================================================================
; ABX
; ==================================================================
l_abx   FDB     l_incb                   ; LFA -> INCB

n_abx   FCB     $83                      ; NFA -> 3
        FCB     $41,$42,$D8              ; "ABX"

c_abx   FDB     Z2EA6
p_abx   FCB     $3A

; ==================================================================
; RTI
; ==================================================================
l_rti   FDB     l_rts                    ; LFA -> RTS

n_rti   FCB     $83                      ; NFA -> 3
        FCB     $52,$54,$C9              ; "RTI"

c_rti   FDB     Z2EA6
p_rti   FCB     $3B

; ==================================================================
; MUL
; ==================================================================
l_mul   FDB     l_abx                    ; LFA -> ABX

n_mul   FCB     $83                      ; NFA -> 3
        FCB     $4D,$55,$CC              ; "MUL"

c_mul   FDB     Z2EA6
p_mul   FCB     $3D

; ==================================================================
; SWI
; ==================================================================
l_swi   FDB     l_clrb                   ; LFA -> CLRB

n_swi   FCB     $83                      ; NFA -> 3
        FCB     $53,$57,$C9              ; "SWI"

c_swi   FDB     Z2EA6
p_swi   FCB     $3F

; ==================================================================
; SWI2
; ==================================================================
l_swi2  FDB     l_swi                    ; LFA -> SWI

n_swi2  FCB     $84                      ; NFA -> 4
        FCB     $53,$57,$49,$B2          ; "SWI2"

c_swi2  FDB     do_col                   ; : SWI2
p_swi2  FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_ckom                   ; C,
        FDB     c_swi                    ; SWI
        FDB     c_semi_                  ; (;)

; ==================================================================
; SWI3
; ==================================================================
l_swi3  FDB     l_swi2                   ; LFA -> SWI2

n_swi3  FCB     $84                      ; NFA -> 4
        FCB     $53,$57,$49,$B3          ; "SWI3"

c_swi3  FDB     do_col                   ; : SWI3
p_swi3  FDB     c_lit_
        FDB     $0011                    ; 17
        FDB     c_ckom                   ; C,
        FDB     c_swi                    ; SWI
        FDB     c_semi_                  ; (;)

; ==================================================================
; %X
; ==================================================================
l_perx  FDB     l_mul                    ; LFA -> MUL

n_perx  FCB     $82                      ; NFA -> 2
        FCB     $25,$D8                  ; "%X"

c_perx  FDB     do_col                   ; : %X
p_perx  FDB     c_creat                  ; CREATE
        FDB     c_kom                    ; ,
        FDB     c_semic                  ; (;CODE)
asm_idx JMP     do_does
        FDB     c_at                     ; @
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_or                     ; OR
        FDB     c_peri                   ; %I
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; []
; ==================================================================
l_lsqrs FDB     l_swi3                   ; LFA -> SWI3

n_lsqrs FCB     $82                      ; NFA -> 2
        FCB     $5B,$DD                  ; "[]"

c_lsqrs FDB     asm_idx
p_lsqrs FDB     c_next                   ; >NEXT

; ==================================================================
; ,PC
; ==================================================================
l_kompc FDB     l_tstb                   ; LFA -> TSTB

n_kompc FCB     $83                      ; NFA -> 3
        FCB     $2C,$50,$C3              ; ",PC"

c_kompc FDB     asm_idx
p_kompc FDB     $048C

; ==================================================================
; ,PCR
; ==================================================================
l_komp0 FDB     l_kompc                  ; LFA -> ,PC

n_komp0 FCB     $84                      ; NFA -> 4
        FCB     $2C,$50,$43,$D2          ; ",PCR"

c_komp0 FDB     asm_idx
p_komp0 FDB     $0C8C

; ==================================================================
; ,X
; ==================================================================
l_komx  FDB     l_komp0                  ; LFA -> ,PCR

n_komx  FCB     $82                      ; NFA -> 2
        FCB     $2C,$D8                  ; ",X"

c_komx  FDB     asm_idx
p_komx  FDB     l_rat

; ==================================================================
; ,Y
; ==================================================================
l_komy  FDB     l_komx                   ; LFA -> ,X

n_komy  FCB     $82                      ; NFA -> 2
        FCB     $2C,$D9                  ; ",Y"

c_komy  FDB     asm_idx
p_komy  FDB     c_dup                    ; DUP

; ==================================================================
; ,U
; ==================================================================
l_komu  FDB     l_komy                   ; LFA -> ,Y

n_komu  FCB     $82                      ; NFA -> 2
        FCB     $2C,$D5                  ; ",U"

c_komu  FDB     asm_idx
p_komu  FDB     $0140

; ==================================================================
; ,S
; ==================================================================
l_koms  FDB     l_komu                   ; LFA -> ,U

n_koms  FCB     $82                      ; NFA -> 2
        FCB     $2C,$D3                  ; ",S"

c_koms  FDB     asm_idx
p_koms  FDB     $0160

; ==================================================================
; ,X+
; ==================================================================
l_komxa FDB     l_koms                   ; LFA -> ,S

n_komxa FCB     $83                      ; NFA -> 3
        FCB     $2C,$58,$AB              ; ",X+"

c_komxa FDB     asm_idx
p_komxa FDB     $0280

; ==================================================================
; ,X++
; ==================================================================
l_komx0 FDB     l_komxa                  ; LFA -> ,X+

n_komx0 FCB     $84                      ; NFA -> 4
        FCB     $2C,$58,$2B,$AB          ; ",X++"

c_komx0 FDB     asm_idx
p_komx0 FDB     $0281

; ==================================================================
; ,-X
; ==================================================================
l_komsu FDB     l_komx0                  ; LFA -> ,X++

n_komsu FCB     $83                      ; NFA -> 3
        FCB     $2C,$2D,$D8              ; ",-X"

c_komsu FDB     asm_idx
p_komsu FDB     $0282

; ==================================================================
; ,--X
; ==================================================================
l_koms0 FDB     l_komsu                  ; LFA -> ,-X

n_koms0 FCB     $84                      ; NFA -> 4
        FCB     $2C,$2D,$2D,$D8          ; ",--X"

c_koms0 FDB     asm_idx
p_koms0 FDB     $0283

; ==================================================================
; ,Y+
; ==================================================================
l_komya FDB     l_koms0                  ; LFA -> ,--X

n_komya FCB     $83                      ; NFA -> 3
        FCB     $2C,$59,$AB              ; ",Y+"

c_komya FDB     asm_idx
p_komya FDB     $02A0

; ==================================================================
; ,Y++
; ==================================================================
l_komy0 FDB     l_komya                  ; LFA -> ,Y+

n_komy0 FCB     $84                      ; NFA -> 4
        FCB     $2C,$59,$2B,$AB          ; ",Y++"

c_komy0 FDB     asm_idx
p_komy0 FDB     $02A1

; ==================================================================
; ,-Y
; ==================================================================
l_koms1 FDB     l_komy0                  ; LFA -> ,Y++

n_koms1 FCB     $83                      ; NFA -> 3
        FCB     $2C,$2D,$D9              ; ",-Y"

c_koms1 FDB     asm_idx
p_koms1 FDB     $02A2

; ==================================================================
; ,--Y
; ==================================================================
l_koms2 FDB     l_koms1                  ; LFA -> ,-Y

n_koms2 FCB     $84                      ; NFA -> 4
        FCB     $2C,$2D,$2D,$D9          ; ",--Y"

c_koms2 FDB     asm_idx
p_koms2 FDB     $02A3

; ==================================================================
; ,U+
; ==================================================================
l_komua FDB     l_koms2                  ; LFA -> ,--Y

n_komua FCB     $83                      ; NFA -> 3
        FCB     $2C,$55,$AB              ; ",U+"

c_komua FDB     asm_idx
p_komua FDB     $02C0

; ==================================================================
; ,U++
; ==================================================================
l_komu0 FDB     l_komua                  ; LFA -> ,U+

n_komu0 FCB     $84                      ; NFA -> 4
        FCB     $2C,$55,$2B,$AB          ; ",U++"

c_komu0 FDB     asm_idx
p_komu0 FDB     p_and

; ==================================================================
; ,-U
; ==================================================================
l_koms3 FDB     l_komu0                  ; LFA -> ,U++

n_koms3 FCB     $83                      ; NFA -> 3
        FCB     $2C,$2D,$D5              ; ",-U"

c_koms3 FDB     asm_idx
p_koms3 FDB     $02C2

; ==================================================================
; ,--U
; ==================================================================
l_koms4 FDB     l_koms3                  ; LFA -> ,-U

n_koms4 FCB     $84                      ; NFA -> 4
        FCB     $2C,$2D,$2D,$D5          ; ",--U"

c_koms4 FDB     asm_idx
p_koms4 FDB     $02C3

; ==================================================================
; ,S+
; ==================================================================
l_komsa FDB     l_koms4                  ; LFA -> ,--U

n_komsa FCB     $83                      ; NFA -> 3
        FCB     $2C,$53,$AB              ; ",S+"

c_komsa FDB     asm_idx
p_komsa FDB     $02E0

; ==================================================================
; ,S++
; ==================================================================
l_koms5 FDB     l_komsa                  ; LFA -> ,S+

n_koms5 FCB     $84                      ; NFA -> 4
        FCB     $2C,$53,$2B,$AB          ; ",S++"

c_koms5 FDB     asm_idx
p_koms5 FDB     $02E1

; ==================================================================
; ,-S
; ==================================================================
l_koms6 FDB     l_koms5                  ; LFA -> ,S++

n_koms6 FCB     $83                      ; NFA -> 3
        FCB     $2C,$2D,$D3              ; ",-S"

c_koms6 FDB     asm_idx
p_koms6 FDB     c_xor                    ; XOR

; ==================================================================
; ,--S
; ==================================================================
l_koms7 FDB     l_koms6                  ; LFA -> ,-S

n_koms7 FCB     $84                      ; NFA -> 4
        FCB     $2C,$2D,$2D,$D3          ; ",--S"

c_koms7 FDB     asm_idx
p_koms7 FDB     $02E3

; ==================================================================
; A,X
; ==================================================================
l_akomx FDB     l_perx                   ; LFA -> %X

n_akomx FCB     $83                      ; NFA -> 3
        FCB     $41,$2C,$D8              ; "A,X"

c_akomx FDB     asm_idx
p_akomx FDB     $0286

; ==================================================================
; B,X
; ==================================================================
l_bkomx FDB     l_rti                    ; LFA -> RTI

n_bkomx FCB     $83                      ; NFA -> 3
        FCB     $42,$2C,$D8              ; "B,X"

c_bkomx FDB     asm_idx
p_bkomx FDB     $0285

; ==================================================================
; D,X
; ==================================================================
l_dkomx FDB     l_koms7                  ; LFA -> ,--S

n_dkomx FCB     $83                      ; NFA -> 3
        FCB     $44,$2C,$D8              ; "D,X"

c_dkomx FDB     asm_idx
p_dkomx FDB     $028B

; ==================================================================
; A,Y
; ==================================================================
l_akomy FDB     l_akomx                  ; LFA -> A,X

n_akomy FCB     $83                      ; NFA -> 3
        FCB     $41,$2C,$D9              ; "A,Y"

c_akomy FDB     asm_idx
p_akomy FDB     $02A6

; ==================================================================
; B,Y
; ==================================================================
l_bkomy FDB     l_bkomx                  ; LFA -> B,X

n_bkomy FCB     $83                      ; NFA -> 3
        FCB     $42,$2C,$D9              ; "B,Y"

c_bkomy FDB     asm_idx
p_bkomy FDB     $02A5

; ==================================================================
; D,Y
; ==================================================================
l_dkomy FDB     l_dkomx                  ; LFA -> D,X

n_dkomy FCB     $83                      ; NFA -> 3
        FCB     $44,$2C,$D9              ; "D,Y"

c_dkomy FDB     asm_idx
p_dkomy FDB     Z02AB

; ==================================================================
; A,U
; ==================================================================
l_akomu FDB     l_akomy                  ; LFA -> A,Y

n_akomu FCB     $83                      ; NFA -> 3
        FCB     $41,$2C,$D5              ; "A,U"

c_akomu FDB     asm_idx
p_akomu FDB     $02C6

; ==================================================================
; B,U
; ==================================================================
l_bkomu FDB     l_bkomy                  ; LFA -> B,Y

n_bkomu FCB     $83                      ; NFA -> 3
        FCB     $42,$2C,$D5              ; "B,U"

c_bkomu FDB     asm_idx
p_bkomu FDB     $02C5

; ==================================================================
; D,U
; ==================================================================
l_dkomu FDB     l_dkomy                  ; LFA -> D,Y

n_dkomu FCB     $83                      ; NFA -> 3
        FCB     $44,$2C,$D5              ; "D,U"

c_dkomu FDB     asm_idx
p_dkomu FDB     l_or

; ==================================================================
; A,S
; ==================================================================
l_akoms FDB     l_akomu                  ; LFA -> A,U

n_akoms FCB     $83                      ; NFA -> 3
        FCB     $41,$2C,$D3              ; "A,S"

c_akoms FDB     asm_idx
p_akoms FDB     $02E6

; ==================================================================
; B,S
; ==================================================================
l_bkoms FDB     l_bkomu                  ; LFA -> B,U

n_bkoms FCB     $83                      ; NFA -> 3
        FCB     $42,$2C,$D3              ; "B,S"

c_bkoms FDB     asm_idx
p_bkoms FDB     $02E5

; ==================================================================
; D,S
; ==================================================================
l_dkoms FDB     l_dkomu                  ; LFA -> D,U

n_dkoms FCB     $83                      ; NFA -> 3
        FCB     $44,$2C,$D3              ; "D,S"

c_dkoms FDB     asm_idx
p_dkoms FDB     $02EB

; ==================================================================
; %R
; ==================================================================
l_perr  FDB     l_akoms                  ; LFA -> A,S

n_perr  FCB     $82                      ; NFA -> 2
        FCB     $25,$D2                  ; "%R"

c_perr  FDB     do_col                   ; : %R
p_perr  FDB     c_qmdup                  ; ?DUP
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z31A2            ; ?BRANCH --Z31A2--v
        FDB     c_peri                   ; %I
        FDB     c_at,c_lit_              ; @
        FDB     $0084                    ; 132
        FDB     c_or                     ; OR
        FDB     c_ckom                   ; C,
        FDB     c_exit                   ; EXIT
Z31A2   FDB     c_dup                    ; DUP
        FDB     c_abs,c_lit_             ; ABS
        FDB     $FF80                    ; -128
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z31C0            ; ?BRANCH --Z31C0--v
        FDB     c_peri                   ; %I
        FDB     c_at,c_lit_              ; @
        FDB     $0089                    ; 137
        FDB     c_or                     ; OR
        FDB     c_ckom                   ; C,
        FDB     c_kom                    ; ,
        FDB     c_exit                   ; EXIT
Z31C0   FDB     c_dup                    ; DUP
        FDB     c_abs,c_lit_             ; ABS
        FDB     $FFF0                    ; -16
        FDB     c_and                    ; AND
        FDB     c_peri                   ; %I
        FDB     c_at,c_lit_              ; @
        FDB     $0010                    ; 16
        FDB     c_and                    ; AND
        FDB     c_or                     ; OR
        FDB     c_qmbra,Z31EA            ; ?BRANCH --Z31EA--v
        FDB     c_peri                   ; %I
        FDB     c_at,c_lit_              ; @
        FDB     $0088                    ; 136
        FDB     c_or                     ; OR
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
        FDB     c_exit                   ; EXIT
Z31EA   FDB     c_lit_
        FDB     $001F                    ; 31
        FDB     c_and                    ; AND
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_or                     ; OR
        FDB     c_ckom                   ; C,
        FDB     c_semi_                  ; (;)

; ==================================================================
; %PC
; ==================================================================
l_perpc FDB     l_perr                   ; LFA -> %R

n_perpc FCB     $83                      ; NFA -> 3
        FCB     $25,$50,$C3              ; "%PC"

c_perpc FDB     do_col                   ; : %PC
p_perpc FDB     c_lit_
        FDB     $0800                    ; 2048
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z3216            ; ?BRANCH --Z3216--v
        FDB     c_here                   ; HERE
        FDB     c_2add                   ; 2+
        FDB     c_sub                    ; -
Z3216   FDB     c_dup                    ; DUP
        FDB     c_abs,c_lit_             ; ABS
        FDB     $FF80                    ; -128
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z3244,c_lit_     ; ?BRANCH --Z3244--v
        FDB     $0800                    ; 2048
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z3236            ; ?BRANCH --Z3236--v
        FDB     c_1                      ; 1
        FDB     c_sub                    ; -
Z3236   FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_1add                   ; 1+
        FDB     c_ckom                   ; C,
        FDB     c_kom                    ; ,
        FDB     c_branc,Z324C            ; BRANCH --Z324C--v
Z3244   FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
Z324C   FDB     c_semi_                  ; (;)

; ==================================================================
; %IX
; ==================================================================
l_perix FDB     l_perpc                  ; LFA -> %PC

n_perix FCB     $83                      ; NFA -> 3
        FCB     $25,$49,$D8              ; "%IX"

c_perix FDB     do_col                   ; : %IX
p_perix FDB     c_lit_
        FDB     $0100                    ; 256
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z326A            ; ?BRANCH --Z326A--v
        FDB     c_perr                   ; %R
        FDB     c_branc,Z329E            ; BRANCH --Z329E--v
Z326A   FDB     c_lit_
        FDB     $0200                    ; 512
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z3282            ; ?BRANCH --Z3282--v
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_ckom                   ; C,
        FDB     c_branc,Z329E            ; BRANCH --Z329E--v
Z3282   FDB     c_lit_
        FDB     $0400                    ; 1024
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z3296            ; ?BRANCH --Z3296--v
        FDB     c_perpc                  ; %PC
        FDB     c_branc,Z329E            ; BRANCH --Z329E--v
Z3296   FDB     c_lit_
        FDB     $009F                    ; 159
        FDB     c_ckom                   ; C,
        FDB     c_kom                    ; ,
Z329E   FDB     c_0                      ; 0
        FDB     c_peri                   ; %I
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; %1
; ==================================================================
l_per1  FDB     l_perix                  ; LFA -> %IX

n_per1  FCB     $82                      ; NFA -> 2
        FCB     $25,$B1                  ; "%1"

c_per1  FDB     do_col                   ; : %1
p_per1  FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_dp  JMP     do_does
        FDB     c_cat                    ; C@
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_qmbra,Z32CE,c_lit_     ; ?BRANCH --Z32CE--v
        FDB     $0060                    ; 96
        FDB     c_add                    ; +
        FDB     c_ckom                   ; C,
        FDB     c_perix                  ; %IX
        FDB     c_branc,Z32EE            ; BRANCH --Z32EE--v
Z32CE   FDB     c_perlt                  ; %<<
        FDB     c_at                     ; @
        FDB     c_qmbra,Z32E4            ; ?BRANCH --Z32E4--v
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
        FDB     c_0                      ; 0
        FDB     c_perlt                  ; %<<
        FDB     c_exc                    ; !
        FDB     c_branc,Z32EE            ; BRANCH --Z32EE--v
Z32E4   FDB     c_lit_
        FDB     $0070                    ; 112
        FDB     c_add                    ; +
        FDB     c_ckom                   ; C,
        FDB     c_kom                    ; ,
Z32EE   FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; NEG
; ==================================================================
l_neg   FDB     l_bkoms                  ; LFA -> B,S

n_neg   FCB     $83                      ; NFA -> 3
        FCB     $4E,$45,$C7              ; "NEG"

c_neg   FDB     asm_dp
p_neg   FCB     $00

; ==================================================================
; COM
; ==================================================================
l_com   FDB     l_lsqrs                  ; LFA -> []

n_com   FCB     $83                      ; NFA -> 3
        FCB     $43,$4F,$CD              ; "COM"

c_com   FDB     asm_dp
p_com   FCB     $03

; ==================================================================
; LSR
; ==================================================================
l_lsr   FDB     l_dkoms                  ; LFA -> D,S

n_lsr   FCB     $83                      ; NFA -> 3
        FCB     $4C,$53,$D2              ; "LSR"

c_lsr   FDB     asm_dp
p_lsr   FCB     $04

; ==================================================================
; ROR
; ==================================================================
l_ror   FDB     l_neg                    ; LFA -> NEG

n_ror   FCB     $83                      ; NFA -> 3
        FCB     $52,$4F,$D2              ; "ROR"

c_ror   FDB     asm_dp
p_ror   FCB     $06

; ==================================================================
; ASR
; ==================================================================
l_asr   FDB     l_per1                   ; LFA -> %1

n_asr   FCB     $83                      ; NFA -> 3
        FCB     $41,$53,$D2              ; "ASR"

c_asr   FDB     asm_dp
p_asr   FCB     $07

; ==================================================================
; ASL
; ==================================================================
l_asl   FDB     l_asr                    ; LFA -> ASR

n_asl   FCB     $83                      ; NFA -> 3
        FCB     $41,$53,$CC              ; "ASL"

c_asl   FDB     asm_dp
p_asl   FCB     $08

; ==================================================================
; ROL
; ==================================================================
l_rol   FDB     l_ror                    ; LFA -> ROR

n_rol   FCB     $83                      ; NFA -> 3
        FCB     $52,$4F,$CC              ; "ROL"

c_rol   FDB     asm_dp
p_rol   FCB     $09

; ==================================================================
; DEC
; ==================================================================
l_dec   FDB     l_lsr                    ; LFA -> LSR

n_dec   FCB     $83                      ; NFA -> 3
        FCB     $44,$45,$C3              ; "DEC"

c_dec   FDB     asm_dp
p_dec   FCB     $0A

; ==================================================================
; INC
; ==================================================================
l_inc   FDB     l_asl                    ; LFA -> ASL

n_inc   FCB     $83                      ; NFA -> 3
        FCB     $49,$4E,$C3              ; "INC"

c_inc   FDB     asm_dp
p_inc   FCB     $0C

; ==================================================================
; TST
; ==================================================================
l_tst   FDB     l_dec                    ; LFA -> DEC

n_tst   FCB     $83                      ; NFA -> 3
        FCB     $54,$53,$D4              ; "TST"

c_tst   FDB     asm_dp
p_tst   FCB     $0D

; ==================================================================
; JMP
; ==================================================================
l_jmp   FDB     l_rol                    ; LFA -> ROL

n_jmp   FCB     $83                      ; NFA -> 3
        FCB     $4A,$4D,$D0              ; "JMP"

c_jmp   FDB     asm_dp
p_jmp   FCB     $0E

; ==================================================================
; CLR
; ==================================================================
l_clr   FDB     l_com                    ; LFA -> COM

n_clr   FCB     $83                      ; NFA -> 3
        FCB     $43,$4C,$D2              ; "CLR"

c_clr   FDB     asm_dp
p_clr   FCB     $0F

; ==================================================================
; %23
; ==================================================================
l_per23 FDB     l_inc                    ; LFA -> INC

n_per23 FCB     $83                      ; NFA -> 3
        FCB     $25,$32,$B3              ; "%23"

c_per23 FDB     do_col                   ; : %23
p_per23 FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_qmbra,Z337A,c_lit_     ; ?BRANCH --Z337A--v
        FDB     $0020                    ; 32
        FDB     c_add                    ; +
        FDB     c_ckom                   ; C,
        FDB     c_perix                  ; %IX
        FDB     c_exit                   ; EXIT
Z337A   FDB     c_perlt                  ; %<<
        FDB     c_at                     ; @
        FDB     c_qmbra,Z3394,c_lit_     ; ?BRANCH --Z3394--v
        FDB     $0010                    ; 16
        FDB     c_add                    ; +
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
        FDB     c_0                      ; 0
        FDB     c_perlt                  ; %<<
        FDB     c_exc                    ; !
        FDB     c_exit                   ; EXIT
Z3394   FDB     c_lit_
        FDB     $0030                    ; ' EXIT
        FDB     c_add                    ; +
        FDB     c_ckom                   ; C,
        FDB     c_kom                    ; ,
        FDB     c_semi_                  ; (;)

; ==================================================================
; %2
; ==================================================================
l_per2  FDB     l_per23                  ; LFA -> %23

n_per2  FCB     $82                      ; NFA -> 2
        FCB     $25,$B2                  ; "%2"

c_per2  FDB     do_col                   ; : %2
p_per2  FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_2   JMP     do_does
        FDB     c_cat                    ; C@
        FDB     c_pern_                  ; %#
        FDB     c_at                     ; @
        FDB     c_qmbra,Z33C8            ; ?BRANCH --Z33C8--v
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
        FDB     c_0                      ; 0
        FDB     c_pern_                  ; %#
        FDB     c_exc                    ; !
        FDB     c_branc,Z33CA            ; BRANCH --Z33CA--v
Z33C8   FDB     c_per23                  ; %23
Z33CA   FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; %3
; ==================================================================
l_per3  FDB     l_per2                   ; LFA -> %2

n_per3  FCB     $82                      ; NFA -> 2
        FCB     $25,$B3                  ; "%3"

c_per3  FDB     do_col                   ; : %3
p_per3  FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_3   JMP     do_does
        FDB     c_cat                    ; C@
        FDB     c_pern_                  ; %#
        FDB     c_at                     ; @
        FDB     c_qmbra,Z33F6            ; ?BRANCH --Z33F6--v
        FDB     c_ckom                   ; C,
        FDB     c_kom                    ; ,
        FDB     c_0                      ; 0
        FDB     c_pern_                  ; %#
        FDB     c_exc                    ; !
        FDB     c_branc,Z33F8            ; BRANCH --Z33F8--v
Z33F6   FDB     c_per23                  ; %23
Z33F8   FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; SUBA
; ==================================================================
l_suba  FDB     l_clr                    ; LFA -> CLR

n_suba  FCB     $84                      ; NFA -> 4
        FCB     $53,$55,$42,$C1          ; "SUBA"

c_suba  FDB     asm_2
p_suba  FCB     $80

; ==================================================================
; CMPA
; ==================================================================
l_cmpa  FDB     l_suba                   ; LFA -> SUBA

n_cmpa  FCB     $84                      ; NFA -> 4
        FCB     $43,$4D,$50,$C1          ; "CMPA"

c_cmpa  FDB     asm_2
p_cmpa  FCB     $81

; ==================================================================
; SBCA
; ==================================================================
l_sbca  FDB     l_cmpa                   ; LFA -> CMPA

n_sbca  FCB     $84                      ; NFA -> 4
        FCB     $53,$42,$43,$C1          ; "SBCA"

c_sbca  FDB     asm_2
p_sbca  FCB     $82

; ==================================================================
; SUBD
; ==================================================================
l_subd  FDB     l_sbca                   ; LFA -> SBCA

n_subd  FCB     $84                      ; NFA -> 4
        FCB     $53,$55,$42,$C4          ; "SUBD"

c_subd  FDB     asm_3
p_subd  FCB     $83

; ==================================================================
; ANDA
; ==================================================================
l_anda  FDB     l_per3                   ; LFA -> %3

n_anda  FCB     $84                      ; NFA -> 4
        FCB     $41,$4E,$44,$C1          ; "ANDA"

c_anda  FDB     asm_2
p_anda  FCB     $84

; ==================================================================
; BITA
; ==================================================================
l_bita  FDB     l_jmp                    ; LFA -> JMP

n_bita  FCB     $84                      ; NFA -> 4
        FCB     $42,$49,$54,$C1          ; "BITA"

c_bita  FDB     asm_2
p_bita  FCB     $85

; ==================================================================
; LDA
; ==================================================================
l_lda   FDB     l_tst                    ; LFA -> TST

n_lda   FCB     $83                      ; NFA -> 3
        FCB     $4C,$44,$C1              ; "LDA"

c_lda   FDB     asm_2
p_lda   FCB     $86

; ==================================================================
; EORA
; ==================================================================
l_eora  FDB     l_anda                   ; LFA -> ANDA

n_eora  FCB     $84                      ; NFA -> 4
        FCB     $45,$4F,$52,$C1          ; "EORA"

c_eora  FDB     asm_2
p_eora  FCB     $88

; ==================================================================
; ADCA
; ==================================================================
l_adca  FDB     l_eora                   ; LFA -> EORA

n_adca  FCB     $84                      ; NFA -> 4
        FCB     $41,$44,$43,$C1          ; "ADCA"

c_adca  FDB     asm_2
p_adca  FCB     $89

; ==================================================================
; ORA
; ==================================================================
l_ora   FDB     l_subd                   ; LFA -> SUBD

n_ora   FCB     $83                      ; NFA -> 3
        FCB     $4F,$52,$C1              ; "ORA"

c_ora   FDB     asm_2
p_ora   FCB     $8A

; ==================================================================
; ADDA
; ==================================================================
l_adda  FDB     l_adca                   ; LFA -> ADCA

n_adda  FCB     $84                      ; NFA -> 4
        FCB     $41,$44,$44,$C1          ; "ADDA"

c_adda  FDB     asm_2
p_adda  FCB     $8B

; ==================================================================
; CMPX
; ==================================================================
l_cmpx  FDB     l_ora                    ; LFA -> ORA

n_cmpx  FCB     $84                      ; NFA -> 4
        FCB     $43,$4D,$50,$D8          ; "CMPX"

c_cmpx  FDB     asm_3
p_cmpx  FCB     $8C

; ==================================================================
; LDX
; ==================================================================
l_ldx   FDB     l_lda                    ; LFA -> LDA

n_ldx   FCB     $83                      ; NFA -> 3
        FCB     $4C,$44,$D8              ; "LDX"

c_ldx   FDB     asm_3
p_ldx   FCB     $8E

; ==================================================================
; SUBB
; ==================================================================
l_subb  FDB     l_cmpx                   ; LFA -> CMPX

n_subb  FCB     $84                      ; NFA -> 4
        FCB     $53,$55,$42,$C2          ; "SUBB"

c_subb  FDB     asm_2
p_subb  FCB     $C0

; ==================================================================
; CMPB
; ==================================================================
l_cmpb  FDB     l_subb                   ; LFA -> SUBB

n_cmpb  FCB     $84                      ; NFA -> 4
        FCB     $43,$4D,$50,$C2          ; "CMPB"

c_cmpb  FDB     asm_2
p_cmpb  FCB     $C1

; ==================================================================
; SBCB
; ==================================================================
l_sbcb  FDB     l_cmpb                   ; LFA -> CMPB

n_sbcb  FCB     $84                      ; NFA -> 4
        FCB     $53,$42,$43,$C2          ; "SBCB"

c_sbcb  FDB     asm_2
p_sbcb  FCB     $C2

; ==================================================================
; ADDD
; ==================================================================
l_addd  FDB     l_adda                   ; LFA -> ADDA

n_addd  FCB     $84                      ; NFA -> 4
        FCB     $41,$44,$44,$C4          ; "ADDD"

c_addd  FDB     asm_3
p_addd  FCB     $C3

; ==================================================================
; ANDB
; ==================================================================
l_andb  FDB     l_addd                   ; LFA -> ADDD

n_andb  FCB     $84                      ; NFA -> 4
        FCB     $41,$4E,$44,$C2          ; "ANDB"

c_andb  FDB     asm_2
p_andb  FCB     $C4

; ==================================================================
; BITB
; ==================================================================
l_bitb  FDB     l_bita                   ; LFA -> BITA

n_bitb  FCB     $84                      ; NFA -> 4
        FCB     $42,$49,$54,$C2          ; "BITB"

c_bitb  FDB     asm_2
p_bitb  FCB     $C5

; ==================================================================
; LDB
; ==================================================================
l_ldb   FDB     l_ldx                    ; LFA -> LDX

n_ldb   FCB     $83                      ; NFA -> 3
        FCB     $4C,$44,$C2              ; "LDB"

c_ldb   FDB     asm_2
p_ldb   FCB     $C6

; ==================================================================
; EORB
; ==================================================================
l_eorb  FDB     l_andb                   ; LFA -> ANDB

n_eorb  FCB     $84                      ; NFA -> 4
        FCB     $45,$4F,$52,$C2          ; "EORB"

c_eorb  FDB     asm_2
p_eorb  FCB     $C8

; ==================================================================
; ADCB
; ==================================================================
l_adcb  FDB     l_eorb                   ; LFA -> EORB

n_adcb  FCB     $84                      ; NFA -> 4
        FCB     $41,$44,$43,$C2          ; "ADCB"

c_adcb  FDB     asm_2
p_adcb  FCB     $C9

; ==================================================================
; ORB
; ==================================================================
l_orb   FDB     l_sbcb                   ; LFA -> SBCB

n_orb   FCB     $83                      ; NFA -> 3
        FCB     $4F,$52,$C2              ; "ORB"

c_orb   FDB     asm_2
p_orb   FCB     $CA

; ==================================================================
; ADDB
; ==================================================================
l_addb  FDB     l_adcb                   ; LFA -> ADCB

n_addb  FCB     $84                      ; NFA -> 4
        FCB     $41,$44,$44,$C2          ; "ADDB"

c_addb  FDB     asm_2
p_addb  FCB     $CB

; ==================================================================
; LDD
; ==================================================================
l_ldd   FDB     l_ldb                    ; LFA -> LDB

n_ldd   FCB     $83                      ; NFA -> 3
        FCB     $4C,$44,$C4              ; "LDD"

c_ldd   FDB     asm_3
p_ldd   FCB     $CC

; ==================================================================
; LDU
; ==================================================================
l_ldu   FDB     l_ldd                    ; LFA -> LDD

n_ldu   FCB     $83                      ; NFA -> 3
        FCB     $4C,$44,$D5              ; "LDU"

c_ldu   FDB     asm_3
p_ldu   FCB     $CE

; ==================================================================
; LDY
; ==================================================================
l_ldy   FDB     l_ldu                    ; LFA -> LDU

n_ldy   FCB     $83                      ; NFA -> 3
        FCB     $4C,$44,$D9              ; "LDY"

c_ldy   FDB     do_col                   ; : LDY
p_ldy   FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_ckom                   ; C,
        FDB     c_ldx                    ; LDX
        FDB     c_semi_                  ; (;)

; ==================================================================
; LDS
; ==================================================================
l_lds   FDB     l_ldy                    ; LFA -> LDY

n_lds   FCB     $83                      ; NFA -> 3
        FCB     $4C,$44,$D3              ; "LDS"

c_lds   FDB     do_col                   ; : LDS
p_lds   FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_ckom                   ; C,
        FDB     c_ldu                    ; LDU
        FDB     c_semi_                  ; (;)

; ==================================================================
; CMPY
; ==================================================================
l_cmpy  FDB     l_orb                    ; LFA -> ORB

n_cmpy  FCB     $84                      ; NFA -> 4
        FCB     $43,$4D,$50,$D9          ; "CMPY"

c_cmpy  FDB     do_col                   ; : CMPY
p_cmpy  FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_ckom                   ; C,
        FDB     c_cmpx                   ; CMPX
        FDB     c_semi_                  ; (;)

; ==================================================================
; CMPD
; ==================================================================
l_cmpd  FDB     l_cmpy                   ; LFA -> CMPY

n_cmpd  FCB     $84                      ; NFA -> 4
        FCB     $43,$4D,$50,$C4          ; "CMPD"

c_cmpd  FDB     do_col                   ; : CMPD
p_cmpd  FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_ckom                   ; C,
        FDB     c_subd                   ; SUBD
        FDB     c_semi_                  ; (;)

; ==================================================================
; CMPS
; ==================================================================
l_cmps  FDB     l_cmpd                   ; LFA -> CMPD

n_cmps  FCB     $84                      ; NFA -> 4
        FCB     $43,$4D,$50,$D3          ; "CMPS"

c_cmps  FDB     do_col                   ; : CMPS
p_cmps  FDB     c_lit_
        FDB     $0011                    ; 17
        FDB     c_ckom                   ; C,
        FDB     c_cmpx                   ; CMPX
        FDB     c_semi_                  ; (;)

; ==================================================================
; CMPU
; ==================================================================
l_cmpu  FDB     l_cmps                   ; LFA -> CMPS

n_cmpu  FCB     $84                      ; NFA -> 4
        FCB     $43,$4D,$50,$D5          ; "CMPU"

c_cmpu  FDB     do_col                   ; : CMPU
p_cmpu  FDB     c_lit_
        FDB     $0011                    ; 17
        FDB     c_ckom                   ; C,
        FDB     c_subd                   ; SUBD
        FDB     c_semi_                  ; (;)

; ==================================================================
; %S
; ==================================================================
l_pers  FDB     l_addb                   ; LFA -> ADDB

n_pers  FCB     $82                      ; NFA -> 2
        FCB     $25,$D3                  ; "%S"

c_pers  FDB     do_col                   ; : %S
p_pers  FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_23  JMP     do_does
        FDB     c_pern_                  ; %#
        FDB     c_at                     ; @
        FDB     c_qmbra,Z3583            ; ?BRANCH --Z3583--v
        FDB     c_perch                  ; %CHK
Z3583   FDB     c_cat                    ; C@
        FDB     c_per23                  ; %23
        FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; STA
; ==================================================================
l_sta   FDB     l_cmpu                   ; LFA -> CMPU

n_sta   FCB     $83                      ; NFA -> 3
        FCB     $53,$54,$C1              ; "STA"

c_sta   FDB     asm_23
p_sta   FCB     $87

; ==================================================================
; STB
; ==================================================================
l_stb   FDB     l_sta                    ; LFA -> STA

n_stb   FCB     $83                      ; NFA -> 3
        FCB     $53,$54,$C2              ; "STB"

c_stb   FDB     asm_23
p_stb   FCB     $C7

; ==================================================================
; STX
; ==================================================================
l_stx   FDB     l_stb                    ; LFA -> STB

n_stx   FCB     $83                      ; NFA -> 3
        FCB     $53,$54,$D8              ; "STX"

c_stx   FDB     asm_23
p_stx   FCB     $8F

; ==================================================================
; STD
; ==================================================================
l_std   FDB     l_stx                    ; LFA -> STX

n_std   FCB     $83                      ; NFA -> 3
        FCB     $53,$54,$C4              ; "STD"

c_std   FDB     asm_23
p_std   FCB     $CD

; ==================================================================
; STU
; ==================================================================
l_stu   FDB     l_std                    ; LFA -> STD

n_stu   FCB     $83                      ; NFA -> 3
        FCB     $53,$54,$D5              ; "STU"

c_stu   FDB     asm_23
p_stu   FCB     $CF

; ==================================================================
; JSR
; ==================================================================
l_jsr   FDB     l_bitb                   ; LFA -> BITB

n_jsr   FCB     $83                      ; NFA -> 3
        FCB     $4A,$53,$D2              ; "JSR"

c_jsr   FDB     asm_23
p_jsr   FCB     $8D

; ==================================================================
; STY
; ==================================================================
l_sty   FDB     l_stu                    ; LFA -> STU

n_sty   FCB     $83                      ; NFA -> 3
        FCB     $53,$54,$D9              ; "STY"

c_sty   FDB     do_col                   ; : STY
p_sty   FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_ckom                   ; C,
        FDB     c_stx                    ; STX
        FDB     c_semi_                  ; (;)

; ==================================================================
; STS
; ==================================================================
l_sts   FDB     l_sty                    ; LFA -> STY

n_sts   FCB     $83                      ; NFA -> 3
        FCB     $53,$54,$D3              ; "STS"

c_sts   FDB     do_col                   ; : STS
p_sts   FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_ckom                   ; C,
        FDB     c_stu                    ; STU
        FDB     c_semi_                  ; (;)

; ==================================================================
; %L
; ==================================================================
l_perl  FDB     l_pers                   ; LFA -> %S

n_perl  FCB     $82                      ; NFA -> 2
        FCB     $25,$CC                  ; "%L"

c_perl  FDB     do_col                   ; : %L
p_perl  FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_lea JMP     do_does
        FDB     c_peri                   ; %I
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z3607            ; ?BRANCH --Z3607--v
        FDB     c_1                      ; 1
        FDB     c_peri                   ; %I
        FDB     c_exc                    ; !
        FDB     c_perch                  ; %CHK
Z3607   FDB     c_cat                    ; C@
        FDB     c_ckom                   ; C,
        FDB     c_perix                  ; %IX
        FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; LEAX
; ==================================================================
l_leax  FDB     l_lds                    ; LFA -> LDS

n_leax  FCB     $84                      ; NFA -> 4
        FCB     $4C,$45,$41,$D8          ; "LEAX"

c_leax  FDB     asm_lea
p_leax  FCB     $30

; ==================================================================
; LEAY
; ==================================================================
l_leay  FDB     l_leax                   ; LFA -> LEAX

n_leay  FCB     $84                      ; NFA -> 4
        FCB     $4C,$45,$41,$D9          ; "LEAY"

c_leay  FDB     asm_lea
p_leay  FCB     $31

; ==================================================================
; LEAS
; ==================================================================
l_leas  FDB     l_leay                   ; LFA -> LEAY

n_leas  FCB     $84                      ; NFA -> 4
        FCB     $4C,$45,$41,$D3          ; "LEAS"

c_leas  FDB     asm_lea
p_leas  FCB     $32

; ==================================================================
; LEAU
; ==================================================================
l_leau  FDB     l_leas                   ; LFA -> LEAS

n_leau  FCB     $84                      ; NFA -> 4
        FCB     $4C,$45,$41,$D5          ; "LEAU"

c_leau  FDB     asm_lea
p_leau  FCB     $33

; ==================================================================
; %C
; ==================================================================
l_perc  FDB     l_perl                   ; LFA -> %L

n_perc  FCB     $82                      ; NFA -> 2
        FCB     $25,$C3                  ; "%C"

c_perc  FDB     do_col                   ; : %C
p_perc  FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_cc  JMP     do_does
        FDB     c_pern_                  ; %#
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z365B            ; ?BRANCH --Z365B--v
        FDB     c_1                      ; 1
        FDB     c_pern_                  ; %#
        FDB     c_exc                    ; !
        FDB     c_perch                  ; %CHK
Z365B   FDB     c_cat                    ; C@
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
        FDB     c_0                      ; 0
        FDB     c_pern_                  ; %#
        FDB     c_exc                    ; !
        FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; ORCC
; ==================================================================
l_orcc  FDB     l_sts                    ; LFA -> STS

n_orcc  FCB     $84                      ; NFA -> 4
        FCB     $4F,$52,$43,$C3          ; "ORCC"

c_orcc  FDB     asm_cc
p_orcc  FCB     $1A

; ==================================================================
; ANDCC
; ==================================================================
l_andcc FDB     l_perc                   ; LFA -> %C

n_andcc FCB     $85                      ; NFA -> 5
        FCB     $41,$4E,$44,$43,$C3      ; "ANDCC"

c_andcc FDB     asm_cc
p_andcc FCB     $1C

; ==================================================================
; CWAI
; ==================================================================
l_cwai  FDB     l_orcc                   ; LFA -> ORCC

n_cwai  FCB     $84                      ; NFA -> 4
        FCB     $43,$57,$41,$C9          ; "CWAI"

c_cwai  FDB     asm_cc
p_cwai  FCB     $3C

; ==================================================================
; %RL
; ==================================================================
l_perrl FDB     l_andcc                  ; LFA -> ANDCC

n_perrl FCB     $83                      ; NFA -> 3
        FCB     $25,$52,$CC              ; "%RL"

c_perrl FDB     do_col                   ; : %RL
p_perrl FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_rl  JMP     do_does
        FDB     c_dup                    ; DUP
        FDB     c_cat                    ; C@
        FDB     c_perp                   ; %P
        FDB     c_at                     ; @
        FDB     c_or                     ; OR
        FDB     c_perp                   ; %P
        FDB     c_exc                    ; !
        FDB     c_1add                   ; 1+
        FDB     c_cat                    ; C@
        FDB     c_pert                   ; %T
        FDB     c_at,c_lit_              ; @
        FDB     $0010                    ; 16
        FDB     c_ast                    ; *
        FDB     c_add                    ; +
        FDB     c_pert                   ; %T
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; _PC
; ==================================================================
l_pc    FDB     l_cwai                   ; LFA -> CWAI

n_pc    FCB     $83                      ; NFA -> 3
        FCB     $5F,$50,$C3              ; "_PC"

c_pc    FDB     asm_rl
p_pc    FDB     $8005

; ==================================================================
; _S
; ==================================================================
l_s1    FDB     l_pc                     ; LFA -> _PC

n_s1    FCB     $82                      ; NFA -> 2
        FCB     $5F,$D3                  ; "_S"

c_s1    FDB     asm_rl
p_s1    FDB     $4004

; ==================================================================
; _U
; ==================================================================
l_u0    FDB     l_s1                     ; LFA -> _S

n_u0    FCB     $82                      ; NFA -> 2
        FCB     $5F,$D5                  ; "_U"

c_u0    FDB     asm_rl
p_u0    FDB     $4003

; ==================================================================
; _Y
; ==================================================================
l_y     FDB     l_u0                     ; LFA -> _U

n_y     FCB     $82                      ; NFA -> 2
        FCB     $5F,$D9                  ; "_Y"

c_y     FDB     asm_rl
p_y     FDB     $2002

; ==================================================================
; _X
; ==================================================================
l_x0    FDB     l_y                      ; LFA -> _Y

n_x0    FCB     $82                      ; NFA -> 2
        FCB     $5F,$D8                  ; "_X"

c_x0    FDB     asm_rl
p_x0    FDB     $1001

; ==================================================================
; _D
; ==================================================================
l_d0    FDB     l_x0                     ; LFA -> _X

n_d0    FCB     $82                      ; NFA -> 2
        FCB     $5F,$C4                  ; "_D"

c_d0    FDB     asm_rl
p_d0    FDB     p_ast

; ==================================================================
; _DP
; ==================================================================
l_dp0   FDB     l_d0                     ; LFA -> _D

n_dp0   FCB     $83                      ; NFA -> 3
        FCB     $5F,$44,$D0              ; "_DP"

c_dp0   FDB     asm_rl
p_dp0   FDB     l_do

; ==================================================================
; _CC
; ==================================================================
l_cc    FDB     l_dp0                    ; LFA -> _DP

n_cc    FCB     $83                      ; NFA -> 3
        FCB     $5F,$43,$C3              ; "_CC"

c_cc    FDB     asm_rl
p_cc    FDB     $010A

; ==================================================================
; _B
; ==================================================================
l_b0    FDB     l_cc                     ; LFA -> _CC

n_b0    FCB     $82                      ; NFA -> 2
        FCB     $5F,$C2                  ; "_B"

c_b0    FDB     asm_rl
p_b0    FDB     $0409

; ==================================================================
; _A
; ==================================================================
l_a     FDB     l_b0                     ; LFA -> _B

n_a     FCB     $82                      ; NFA -> 2
        FCB     $5F,$C1                  ; "_A"

c_a     FDB     asm_rl
p_a     FDB     $0208

; ==================================================================
; %PTE
; ==================================================================
l_perpt FDB     l_perrl                  ; LFA -> %RL

n_perpt FCB     $84                      ; NFA -> 4
        FCB     $25,$50,$54,$C5          ; "%PTE"

c_perpt FDB     do_col                   ; : %PTE
p_perpt FDB     c_0                      ; 0
        FDB     c_perp                   ; %P
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_pert                   ; %T
        FDB     c_exc                    ; !
        FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; %PP
; ==================================================================
l_perpp FDB     l_perpt                  ; LFA -> %PTE

n_perpp FCB     $83                      ; NFA -> 3
        FCB     $25,$50,$D0              ; "%PP"

c_perpp FDB     do_col                   ; : %PP
p_perpp FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_pp  JMP     do_does
        FDB     c_cat                    ; C@
        FDB     c_ckom                   ; C,
        FDB     c_perp                   ; %P
        FDB     c_at                     ; @
        FDB     c_ckom                   ; C,
        FDB     c_perpt                  ; %PTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; PSHS
; ==================================================================
l_pshs  FDB     l_leau                   ; LFA -> LEAU

n_pshs  FCB     $84                      ; NFA -> 4
        FCB     $50,$53,$48,$D3          ; "PSHS"

c_pshs  FDB     asm_pp
p_pshs  FCB     $34

; ==================================================================
; PSHU
; ==================================================================
l_pshu  FDB     l_pshs                   ; LFA -> PSHS

n_pshu  FCB     $84                      ; NFA -> 4
        FCB     $50,$53,$48,$D5          ; "PSHU"

c_pshu  FDB     asm_pp
p_pshu  FCB     $36

; ==================================================================
; PULS
; ==================================================================
l_puls  FDB     l_pshu                   ; LFA -> PSHU

n_puls  FCB     $84                      ; NFA -> 4
        FCB     $50,$55,$4C,$D3          ; "PULS"

c_puls  FDB     asm_pp
p_puls  FCB     $35

; ==================================================================
; PULU
; ==================================================================
l_pulu  FDB     l_puls                   ; LFA -> PULS

n_pulu  FCB     $84                      ; NFA -> 4
        FCB     $50,$55,$4C,$D5          ; "PULU"

c_pulu  FDB     asm_pp
p_pulu  FCB     $37

; ==================================================================
; TFR
; ==================================================================
l_tfr   FDB     l_pulu                   ; LFA -> PULU

n_tfr   FCB     $83                      ; NFA -> 3
        FCB     $54,$46,$D2              ; "TFR"

c_tfr   FDB     do_col                   ; : TFR
p_tfr   FDB     c_lit_
        FDB     $001F                    ; 31
        FDB     c_ckom                   ; C,
        FDB     c_pert                   ; %T
        FDB     c_at                     ; @
        FDB     c_ckom                   ; C,
        FDB     c_perpt                  ; %PTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; EXG
; ==================================================================
l_exg   FDB     l_perpp                  ; LFA -> %PP

n_exg   FCB     $83                      ; NFA -> 3
        FCB     $45,$58,$C7              ; "EXG"

c_exg   FDB     do_col                   ; : EXG
p_exg   FDB     c_lit_
        FDB     $001E                    ; 30
        FDB     c_ckom                   ; C,
        FDB     c_pert                   ; %T
        FDB     c_at                     ; @
        FDB     c_ckom                   ; C,
        FDB     c_perpt                  ; %PTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; %RA
; ==================================================================
l_perra FDB     l_exg                    ; LFA -> EXG

n_perra FCB     $83                      ; NFA -> 3
        FCB     $25,$52,$C1              ; "%RA"

c_perra FDB     do_col                   ; : %RA
p_perra FDB     c_2add                   ; 2+
        FDB     c_sub                    ; -
        FDB     c_dup,c_lit_             ; DUP
        FDB     $007F                    ; 127
        FDB     c_gt                     ; >
        FDB     c_over,c_lit_            ; OVER
        FDB     $FF80                    ; -128
        FDB     c_lt                     ; <
        FDB     c_or                     ; OR
        FDB     c_abort                  ; (ABORT") len=19
        FCB     $13
        FDB     $4164,$6472,$6573,$732D  ; "Address-"
        FDB     $7261,$6E67,$6520,$6572  ; "range er"
        FCB     $72,$6F,$72              ; "ror"
        FDB     c_semi_                  ; (;)

; ==================================================================
; BRA
; ==================================================================
l_bra   FDB     l_jsr                    ; LFA -> JSR

n_bra   FCB     $83                      ; NFA -> 3
        FCB     $42,$52,$C1              ; "BRA"

c_bra   FDB     do_col                   ; : BRA
p_bra   FDB     c_here                   ; HERE
        FDB     c_perra                  ; %RA
        FDB     c_perb                   ; %B
        FDB     c_at,c_lit_              ; @
        FDB     $0020                    ; 32
        FDB     c_add                    ; +
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
        FDB     c_0                      ; 0
        FDB     c_perb                   ; %B
        FDB     c_exc                    ; !
        FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; BSR
; ==================================================================
l_bsr   FDB     l_bra                    ; LFA -> BRA

n_bsr   FCB     $83                      ; NFA -> 3
        FCB     $42,$53,$D2              ; "BSR"

c_bsr   FDB     do_col                   ; : BSR
p_bsr   FDB     c_here                   ; HERE
        FDB     c_perra,c_lit_           ; %RA
        FDB     $008D                    ; 141
        FDB     c_ckom                   ; C,
        FDB     c_ckom                   ; C,
        FDB     c_perch                  ; %CHK
        FDB     c_semi_                  ; (;)

; ==================================================================
; %CB
; ==================================================================
l_percb FDB     l_perra                  ; LFA -> %RA

n_percb FCB     $83                      ; NFA -> 3
        FCB     $25,$43,$C2              ; "%CB"

c_percb FDB     do_col                   ; : %CB
p_percb FDB     c_creat                  ; CREATE
        FDB     c_ckom                   ; C,
        FDB     c_semic                  ; (;CODE)
asm_cb  JMP     do_does
        FDB     c_cat                    ; C@
        FDB     c_perb                   ; %B
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; ALWAYS
; ==================================================================
l_alway FDB     l_percb                  ; LFA -> %CB

n_alway FCB     $86                      ; NFA -> 6
        FCB     $41,$4C,$57,$41,$59,$D3  ; "ALWAYS"

c_alway FDB     asm_cb
p_alway FCB     $00

; ==================================================================
; NEVER
; ==================================================================
l_never FDB     l_bsr                    ; LFA -> BSR

n_never FCB     $85                      ; NFA -> 5
        FCB     $4E,$45,$56,$45,$D2      ; "NEVER"

c_never FDB     asm_cb
p_never FCB     $01

; ==================================================================
; HI
; ==================================================================
l_hi    FDB     l_tfr                    ; LFA -> TFR

n_hi    FCB     $82                      ; NFA -> 2
        FCB     $48,$C9                  ; "HI"

c_hi    FDB     asm_cb
p_hi    FCB     $02

; ==================================================================
; LS
; ==================================================================
l_ls    FDB     l_hi                     ; LFA -> HI

n_ls    FCB     $82                      ; NFA -> 2
        FCB     $4C,$D3                  ; "LS"

c_ls    FDB     asm_cb
p_ls    FCB     $03

; ==================================================================
; CC
; ==================================================================
l_cc0   FDB     l_a                      ; LFA -> _A

n_cc0   FCB     $82                      ; NFA -> 2
        FCB     $43,$C3                  ; "CC"

c_cc0   FDB     asm_cb
p_cc0   FCB     $04

; ==================================================================
; CS
; ==================================================================
l_cs    FDB     l_cc0                    ; LFA -> CC

n_cs    FCB     $82                      ; NFA -> 2
        FCB     $43,$D3                  ; "CS"

c_cs    FDB     asm_cb
p_cs    FCB     $05

; ==================================================================
; NE
; ==================================================================
l_ne    FDB     l_never                  ; LFA -> NEVER

n_ne    FCB     $82                      ; NFA -> 2
        FCB     $4E,$C5                  ; "NE"

c_ne    FDB     asm_cb
p_ne    FCB     $06

; ==================================================================
; EQ
; ==================================================================
l_eq0   FDB     l_alway                  ; LFA -> ALWAYS

n_eq0   FCB     $82                      ; NFA -> 2
        FCB     $45,$D1                  ; "EQ"

c_eq0   FDB     asm_cb
p_eq0   FCB     $07

; ==================================================================
; VC
; ==================================================================
l_vc    FDB     l_ne                     ; LFA -> NE

n_vc    FCB     $82                      ; NFA -> 2
        FCB     $56,$C3                  ; "VC"

c_vc    FDB     asm_cb
p_vc    FCB     $08

; ==================================================================
; VS
; ==================================================================
l_vs    FDB     l_vc                     ; LFA -> VC

n_vs    FCB     $82                      ; NFA -> 2
        FCB     $56,$D3                  ; "VS"

c_vs    FDB     asm_cb
p_vs    FCB     $09

; ==================================================================
; PL
; ==================================================================
l_pl    FDB     l_ls                     ; LFA -> LS

n_pl    FCB     $82                      ; NFA -> 2
        FCB     $50,$CC                  ; "PL"

c_pl    FDB     asm_cb
p_pl    FCB     $0A

; ==================================================================
; MI
; ==================================================================
l_mi    FDB     l_eq0                    ; LFA -> EQ

n_mi    FCB     $82                      ; NFA -> 2
        FCB     $4D,$C9                  ; "MI"

c_mi    FDB     asm_cb
p_mi    FCB     $0B

; ==================================================================
; GE
; ==================================================================
l_ge    FDB     l_cs                     ; LFA -> CS

n_ge    FCB     $82                      ; NFA -> 2
        FCB     $47,$C5                  ; "GE"

c_ge    FDB     asm_cb
p_ge    FCB     $0C

; ==================================================================
; LT
; ==================================================================
l_lt0   FDB     l_pl                     ; LFA -> PL

n_lt0   FCB     $82                      ; NFA -> 2
        FCB     $4C,$D4                  ; "LT"

c_lt0   FDB     asm_cb
p_lt0   FCB     $0D

; ==================================================================
; GT
; ==================================================================
l_gt0   FDB     l_ge                     ; LFA -> GE

n_gt0   FCB     $82                      ; NFA -> 2
        FCB     $47,$D4                  ; "GT"

c_gt0   FDB     asm_cb
p_gt0   FCB     $0E

; ==================================================================
; LE
; ==================================================================
l_le    FDB     l_lt0                    ; LFA -> LT

n_le    FCB     $82                      ; NFA -> 2
        FCB     $4C,$C5                  ; "LE"

c_le    FDB     asm_cb
p_le    FCB     $0F

; ==================================================================
; %PAIR
; ==================================================================
l_perpa FDB     l_mi                     ; LFA -> MI

n_perpa FCB     $85                      ; NFA -> 5
        FCB     $25,$50,$41,$49,$D2      ; "%PAIR"

c_perpa FDB     do_col                   ; : %PAIR
p_perpa FDB     c_sub                    ; -
        FDB     c_abort                  ; (ABORT") len=21
        FCB     $15
        FDB     $556E,$7061,$6972,$6564  ; "Unpaired"
        FDB     $2063,$6F6E,$6469,$7469  ; " conditi"
        FCB     $6F,$6E,$61,$6C,$73      ; "onals"
        FDB     c_semi_                  ; (;)

; ==================================================================
; %-B
; ==================================================================
l_persu FDB     l_perpa                  ; LFA -> %PAIR

n_persu FCB     $83                      ; NFA -> 3
        FCB     $25,$2D,$C2              ; "%-B"

c_persu FDB     do_col                   ; : %-B
p_persu FDB     c_perb                   ; %B
        FDB     c_at                     ; @
        FDB     c_1                      ; 1
        FDB     c_xor                    ; XOR
        FDB     c_perb                   ; %B
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; IF,
; ==================================================================
l_ifkom FDB     l_persu                  ; LFA -> %-B

n_ifkom FCB     $83                      ; NFA -> 3
        FCB     $49,$46,$AC              ; "IF,"

c_ifkom FDB     do_col                   ; : IF,
p_ifkom FDB     c_here,c_lit_            ; HERE
        FDB     $00F2                    ; 242
        FDB     c_persu                  ; %-B
        FDB     c_here                   ; HERE
        FDB     c_bra                    ; BRA
        FDB     c_semi_                  ; (;)

; ==================================================================
; THEN,
; ==================================================================
l_thenk FDB     l_le                     ; LFA -> LE

n_thenk FCB     $85                      ; NFA -> 5
        FCB     $54,$48,$45,$4E,$AC      ; "THEN,"

c_thenk FDB     do_col                   ; : THEN,
p_thenk FDB     c_1                      ; 1
        FDB     c_or,c_lit_              ; OR
        FDB     $00F3                    ; 243
        FDB     c_perpa                  ; %PAIR
        FDB     c_here                   ; HERE
        FDB     c_over                   ; OVER
        FDB     c_perra                  ; %RA
        FDB     c_swap                   ; SWAP
        FDB     c_1add                   ; 1+
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; ELSE,
; ==================================================================
l_elsek FDB     l_ifkom                  ; LFA -> IF,

n_elsek FCB     $85                      ; NFA -> 5
        FCB     $45,$4C,$53,$45,$AC      ; "ELSE,"

c_elsek FDB     do_col                   ; : ELSE,
p_elsek FDB     c_lit_
        FDB     $00F2                    ; 242
        FDB     c_perpa                  ; %PAIR
        FDB     c_to_r                   ; >R
        FDB     c_here,c_lit_            ; HERE
        FDB     $00F3                    ; 243
        FDB     c_here                   ; HERE
        FDB     c_alway                  ; ALWAYS
        FDB     c_bra                    ; BRA
        FDB     c_from_,c_lit_           ; R>
        FDB     $00F2                    ; 242
        FDB     c_thenk                  ; THEN,
        FDB     c_semi_                  ; (;)

; ==================================================================
; BEGIN,
; ==================================================================
l_begi0 FDB     l_vs                     ; LFA -> VS

n_begi0 FCB     $86                      ; NFA -> 6
        FCB     $42,$45,$47,$49,$4E,$AC  ; "BEGIN,"

c_begi0 FDB     do_col                   ; : BEGIN,
p_begi0 FDB     c_here,c_lit_            ; HERE
        FDB     $00F1                    ; 241
        FDB     c_semi_                  ; (;)

; ==================================================================
; WHILE,
; ==================================================================
l_whil0 FDB     l_gt0                    ; LFA -> GT

n_whil0 FCB     $86                      ; NFA -> 6
        FCB     $57,$48,$49,$4C,$45,$AC  ; "WHILE,"

c_whil0 FDB     do_col                   ; : WHILE,
p_whil0 FDB     c_lit_
        FDB     $00F1                    ; 241
        FDB     c_perpa                  ; %PAIR
        FDB     c_ifkom                  ; IF,
        FDB     c_2add                   ; 2+
        FDB     c_semi_                  ; (;)

; ==================================================================
; REPEAT,
; ==================================================================
l_repe0 FDB     l_begi0                  ; LFA -> BEGIN,

n_repe0 FCB     $87                      ; NFA -> 7
        FCB     $52,$45,$50,$45,$41,$54  ; "REPEAT"
        FCB     $AC                      ; ","

c_repe0 FDB     do_col                   ; : REPEAT,
p_repe0 FDB     c_lit_
        FDB     $00F4                    ; 244
        FDB     c_perpa                  ; %PAIR
        FDB     c_to_r                   ; >R
        FDB     c_alway                  ; ALWAYS
        FDB     c_bra                    ; BRA
        FDB     c_from_,c_lit_           ; R>
        FDB     $00F2                    ; 242
        FDB     c_thenk                  ; THEN,
        FDB     c_semi_                  ; (;)

; ==================================================================
; UNTIL,
; ==================================================================
l_unti0 FDB     l_elsek                  ; LFA -> ELSE,

n_unti0 FCB     $86                      ; NFA -> 6
        FCB     $55,$4E,$54,$49,$4C,$AC  ; "UNTIL,"

c_unti0 FDB     do_col                   ; : UNTIL,
p_unti0 FDB     c_lit_
        FDB     $00F1                    ; 241
        FDB     c_perpa                  ; %PAIR
        FDB     c_persu                  ; %-B
        FDB     c_bra                    ; BRA
        FDB     c_semi_                  ; (;)

; ==================================================================
; AGAIN,
; ==================================================================
l_agai0 FDB     l_unti0                  ; LFA -> UNTIL,

n_agai0 FCB     $86                      ; NFA -> 6
        FCB     $41,$47,$41,$49,$4E,$AC  ; "AGAIN,"

c_agai0 FDB     do_col                   ; : AGAIN,
p_agai0 FDB     c_never                  ; NEVER
        FDB     c_unti0                  ; UNTIL,
        FDB     c_semi_                  ; (;)

; ==================================================================
; END-CODE
; ==================================================================
l_endsu FDB     l_agai0                  ; LFA -> AGAIN,

n_endsu FCB     $88                      ; NFA -> 8
        FDB     $454E,$442D,$434F,$44C5  ; "END-CODE"

c_endsu FDB     do_col                   ; : END-CODE
p_endsu FDB     c_curre                  ; CURRENT
        FDB     c_at                     ; @
        FDB     c_conte                  ; CONTEXT
        FDB     c_exc                    ; !
        FDB     c_qmcsp                  ; ?CSP
        FDB     c_revea                  ; REVEAL
        FDB     c_semi_                  ; (;)

; ==================================================================
; NEXT,
; ==================================================================
l_nextk FDB     l_repe0                  ; LFA -> REPEAT,

n_nextk FCB     $85                      ; NFA -> 5
        FCB     $4E,$45,$58,$54,$AC      ; "NEXT,"

c_nextk FDB     do_col                   ; : NEXT,
p_nextk FDB     c_next                   ; >NEXT
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0100                    ; 256
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z3A05            ; ?BRANCH --Z3A05--v
        FDB     c_from4                  ; <<
Z3A05   FDB     c_jmp                    ; JMP
        FDB     c_semi_                  ; (;)

; ==================================================================
; CODE
; ==================================================================
l_code  FDB     l_gobug                  ; LFA -> GOBUG

n_code  FCB     $84                      ; NFA -> 4
        FCB     $43,$4F,$44,$C5          ; "CODE"

c_code  FDB     do_col                   ; : CODE
p_code  FDB     c_spat                   ; SP@
        FDB     c_csp                    ; CSP
        FDB     c_exc                    ; !
        FDB     c_creat                  ; CREATE
        FDB     c_hide                   ; HIDE
        FDB     c_here                   ; HERE
        FDB     c_here                   ; HERE
        FDB     c_2sub                   ; 2-
        FDB     c_exc                    ; !
        FDB     c_assem                  ; ASSEMBLER
        FDB     c_perin                  ; %INIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; ;CODE
; ==================================================================
l_semi0 FDB     l_code                   ; LFA -> CODE

n_semi0 FCB     $C5                      ; NFA -> 5 IMMEDIATE
        FCB     $3B,$43,$4F,$44,$C5      ; ";CODE"

c_semi0 FDB     do_col                   ; : ;CODE
p_semi0 FDB     c_qmcsp                  ; ?CSP
        FDB     c_compi                  ; COMPILE
        FDB     c_semic                  ; (;CODE)
        FDB     c_revea                  ; REVEAL
        FDB     c_lsq                    ; [
        FDB     c_assem                  ; ASSEMBLER
        FDB     c_perin                  ; %INIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; >DOS
; ==================================================================
l_to_do FDB     l_resum                  ; LFA -> RESUME

n_to_do FCB     $84                      ; NFA -> 4
        FCB     $3E,$44,$4F,$D3          ; ">DOS"

c_to_do FDB     p_to_do                  ; ASSEMBLER
p_to_do PULU    X,D
        PSHS    Y
        TSTB
        BPL     Z3A56
        LDB     #$7F
Z3A56   LDY     #LINBUF
        STY     LINBFP
Z3A5E   LDA     ,X+
        STA     ,Y+
        DECB
        BNE     Z3A5E
        LDA     #$0D
        STA     ,Y
        PULS    Y
        JMP     p_next

; ==================================================================
; DOS
; ==================================================================
l_dos   FDB     l_trace                  ; LFA -> TRACE

n_dos   FCB     $83                      ; NFA -> 3
        FCB     $44,$4F,$D3              ; "DOS"

c_dos   FDB     p_dos                    ; ASSEMBLER
p_dos   PSHS    U,Y,DP
        JSR     DOCMND
        PULS    U,Y,DP
        JMP     p_next

; ==================================================================
; _(
; ==================================================================
l_lp0   FDB     l_semi0                  ; LFA -> ;CODE

n_lp0   FCB     $82                      ; NFA -> 2
        FCB     $5F,$A8                  ; "_("

c_lp0   FDB     do_col                   ; : _(
p_lp0   FDB     c_lit_
        FDB     $0029                    ; 41
        FDB     c_word                   ; WORD
        FDB     c_count                  ; COUNT
        FDB     c_to_do                  ; >DOS
        FDB     c_dos                    ; DOS
        FDB     c_semi_                  ; (;)

; ==================================================================
; GETFIL
; ==================================================================
l_getfi FDB     l_lp0                    ; LFA -> _(

n_getfi FCB     $86                      ; NFA -> 6
        FCB     $47,$45,$54,$46,$49,$CC  ; "GETFIL"

c_getfi FDB     p_getfi                  ; ASSEMBLER
p_getfi PSHS    U,Y,DP
        LDX     ,U
        JSR     GETFIL
        PULS    U,Y,DP
        BCC     Z3AAD
        LDB     $01,X
        BRA     Z3AAE
Z3AAD   CLRB
Z3AAE   CLRA
        STD     ,U
        JMP     p_next

; ==================================================================
; FILENAME
; ==================================================================
l_filen FDB     l_to_do                  ; LFA -> >DOS

n_filen FCB     $88                      ; NFA -> 8
        FDB     $4649,$4C45,$4E41,$4DC5  ; "FILENAME"

c_filen FDB     do_col                   ; : FILENAME
p_filen FDB     c_to_r                   ; >R
        FDB     c_to_do                  ; >DOS
        FDB     c_from_                  ; R>
        FDB     c_getfi                  ; GETFIL
        FDB     c_semi_                  ; (;)

; ==================================================================
; SETEXT
; ==================================================================
l_setex FDB     l_getfi                  ; LFA -> GETFIL

n_setex FCB     $86                      ; NFA -> 6
        FCB     $53,$45,$54,$45,$58,$D4  ; "SETEXT"

c_setex FDB     p_setex                  ; ASSEMBLER
p_setex PULU    X,D
        PSHS    U,Y,DP
        EXG     D,X
        TFR     B,A
        JSR     SETEXT
        PULS    U,Y,DP
        JMP     p_next

; ==================================================================
; CLSALL
; ==================================================================
l_clsal FDB     l_setex                  ; LFA -> SETEXT

n_clsal FCB     $86                      ; NFA -> 6
        FCB     $43,$4C,$53,$41,$4C,$CC  ; "CLSALL"

c_clsal FDB     p_clsal                  ; ASSEMBLER
p_clsal PSHS    U,Y,DP
        JSR     FMSCLS
        PULS    U,Y,DP
        JMP     p_next

; ==================================================================
; RPTERR
; ==================================================================
l_rpter FDB     l_filen                  ; LFA -> FILENAME

n_rpter FCB     $86                      ; NFA -> 6
        FCB     $52,$50,$54,$45,$52,$D2  ; "RPTERR"

c_rpter FDB     p_rpter                  ; ASSEMBLER
p_rpter PSHS    U,Y,DP
        PULU    X
        JSR     RPTERR
        PULS    U,Y,DP
        JMP     p_next

; ==================================================================
; FMS
; ==================================================================
l_fms   FDB     l_rpter                  ; LFA -> RPTERR

n_fms   FCB     $83                      ; NFA -> 3
        FCB     $46,$4D,$D3              ; "FMS"

c_fms   FDB     p_fms                    ; ASSEMBLER
p_fms   LDX     ,U
        JSR     FMS
        CLRA
        LDB     $01,X
        STD     ,U
        JMP     p_next

; ==================================================================
; >FMS
; ==================================================================
l_to_fm FDB     l_fms                    ; LFA -> FMS

n_to_fm FCB     $84                      ; NFA -> 4
        FCB     $3E,$46,$4D,$D3          ; ">FMS"

c_to_fm FDB     p_to_fm                  ; ASSEMBLER
p_to_fm PULU    X
        LDD     ,U
        TFR     B,A
        JSR     FMS
        CLRA
        LDB     $01,X
        STD     ,U
        JMP     p_next

; ==================================================================
; FMS>
; ==================================================================
l_fmsgt FDB     l_to_fm                  ; LFA -> >FMS

n_fmsgt FCB     $84                      ; NFA -> 4
        FCB     $46,$4D,$53,$BE          ; "FMS>"

c_fmsgt FDB     p_fmsgt                  ; ASSEMBLER
p_fmsgt LDX     ,U
        JSR     FMS
        TFR     A,B
        CLRA
        STD     ,U
        LDB     $01,X
        PSHU    D
        JMP     p_next

; ==================================================================
; C-NAME
; ==================================================================
l_csubn FDB     l_clsal                  ; LFA -> CLSALL

n_csubn FCB     $86                      ; NFA -> 6
        FCB     $43,$2D,$4E,$41,$4D,$C5  ; "C-NAME"

c_csubn FDB     do_const                 ; CONSTANT C-NAME
p_csubn FDB     $001B                    ; 27

; ==================================================================
; C/L0
; ==================================================================
l_cdiv0 FDB     l_csubn                  ; LFA -> C-NAME

n_cdiv0 FCB     $83                      ; NFA -> 3
        FCB     $43,$2F,$CC              ; "C/L"

c_cdiv0 FDB     do_const                 ; CONSTANT C/L0
p_cdiv0 FDB     $0028                    ; 40

; ==================================================================
; |
; ==================================================================
l_bar   FDB     l_dos                    ; LFA -> DOS

n_bar   FCB     $C1                      ; NFA -> 1 IMMEDIATE
        FCB     $FC                      ; "|"

c_bar   FDB     do_col                   ; : |
p_bar   FDB     c_to_in                  ; >IN
        FDB     c_at                     ; @
        FDB     c_csubn                  ; C-NAME
        FDB     c_sub                    ; -
        FDB     c_cdiv0                  ; C/L0
        FDB     c_div                    ; /
        FDB     c_1add                   ; 1+
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ast                    ; *
        FDB     c_csubn                  ; C-NAME
        FDB     c_add                    ; +
        FDB     c_to_in                  ; >IN
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; VAR
; ==================================================================
l_var   FDB     l_fmsgt                  ; LFA -> FMS>

n_var   FCB     $83                      ; NFA -> 3
        FCB     $56,$41,$D2              ; "VAR"

c_var   FDB     do_col                   ; : VAR
p_var   FDB     c_varia                  ; VARIABLE
        FDB     c_semi_                  ; (;)

; ==================================================================
; CTE
; ==================================================================
l_cte   FDB     l_cdiv0                  ; LFA -> C/L0

n_cte   FCB     $83                      ; NFA -> 3
        FCB     $43,$54,$C5              ; "CTE"

c_cte   FDB     do_col                   ; : CTE
p_cte   FDB     c_const                  ; CONSTANT
        FDB     c_semi_                  ; (;)

; ==================================================================
; 2VAR
; ==================================================================
l_2var  FDB     l_var                    ; LFA -> VAR

n_2var  FCB     $84                      ; NFA -> 4
        FCB     $32,$56,$41,$D2          ; "2VAR"

c_2var  FDB     do_col                   ; : 2VAR
p_2var  FDB     c_2vari                  ; 2VARIABLE
        FDB     c_semi_                  ; (;)

; ==================================================================
; 2CTE
; ==================================================================
l_2cte  FDB     l_2var                   ; LFA -> 2VAR

n_2cte  FCB     $84                      ; NFA -> 4
        FCB     $32,$43,$54,$C5          ; "2CTE"

c_2cte  FDB     do_col                   ; : 2CTE
p_2cte  FDB     c_2cons                  ; 2CONSTANT
        FDB     c_semi_                  ; (;)

; ==================================================================
; EDITE>
; ==================================================================
l_edite FDB     l_assem                  ; LFA -> ASSEMBLER

n_edite FCB     $86                      ; NFA -> 6
        FCB     $45,$44,$49,$54,$45,$BE  ; "EDITE>"

c_edite FDB     p_edite                  ; ASSEMBLER
p_edite LDD     ,U++
        LDX     ,U++
        PSHS    U
        LEAU    D,X
        LEAU    $01,U
        JSR     ZDDFD
        PULS    U
        LEAX    -$01,X
        PSHU    X
        JMP     p_next

; ==================================================================
; -ROT
; ==================================================================
l_subro FDB     l_edite                  ; LFA -> EDITE>

n_subro FCB     $84                      ; NFA -> 4
        FCB     $2D,$52,$4F,$D4          ; "-ROT"

c_subro FDB     p_subro                  ; ASSEMBLER
p_subro PSHS    Y
        PULU    Y,X,D
        PSHU    D
        PSHU    Y,X
        PULS    Y
        JMP     p_next

; ==================================================================
; NIP
; ==================================================================
l_nip   FDB     l_2cte                   ; LFA -> 2CTE

n_nip   FCB     $83                      ; NFA -> 3
        FCB     $4E,$49,$D0              ; "NIP"

c_nip   FDB     p_nip                    ; ASSEMBLER
p_nip   PULU    X,D
        PSHU    D
        JMP     p_next

; ==================================================================
; TUCK
; ==================================================================
l_tuck  FDB     l_bar                    ; LFA -> |

n_tuck  FCB     $84                      ; NFA -> 4
        FCB     $54,$55,$43,$CB          ; "TUCK"

c_tuck  FDB     p_tuck                   ; ASSEMBLER
p_tuck  PULU    X,D
        PSHU    D
        PSHU    X,D
        JMP     p_next

; ==================================================================
; 2SWAP2
; ==================================================================
l_2swa0 FDB     l_nip                    ; LFA -> NIP

n_2swa0 FCB     $86                      ; NFA -> 6
        FCB     $32,$53,$57,$41,$50,$B2  ; "2SWAP2"

c_2swa0 FDB     p_2swa0                  ; ASSEMBLER
p_2swa0 PSHS    Y
        PULU    Y,X,D
        PSHS    D
        PULU    D
        PSHU    X
        PULS    X
        PSHU    X,D
        PSHU    Y
        PULS    Y
        JMP     p_next

; ==================================================================
; CHANGEBODY
; ==================================================================
l_chang FDB     l_cte                    ; LFA -> CTE

n_chang FCB     $8A                      ; NFA -> 10
        FDB     $4348,$414E,$4745,$424F  ; "CHANGEBO"
        FCB     $44,$D9                  ; "DY"

c_chang FDB     do_col                   ; : CHANGEBODY
p_chang FDB     c_to_bo                  ; >BODY
        FDB     c_to_r,c_lit_            ; >R
        FDB     $0030                    ; ' EXIT
        FDB     c_swap                   ; SWAP
        FDB     c_from_                  ; R>
        FDB     c_2exc                   ; 2!
        FDB     c_semi_                  ; (;)

; ==================================================================
; VXBODY
; ==================================================================
l_vxbod FDB     l_2swa0                  ; LFA -> 2SWAP2

n_vxbod FCB     $86                      ; NFA -> 6
        FCB     $56,$58,$42,$4F,$44,$D9  ; "VXBODY"

c_vxbod FDB     do_var                   ; VARIABLE VXBODY
p_vxbod FDB     $0A8C                    ; 2700
        FCB     $08,$C8,$0E,$76,$09,$B0,$01,$74,$00,$EB,$0F,$1B,$1B
        FCB     $21,$20,$20,$20,$20

; ==================================================================
; MEMOR
; ==================================================================
l_memor FDB     l_subro                  ; LFA -> -ROT

n_memor FCB     $85                      ; NFA -> 5
        FCB     $4D,$45,$4D,$4F,$D2      ; "MEMOR"

c_memor FDB     do_col                   ; : MEMOR
p_memor FDB     c_to_r                   ; >R
        FDB     c_to_bo                  ; >BODY
        FDB     c_2at                    ; 2@
        FDB     c_vxbod                  ; VXBODY
        FDB     c_from_                  ; R>
        FDB     c_add                    ; +
        FDB     c_2exc                   ; 2!
        FDB     c_semi_                  ; (;)

; ==================================================================
; RENDE
; ==================================================================
l_rende FDB     l_vxbod                  ; LFA -> VXBODY

n_rende FCB     $85                      ; NFA -> 5
        FCB     $52,$45,$4E,$44,$C5      ; "RENDE"

c_rende FDB     do_col                   ; : RENDE
p_rende FDB     c_vxbod                  ; VXBODY
        FDB     c_add                    ; +
        FDB     c_2at                    ; 2@
        FDB     c_rot                    ; ROT
        FDB     c_to_bo                  ; >BODY
        FDB     c_2exc                   ; 2!
        FDB     c_semi_                  ; (;)

; ==================================================================
; VX."
; ==================================================================
l_vxdot FDB     l_rende                  ; LFA -> RENDE

n_vxdot FCB     $84                      ; NFA -> 4
        FCB     $56,$58,$2E,$A2          ; 'VX."'

c_vxdot FDB     do_col                   ; : VX."
p_vxdot FDB     c_lit_
        FDB     $0928                    ; ' ."
        FDB     c_0                      ; 0
        FDB     c_rende                  ; RENDE
        FDB     c_semi_                  ; (;)

; ==================================================================
; VXWHERE
; ==================================================================
l_vxwhe FDB     l_vxdot                  ; LFA -> VX."

n_vxwhe FCB     $87                      ; NFA -> 7
        FCB     $56,$58,$57,$48,$45,$52  ; "VXWHER"
        FCB     $C5                      ; "E"

c_vxwhe FDB     do_col                   ; : VXWHERE
p_vxwhe FDB     c_lit_
        FDB     $21A6,c_lit_             ; ' WHERE
        FDB     $0004                    ; 4
        FDB     c_rende                  ; RENDE
        FDB     c_semi_                  ; (;)

; ==================================================================
; VXEXPECT
; ==================================================================
l_vxexp FDB     l_vxwhe                  ; LFA -> VXWHERE

n_vxexp FCB     $88                      ; NFA -> 8
        FDB     $5658,$4558,$5045,$43D4  ; "VXEXPECT"

c_vxexp FDB     do_col                   ; : VXEXPECT
p_vxexp FDB     c_lit_
        FDB     $0DEF,c_lit_             ; ' EXPECT
        FDB     $0008                    ; 8
        FDB     c_rende                  ; RENDE
        FDB     c_semi_                  ; (;)

; ==================================================================
; VXTERMINAL
; ==================================================================
l_vxter FDB     l_vxexp                  ; LFA -> VXEXPECT

n_vxter FCB     $8A                      ; NFA -> 10
        FDB     $5658,$5445,$524D,$494E  ; "VXTERMIN"
        FCB     $41,$CC                  ; "AL"

c_vxter FDB     do_col                   ; : VXTERMINAL
p_vxter FDB     c_lit_
        FDB     $1BCA,c_lit_             ; ' TERMINAL
        FDB     $000C                    ; 12
        FDB     c_rende                  ; RENDE
        FDB     c_semi_                  ; (;)

; ==================================================================
; ARRAY
; ==================================================================
l_array FDB     l_memor                  ; LFA -> MEMOR

n_array FCB     $85                      ; NFA -> 5
        FCB     $41,$52,$52,$41,$D9      ; "ARRAY"

c_array FDB     do_col                   ; : ARRAY
p_array FDB     c_creat                  ; CREATE
        FDB     c_2ast                   ; 2*
        FDB     c_allot                  ; ALLOT
        FDB     c_semic                  ; (;CODE)
        JMP     do_does
        FDB     c_swap                   ; SWAP
        FDB     c_2ast                   ; 2*
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; EXP-FLEX
; ==================================================================
l_expsu FDB     l_array                  ; LFA -> ARRAY

n_expsu FCB     $88                      ; NFA -> 8
        FDB     $4558,$502D,$464C,$45D8  ; "EXP-FLEX"

c_expsu FDB     do_col                   ; : EXP-FLEX
p_expsu FDB     c_over                   ; OVER
        FDB     c_to_r                   ; >R
        FDB     c_edite                  ; EDITE>
        FDB     c_from_                  ; R>
        FDB     c_sub                    ; -
        FDB     c_span                   ; SPAN
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; D2*
; ==================================================================
l_d2ast FDB     l_tuck                   ; LFA -> TUCK

n_d2ast FCB     $83                      ; NFA -> 3
        FCB     $44,$32,$AA              ; "D2*"

c_d2ast FDB     p_d2ast                  ; ASSEMBLER
p_d2ast ASL     $03,U
        ROL     $02,U
        ROL     $01,U
        ROL     ,U
        JMP     p_next

; ==================================================================
; DM*
; ==================================================================
l_dmast FDB     l_d2ast                  ; LFA -> D2*

n_dmast FCB     $83                      ; NFA -> 3
        FCB     $44,$4D,$AA              ; "DM*"

c_dmast FDB     do_col                   ; : DM*
p_dmast FDB     c_dup                    ; DUP
        FDB     c_rot                    ; ROT
        FDB     c_ast                    ; *
        FDB     c_subro                  ; -ROT
        FDB     c_umast                  ; UM*
        FDB     c_rot                    ; ROT
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; DM/MOD
; ==================================================================
l_dmdiv FDB     l_dmast                  ; LFA -> DM*

n_dmdiv FCB     $86                      ; NFA -> 6
        FCB     $44,$4D,$2F,$4D,$4F,$C4  ; "DM/MOD"

c_dmdiv FDB     do_col                   ; : DM/MOD
p_dmdiv FDB     c_tuck                   ; TUCK
        FDB     c_divmo                  ; /MOD
        FDB     c_to_r                   ; >R
        FDB     c_swap                   ; SWAP
        FDB     c_umdiv                  ; UM/MOD
        FDB     c_to_r                   ; >R
        FDB     c_0                      ; 0
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_semi_                  ; (;)

; ==================================================================
; D*
; ==================================================================
l_dast  FDB     l_dmdiv                  ; LFA -> DM/MOD

n_dast  FCB     $82                      ; NFA -> 2
        FCB     $44,$AA                  ; "D*"

c_dast  FDB     do_col                   ; : D*
p_dast  FDB     c_2dup                   ; 2DUP
        FDB     c_0                      ; 0
        FDB     c_0                      ; 0
        FDB     c_dlt                    ; D<
        FDB     c_to_r                   ; >R
        FDB     c_dabs                   ; DABS
        FDB     c_dabs                   ; DABS
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_2dup                   ; 2DUP
        FDB     c_from_                  ; R>
        FDB     c_dmast                  ; DM*
        FDB     c_from_                  ; R>
        FDB     c_subro                  ; -ROT
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_dmast,c_lit_           ; DM*
        FDB     $8000                    ; -32768
        FDB     c_dmast                  ; DM*
        FDB     c_2                      ; 2
        FDB     c_dmast                  ; DM*
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_dadd                   ; D+
        FDB     c_from_                  ; R>
        FDB     c_qmbra,Z3DC5            ; ?BRANCH --Z3DC5--v
        FDB     c_dnega                  ; DNEGATE
Z3DC5   FDB     c_semi_                  ; (;)

; ==================================================================
; <UD>
; ==================================================================
l_ud_   FDB     l_dast                   ; LFA -> D*

n_ud_   FCB     $84                      ; NFA -> 4
        FCB     $3C,$55,$44,$BE          ; "<UD>"

c_ud_   FDB     do_col                   ; : <UD>
p_ud_   FDB     c_from2                  ; <#
        FDB     c_n_s                    ; #S
        FDB     c_n_gt                   ; #>
        FDB     c_semi_                  ; (;)

; ==================================================================
; UD.
; ==================================================================
l_uddot FDB     l_expsu                  ; LFA -> EXP-FLEX

n_uddot FCB     $83                      ; NFA -> 3
        FCB     $55,$44,$AE              ; "UD."

c_uddot FDB     do_col                   ; : UD.
p_uddot FDB     c_ud_                    ; <UD>
        FDB     c_type                   ; TYPE
        FDB     c_space                  ; SPACE
        FDB     c_semi_                  ; (;)

; ==================================================================
; UD.R
; ==================================================================
l_uddo0 FDB     l_uddot                  ; LFA -> UD.

n_uddo0 FCB     $84                      ; NFA -> 4
        FCB     $55,$44,$2E,$D2          ; "UD.R"

c_uddo0 FDB     do_col                   ; : UD.R
p_uddo0 FDB     c_to_r                   ; >R
        FDB     c_ud_                    ; <UD>
        FDB     c_from_                  ; R>
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_spac0                  ; SPACES
        FDB     c_type                   ; TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; FALSE
; ==================================================================
l_false FDB     l_vxter                  ; LFA -> VXTERMINAL

n_false FCB     $85                      ; NFA -> 5
        FCB     $46,$41,$4C,$53,$C5      ; "FALSE"

c_false FDB     do_const                 ; CONSTANT FALSE
p_false FDB     $0000                    ; 0

; ==================================================================
; TRUE
; ==================================================================
l_true  FDB     l_ud_                    ; LFA -> <UD>

n_true  FCB     $84                      ; NFA -> 4
        FCB     $54,$52,$55,$C5          ; "TRUE"

c_true  FDB     do_const                 ; CONSTANT TRUE
p_true  FDB     $FFFF                    ; 65535

; ==================================================================
; NBCAR
; ==================================================================
l_nbcar FDB     l_false                  ; LFA -> FALSE

n_nbcar FCB     $85                      ; NFA -> 5
        FCB     $4E,$42,$43,$41,$D2      ; "NBCAR"

c_nbcar FDB     do_const                 ; CONSTANT NBCAR
p_nbcar FDB     Z03C0                    ; 960

; ==================================================================
; B/BUF
; ==================================================================
l_bdivb FDB     l_nbcar                  ; LFA -> NBCAR

n_bdivb FCB     $85                      ; NFA -> 5
        FCB     $42,$2F,$42,$55,$C6      ; "B/BUF"

c_bdivb FDB     do_const                 ; CONSTANT B/BUF
p_bdivb FDB     $0400                    ; 1024

; ==================================================================
; =<
; ==================================================================
l_eqlt  FDB     l_uddo0                  ; LFA -> UD.R

n_eqlt  FCB     $82                      ; NFA -> 2
        FCB     $3D,$BC                  ; "=<"

c_eqlt  FDB     do_col                   ; : =<
p_eqlt  FDB     c_gt                     ; >
        FDB     c_not                    ; NOT
        FDB     c_semi_                  ; (;)

; ==================================================================
; >=
; ==================================================================
l_to_eq FDB     l_bdivb                  ; LFA -> B/BUF

n_to_eq FCB     $82                      ; NFA -> 2
        FCB     $3E,$BD                  ; ">="

c_to_eq FDB     do_col                   ; : >=
p_to_eq FDB     c_lt                     ; <
        FDB     c_not                    ; NOT
        FDB     c_semi_                  ; (;)

; ==================================================================
; ~
; ==================================================================
l_tild  FDB     l_to_eq                  ; LFA -> >=

n_tild  FCB     $81                      ; NFA -> 1
        FCB     $FE                      ; "~"

c_tild  FDB     do_col                   ; : ~
p_tild  FDB     c_emit                   ; EMIT
        FDB     c_exit                   ; EXIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; P~
; ==================================================================
l_ptild FDB     l_true                   ; LFA -> TRUE

n_ptild FCB     $82                      ; NFA -> 2
        FCB     $50,$FE                  ; "P~"

c_ptild FDB     do_col                   ; : P~
p_ptild FDB     c_decim,c_lit_           ; DECIMAL
        FDB     $000C                    ; 12
        FDB     c_io                     ; IO
        FDB     c_1                      ; 1
        FDB     c_out                    ; OUT
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; US
; ==================================================================
l_us    FDB     l_eqlt                   ; LFA -> =<

n_us    FCB     $82                      ; NFA -> 2
        FCB     $55,$D3                  ; "US"

c_us    FDB     do_col                   ; : US
p_us    FDB     c_lit_
        FDB     $001B                    ; 27
        FDB     c_ptild,c_lit_           ; P~
        FDB     $0052                    ; 82
        FDB     c_ptild                  ; P~
        FDB     c_0                      ; 0
        FDB     c_ptild                  ; P~
        FDB     c_semi_                  ; (;)

; ==================================================================
; FR
; ==================================================================
l_fr    FDB     l_tild                   ; LFA -> ~

n_fr    FCB     $82                      ; NFA -> 2
        FCB     $46,$D2                  ; "FR"

c_fr    FDB     do_col                   ; : FR
p_fr    FDB     c_lit_
        FDB     $001B                    ; 27
        FDB     c_ptild,c_lit_           ; P~
        FDB     $0052                    ; 82
        FDB     c_ptild                  ; P~
        FDB     c_1                      ; 1
        FDB     c_ptild                  ; P~
        FDB     c_semi_                  ; (;)

; ==================================================================
; BSP
; ==================================================================
l_bsp   FDB     l_fr                     ; LFA -> FR

n_bsp   FCB     $83                      ; NFA -> 3
        FCB     $42,$53,$D0              ; "BSP"

c_bsp   FDB     do_col                   ; : BSP
p_bsp   FDB     c_lit_
        FDB     $0008                    ; 8
        FDB     c_ptild,c_lit_           ; P~
        FDB     $FFFE                    ; -2
        FDB     c_out                    ; OUT
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; FRP^
; ==================================================================
l_frpup FDB     l_bsp                    ; LFA -> BSP

n_frpup FCB     $84                      ; NFA -> 4
        FCB     $46,$52,$50,$DE          ; "FRP^"

c_frpup FDB     do_col                   ; : FRP^
p_frpup FDB     c_ptild                  ; P~
        FDB     c_bsp,c_lit_             ; BSP
        FDB     $005E                    ; 94
        FDB     c_ptild                  ; P~
        FDB     c_semi_                  ; (;)

; ==================================================================
; FRP"
; ==================================================================
l_frpqt FDB     l_frpup                  ; LFA -> FRP^

n_frpqt FCB     $84                      ; NFA -> 4
        FCB     $46,$52,$50,$A2          ; 'FRP"'

c_frpqt FDB     do_col                   ; : FRP"
p_frpqt FDB     c_ptild                  ; P~
        FDB     c_bsp,c_lit_             ; BSP
        FDB     $007E                    ; 126
        FDB     c_ptild                  ; P~
        FDB     c_semi_                  ; (;)

; ==================================================================
; <=>
; ==================================================================
l_eq_   FDB     l_ptild                  ; LFA -> P~

n_eq_   FCB     $83                      ; NFA -> 3
        FCB     $3C,$3D,$BE              ; "<=>"

c_eq_   FDB     do_col                   ; : <=>
p_eq_   FDB     c_to_r                   ; >R
        FDB     c_over                   ; OVER
        FDB     c_gt                     ; >
        FDB     c_swap                   ; SWAP
        FDB     c_from_                  ; R>
        FDB     c_gt                     ; >
        FDB     c_or                     ; OR
        FDB     c_not                    ; NOT
        FDB     c_semi_                  ; (;)

; ==================================================================
; SP
; ==================================================================
l_sp    FDB     l_chang                  ; LFA -> CHANGEBODY

n_sp    FCB     $82                      ; NFA -> 2
        FCB     $53,$D0                  ; "SP"

c_sp    FDB     do_col                   ; : SP
p_sp    FDB     c_spac0                  ; SPACES
        FDB     c_semi_                  ; (;)

; ==================================================================
; AL
; ==================================================================
l_al    FDB     l_us                     ; LFA -> US

n_al    FCB     $82                      ; NFA -> 2
        FCB     $41,$CC                  ; "AL"

c_al    FDB     do_col                   ; : AL
p_al    FDB     c_0                      ; 0
        FDB     c_do_,Z3F19              ; (DO) --Z3F19--v
Z3F13   FDB     c_cr                     ; CR
        FDB     c_loop_,Z3F13            ; (LOOP) --Z3F13--^
Z3F19   FDB     c_semi_                  ; (;)

; ==================================================================
; -AL
; ==================================================================
l_subal FDB     l_al                     ; LFA -> AL

n_subal FCB     $83                      ; NFA -> 3
        FCB     $2D,$41,$CC              ; "-AL"

c_subal FDB     do_col                   ; : -AL
p_subal FDB     c_0                      ; 0
        FDB     c_do_,Z3F33              ; (DO) --Z3F33--v
Z3F29   FDB     c_lit_
        FDB     $000B                    ; 11
        FDB     c_tild                   ; ~
        FDB     c_loop_,Z3F29            ; (LOOP) --Z3F29--^
Z3F33   FDB     c_semi_                  ; (;)

; ==================================================================
; FRP~
; ==================================================================
l_frpti FDB     l_frpqt                  ; LFA -> FRP"

n_frpti FCB     $84                      ; NFA -> 4
        FCB     $46,$52,$50,$FE          ; "FRP~"

c_frpti FDB     do_col                   ; : FRP~
p_frpti FDB     c_lit_
        FDB     $005F                    ; 95
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z3F56            ; ?BRANCH --Z3F56--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0091                    ; 145
        FDB     c_ptild                  ; P~
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z3F56   FDB     c_lit_
        FDB     $007C                    ; ' !
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z3F6E            ; ?BRANCH --Z3F6E--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $005C                    ; 92
        FDB     c_ptild                  ; P~
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z3F6E   FDB     c_lit_
        FDB     $007E                    ; 126
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z3F86            ; ?BRANCH --Z3F86--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0095                    ; 149
        FDB     c_ptild                  ; P~
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z3F86   FDB     c_lit_
        FDB     $0080                    ; 128
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z3FA2            ; ?BRANCH --Z3FA2--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $007B                    ; 123
        FDB     c_fr                     ; FR
        FDB     c_ptild                  ; P~
        FDB     c_us                     ; US
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z3FA2   FDB     c_lit_
        FDB     $0084                    ; 132
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z3FBE            ; ?BRANCH --Z3FBE--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $007D                    ; 125
        FDB     c_fr                     ; FR
        FDB     c_ptild                  ; P~
        FDB     c_us                     ; US
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z3FBE   FDB     c_lit_
        FDB     $0085                    ; 133
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z3FDA            ; ?BRANCH --Z3FDA--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $007C                    ; ' !
        FDB     c_fr                     ; FR
        FDB     c_ptild                  ; P~
        FDB     c_us                     ; US
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z3FDA   FDB     c_lit_
        FDB     $0086                    ; 134
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z3FF6            ; ?BRANCH --Z3FF6--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $005C                    ; 92
        FDB     c_fr                     ; FR
        FDB     c_ptild                  ; P~
        FDB     c_us                     ; US
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z3FF6   FDB     c_lit_
        FDB     $0087                    ; 135
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4012            ; ?BRANCH --Z4012--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0040                    ; 64
        FDB     c_fr                     ; FR
        FDB     c_ptild                  ; P~
        FDB     c_us                     ; US
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z4012   FDB     c_lit_
        FDB     $008B                    ; ' 2!
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z402A            ; ?BRANCH --Z402A--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0065                    ; 101
        FDB     c_frpqt                  ; FRP"
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z402A   FDB     c_lit_
        FDB     $008C                    ; 140
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4042            ; ?BRANCH --Z4042--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0069                    ; 105
        FDB     c_frpqt                  ; FRP"
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z4042   FDB     c_lit_
        FDB     $008E                    ; 142
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z405A            ; ?BRANCH --Z405A--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0075                    ; 117
        FDB     c_frpqt                  ; FRP"
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z405A   FDB     c_lit_
        FDB     $008F                    ; 143
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4072            ; ?BRANCH --Z4072--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0061                    ; 97
        FDB     c_frpup                  ; FRP^
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z4072   FDB     c_lit_
        FDB     $0090                    ; 144
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z408A            ; ?BRANCH --Z408A--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0065                    ; 101
        FDB     c_frpup                  ; FRP^
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z408A   FDB     c_lit_
        FDB     $0091                    ; 145
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z40A2            ; ?BRANCH --Z40A2--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0069                    ; 105
        FDB     c_frpup                  ; FRP^
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z40A2   FDB     c_lit_
        FDB     $0092                    ; 146
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z40BA            ; ?BRANCH --Z40BA--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $006F                    ; 111
        FDB     c_frpup                  ; FRP^
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z40BA   FDB     c_lit_
        FDB     $0093                    ; 147
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z40D2            ; ?BRANCH --Z40D2--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0075                    ; 117
        FDB     c_frpup                  ; FRP^
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z40D2   FDB     c_lit_
        FDB     $0082                    ; 130
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z40EA            ; ?BRANCH --Z40EA--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0096                    ; 150
        FDB     c_ptild                  ; P~
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z40EA   FDB     c_lit_
        FDB     $0088                    ; 136
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4106            ; ?BRANCH --Z4106--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $005B                    ; 91
        FDB     c_fr                     ; FR
        FDB     c_ptild                  ; P~
        FDB     c_us                     ; US
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z4106   FDB     c_lit_
        FDB     $0089                    ; 137
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z411E            ; ?BRANCH --Z411E--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $0018                    ; 24
        FDB     c_ptild                  ; P~
        FDB     c_branc,Z4126            ; BRANCH --Z4126--v
Z411E   FDB     c_dup                    ; DUP
        FDB     c_us                     ; US
        FDB     c_ptild                  ; P~
        FDB     c_drop                   ; DROP
Z4126   FDB     c_semi_                  ; (;)

; ==================================================================
; FTYPE
; ==================================================================
l_ftype FDB     l_frpti                  ; LFA -> FRP~

n_ftype FCB     $85                      ; NFA -> 5
        FCB     $46,$54,$59,$50,$C5      ; "FTYPE"

c_ftype FDB     do_col                   ; : FTYPE
p_ftype FDB     c_0                      ; 0
        FDB     c_max                    ; MAX
        FDB     c_0                      ; 0
        FDB     c_qmdo_,Z4144            ; (?DO) --Z4144--v
Z413C   FDB     c_count                  ; COUNT
        FDB     c_frpti                  ; FRP~
        FDB     c_loop_,Z413C            ; (LOOP) --Z413C--^
Z4144   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; (."F)
; ==================================================================
l_dotq1 FDB     l_eq_                    ; LFA -> <=>

n_dotq1 FCB     $85                      ; NFA -> 5
        FCB     $28,$2E,$22,$46,$A9      ; '(.\"F)'

c_dotq1 FDB     do_col                   ; : (."F)
p_dotq1 FDB     c_from_                  ; R>
        FDB     c_count                  ; COUNT
        FDB     c_2dup                   ; 2DUP
        FDB     c_add                    ; +
        FDB     c_to_r                   ; >R
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z416A            ; ?BRANCH --Z416A--v
        FDB     c_ftype                  ; FTYPE
        FDB     c_branc,Z416C            ; BRANCH --Z416C--v
Z416A   FDB     c_type                   ; TYPE
Z416C   FDB     c_semi_                  ; (;)

; ==================================================================
; ."F
; ==================================================================
l_dotq2 FDB     l_ftype                  ; LFA -> FTYPE

n_dotq2 FCB     $C3                      ; NFA -> 3 IMMEDIATE
        FCB     $2E,$22,$C6              ; '.\"F'

c_dotq2 FDB     do_col                   ; : ."F
p_dotq2 FDB     c_qmcom                  ; ?COMP
        FDB     c_compi                  ; COMPILE
        FDB     c_dotq1                  ; (."F)
        FDB     c_komqt                  ; ,"
        FDB     c_semi_                  ; (;)

; ==================================================================
; WHERE>
; ==================================================================
l_wher0 FDB     l_sp                     ; LFA -> SP

n_wher0 FCB     $86                      ; NFA -> 6
        FCB     $57,$48,$45,$52,$45,$BE  ; "WHERE>"

c_wher0 FDB     do_col                   ; : WHERE>
p_wher0 FDB     c_cr                     ; CR
        FDB     c_to_in                  ; >IN
        FDB     c_at                     ; @
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z425C            ; ?BRANCH --Z425C--v
        FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_qmbra,Z4202            ; ?BRANCH --Z4202--v
        FDB     c_csubn                  ; C-NAME
        FDB     c_sub                    ; -
        FDB     c_dup                    ; DUP
        FDB     c_nbcar                  ; NBCAR
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z41B9            ; ?BRANCH --Z41B9--v
        FDB     c_0                      ; 0
        FDB     c_to_in                  ; >IN
        FDB     c_exc                    ; !
        FDB     c_drop                   ; DROP
        FDB     c_exc                    ; !
        FDB     c_exit                   ; EXIT
Z41B9   FDB     c_0                      ; 0
        FDB     c_to_in                  ; >IN
        FDB     c_exc                    ; !
        FDB     c_dotqt                  ; (.") len=5
        FCB     $05
        FCB     $45,$63,$72,$61,$6E      ; "Ecran"
        FDB     c_space                  ; SPACE
        FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_dot                    ; .
        FDB     c_dotqt                  ; (.") len=6
        FCB     $06
        FCB     $6C,$69,$67,$6E,$65,$20  ; "ligne "
        FDB     c_cdiv0                  ; C/L0
        FDB     c_divmo                  ; /MOD
        FDB     c_dup                    ; DUP
        FDB     c_dot                    ; .
        FDB     c_cr                     ; CR
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ast                    ; *
        FDB     c_blk                    ; BLK
        FDB     c_at                     ; @
        FDB     c_block                  ; BLOCK
        FDB     c_add                    ; +
        FDB     c_csubn                  ; C-NAME
        FDB     c_add                    ; +
        FDB     c_pad                    ; PAD
        FDB     c_cdiv0                  ; C/L0
        FDB     c_cmove                  ; CMOVE
        FDB     c_pad                    ; PAD
        FDB     c_cdiv0                  ; C/L0
        FDB     c_subtr                  ; -TRAILING
        FDB     c_branc,Z420A            ; BRANCH --Z420A--v
Z4202   FDB     c_1add                   ; 1+
        FDB     c_tib                    ; TIB
        FDB     c_n_tib                  ; #TIB
        FDB     c_at                     ; @
Z420A   FDB     c_lit_
        FDB     $000B,c_lit_             ; 11
        FDB     $0004                    ; 4
        FDB     c_tild                   ; ~
        FDB     c_tild                   ; ~
        FDB     c_type                   ; TYPE
        FDB     c_cr                     ; CR
        FDB     c_2sub                   ; 2-
        FDB     c_0                      ; 0
        FDB     c_max                    ; MAX
        FDB     c_0                      ; 0
        FDB     c_qmdo_,Z423C            ; (?DO) --Z423C--v
Z4226   FDB     c_lit_
        FDB     $0003,c_lit_             ; 3
        FDB     $0004                    ; 4
        FDB     c_tild                   ; ~
        FDB     c_tild,c_lit_            ; ~
        FDB     $002E                    ; 46
        FDB     c_tild                   ; ~
        FDB     c_loop_,Z4226            ; (LOOP) --Z4226--^
Z423C   FDB     c_lit_
        FDB     $0004,c_lit_             ; 4
        FDB     $0004                    ; 4
        FDB     c_tild                   ; ~
        FDB     c_tild,c_lit_            ; ~
        FDB     $005E                    ; 94
        FDB     c_tild,c_lit_            ; ~
        FDB     $000E,c_lit_             ; 14
        FDB     $0004                    ; 4
        FDB     c_tild                   ; ~
        FDB     c_tild                   ; ~
        FDB     c_cr                     ; CR
Z425C   FDB     c_lit_
        FDB     $0014                    ; 20
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; D>S
; ==================================================================
l_dgts  FDB     l_dotq1                  ; LFA -> (."F)

n_dgts  FCB     $83                      ; NFA -> 3
        FCB     $44,$3E,$D3              ; "D>S"

c_dgts  FDB     do_col                   ; : D>S
p_dgts  FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; S>D
; ==================================================================
l_sgtd  FDB     l_wher0                  ; LFA -> WHERE>

n_sgtd  FCB     $83                      ; NFA -> 3
        FCB     $53,$3E,$C4              ; "S>D"

c_sgtd  FDB     do_col                   ; : S>D
p_sgtd  FDB     c_dup                    ; DUP
        FDB     c_0lt                    ; 0<
        FDB     c_semi_                  ; (;)

; ==================================================================
; INVS
; ==================================================================
l_invs  FDB     l_subal                  ; LFA -> -AL

n_invs  FCB     $84                      ; NFA -> 4
        FCB     $49,$4E,$56,$D3          ; "INVS"

c_invs  FDB     do_col                   ; : INVS
p_invs  FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_dnega                  ; DNEGATE
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_semi_                  ; (;)

; ==================================================================
; D/
; ==================================================================
l_ddiv  FDB     l_dgts                   ; LFA -> D>S

n_ddiv  FCB     $82                      ; NFA -> 2
        FCB     $44,$AF                  ; "D/"

c_ddiv  FDB     do_col                   ; : D/
p_ddiv  FDB     c_2dup                   ; 2DUP
        FDB     c_0                      ; 0
        FDB     c_0                      ; 0
        FDB     c_dlt                    ; D<
        FDB     c_to_r                   ; >R
        FDB     c_2dup                   ; 2DUP
        FDB     c_dabs                   ; DABS
        FDB     c_nip                    ; NIP
        FDB     c_qmbra,Z42DA            ; ?BRANCH --Z42DA--v
        FDB     c_rat                    ; R@
        FDB     c_qmbra,Z42B8            ; ?BRANCH --Z42B8--v
        FDB     c_invs                   ; INVS
        FDB     c_dnega                  ; DNEGATE
Z42B8   FDB     c_swap                   ; SWAP
        FDB     c_to_r                   ; >R
        FDB     c_divmo                  ; /MOD
        FDB     c_from_                  ; R>
        FDB     c_swap                   ; SWAP
        FDB     c_from_                  ; R>
        FDB     c_0                      ; 0
        FDB     c_rat                    ; R@
        FDB     c_dup                    ; DUP
        FDB     c_0lt                    ; 0<
        FDB     c_dast                   ; D*
        FDB     c_dsub                   ; D-
        FDB     c_from_                  ; R>
        FDB     c_dup                    ; DUP
        FDB     c_0lt                    ; 0<
        FDB     c_branc,Z42E8            ; BRANCH --Z42E8--v
Z42DA   FDB     c_rat                    ; R@
        FDB     c_qmbra,Z42E2            ; ?BRANCH --Z42E2--v
        FDB     c_invs                   ; INVS
Z42E2   FDB     c_drop                   ; DROP
        FDB     c_abs                    ; ABS
        FDB     c_dmdiv                  ; DM/MOD
Z42E8   FDB     c_from_                  ; R>
        FDB     c_qmbra,Z42F0            ; ?BRANCH --Z42F0--v
        FDB     c_invs                   ; INVS
Z42F0   FDB     c_semi_                  ; (;)

; ==================================================================
; DMOD
; ==================================================================
l_dmod  FDB     l_ddiv                   ; LFA -> D/

n_dmod  FCB     $84                      ; NFA -> 4
        FCB     $44,$4D,$4F,$C4          ; "DMOD"

c_dmod  FDB     do_col                   ; : DMOD
p_dmod  FDB     c_ddiv                   ; D/
        FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; D>
; ==================================================================
l_dgt   FDB     l_dmod                   ; LFA -> DMOD

n_dgt   FCB     $82                      ; NFA -> 2
        FCB     $44,$BE                  ; "D>"

c_dgt   FDB     do_col                   ; : D>
p_dgt   FDB     c_2swap                  ; 2SWAP
        FDB     c_dlt                    ; D<
        FDB     c_semi_                  ; (;)

; ==================================================================
; SQR
; ==================================================================
l_sqr   FDB     l_sgtd                   ; LFA -> S>D

n_sqr   FCB     $83                      ; NFA -> 3
        FCB     $53,$51,$D2              ; "SQR"

c_sqr   FDB     do_col                   ; : SQR
p_sqr   FDB     c_1,c_lit_               ; 1
        FDB     $000A                    ; 10
        FDB     c_0                      ; 0
        FDB     c_do_,Z432E              ; (DO) --Z432E--v
Z4322   FDB     c_2dup                   ; 2DUP
        FDB     c_div                    ; /
        FDB     c_add                    ; +
        FDB     c_2div                   ; 2/
        FDB     c_loop_,Z4322            ; (LOOP) --Z4322--^
Z432E   FDB     c_nip                    ; NIP
        FDB     c_semi_                  ; (;)

; ==================================================================
; DSQR
; ==================================================================
l_dsqr  FDB     l_dgt                    ; LFA -> D>

n_dsqr  FCB     $84                      ; NFA -> 4
        FCB     $44,$53,$51,$D2          ; "DSQR"

c_dsqr  FDB     do_col                   ; : DSQR
p_dsqr  FDB     c_lit_
        FDB     $0001,c_lit_             ; 1
        FDB     $0000,c_lit_             ; 0
        FDB     $0013                    ; 19
        FDB     c_0                      ; 0
        FDB     c_do_,Z435B              ; (DO) --Z435B--v
Z434D   FDB     c_2over                  ; 2OVER
        FDB     c_2over                  ; 2OVER
        FDB     c_ddiv                   ; D/
        FDB     c_dadd                   ; D+
        FDB     c_d2div                  ; D2/
        FDB     c_loop_,Z434D            ; (LOOP) --Z434D--^
Z435B   FDB     c_2swap                  ; 2SWAP
        FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; YCU
; ==================================================================
l_ycu   FDB     l_invs                   ; LFA -> INVS

n_ycu   FCB     $83                      ; NFA -> 3
        FCB     $59,$43,$D5              ; "YCU"

c_ycu   FDB     do_col                   ; : YCU
p_ycu   FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z4379,c_lit_     ; ?BRANCH --Z4379--v
        FDB     $0014                    ; 20
        FDB     c_tild                   ; ~
Z4379   FDB     c_semi_                  ; (;)

; ==================================================================
; NCU
; ==================================================================
l_ncu   FDB     l_dotq2                  ; LFA -> ."F

n_ncu   FCB     $83                      ; NFA -> 3
        FCB     $4E,$43,$D5              ; "NCU"

c_ncu   FDB     do_col                   ; : NCU
p_ncu   FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z4393,c_lit_     ; ?BRANCH --Z4393--v
        FDB     $0015                    ; 21
        FDB     c_tild                   ; ~
Z4393   FDB     c_semi_                  ; (;)

; ==================================================================
; (")
; ==================================================================
l_qt_   FDB     l_dsqr                   ; LFA -> DSQR

n_qt_   FCB     $83                      ; NFA -> 3
        FCB     $28,$22,$A9              ; '(\")'

c_qt_   FDB     do_col                   ; : (")
p_qt_   FDB     c_from_                  ; R>
        FDB     c_count                  ; COUNT
        FDB     c_2dup                   ; 2DUP
        FDB     c_add                    ; +
        FDB     c_to_r                   ; >R
        FDB     c_semi_                  ; (;)

; ==================================================================
; "
; ==================================================================
l_qt    FDB     l_ncu                    ; LFA -> NCU

n_qt    FCB     $C1                      ; NFA -> 1 IMMEDIATE
        FCB     $A2                      ; '"'

c_qt    FDB     do_col                   ; : "
p_qt    FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_qmbra,Z43C1            ; ?BRANCH --Z43C1--v
        FDB     c_compi                  ; COMPILE
        FDB     c_qt_                    ; (")
        FDB     c_komqt                  ; ,"
        FDB     c_branc,Z43DB            ; BRANCH --Z43DB--v
Z43C1   FDB     c_lit_
        FDB     $0016                    ; 22
        FDB     c_strin                  ; STRING
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_pad                    ; PAD
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_1sub                   ; 1-
Z43DB   FDB     c_semi_                  ; (;)

; ==================================================================
; TTYSET
; ==================================================================
l_ttyse FDB     l_qt_                    ; LFA -> (")

n_ttyse FCB     $86                      ; NFA -> 6
        FCB     $54,$54,$59,$53,$45,$D4  ; "TTYSET"

c_ttyse FDB     do_col                   ; : TTYSET
p_ttyse FDB     c_ncu                    ; NCU
        FDB     c_qt_                    ; (") len=6
        FCB     $06
        FCB     $54,$54,$59,$53,$45,$54  ; "TTYSET"
        FDB     c_to_do                  ; >DOS
        FDB     c_dos                    ; DOS
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; 0CAT
; ==================================================================
l_0cat  FDB     l_ttyse                  ; LFA -> TTYSET

n_0cat  FCB     $84                      ; NFA -> 4
        FCB     $30,$43,$41,$D4          ; "0CAT"

c_0cat  FDB     do_col                   ; : 0CAT
p_0cat  FDB     c_ncu                    ; NCU
        FDB     c_qt_                    ; (") len=3
        FCB     $03
        FCB     $43,$41,$54              ; "CAT"
        FDB     c_to_do                  ; >DOS
        FDB     c_dos                    ; DOS
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; 1CAT
; ==================================================================
l_1cat  FDB     l_ycu                    ; LFA -> YCU

n_1cat  FCB     $84                      ; NFA -> 4
        FCB     $31,$43,$41,$D4          ; "1CAT"

c_1cat  FDB     do_col                   ; : 1CAT
p_1cat  FDB     c_ncu                    ; NCU
        FDB     c_qt_                    ; (") len=5
        FCB     $05
        FCB     $43,$41,$54,$20,$31      ; "CAT 1"
        FDB     c_to_do                  ; >DOS
        FDB     c_dos                    ; DOS
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEB
; ==================================================================
l_deb   FDB     l_0cat                   ; LFA -> 0CAT

n_deb   FCB     $83                      ; NFA -> 3
        FCB     $44,$45,$C2              ; "DEB"

c_deb   FDB     do_col                   ; : DEB
p_deb   FDB     c_block                  ; BLOCK
        FDB     c_csubn                  ; C-NAME
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; BS
; ==================================================================
l_bs    FDB     l_qt                     ; LFA -> "

n_bs    FCB     $82                      ; NFA -> 2
        FCB     $42,$D3                  ; "BS"

c_bs    FDB     do_col                   ; : BS
p_bs    FDB     c_lit_
        FDB     $0008                    ; 8
        FDB     c_tild                   ; ~
        FDB     c_sub1                   ; -1
        FDB     c_out                    ; OUT
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; _BS
; ==================================================================
l_bs0   FDB     l_sqr                    ; LFA -> SQR

n_bs0   FCB     $83                      ; NFA -> 3
        FCB     $5F,$42,$D3              ; "_BS"

c_bs0   FDB     do_col                   ; : _BS
p_bs0   FDB     c_ncu                    ; NCU
        FDB     c_0                      ; 0
        FDB     c_do_,Z446A              ; (DO) --Z446A--v
Z4464   FDB     c_bs                     ; BS
        FDB     c_loop_,Z4464            ; (LOOP) --Z4464--^
Z446A   FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; L/E
; ==================================================================
l_ldive FDB     l_deb                    ; LFA -> DEB

n_ldive FCB     $83                      ; NFA -> 3
        FCB     $4C,$2F,$C5              ; "L/E"

c_ldive FDB     do_const                 ; CONSTANT L/E
p_ldive FDB     n_noop                   ; 24

; ==================================================================
; PAP
; ==================================================================
l_pap   FDB     l_ldive                  ; LFA -> L/E

n_pap   FCB     $83                      ; NFA -> 3
        FCB     $50,$41,$D0              ; "PAP"

c_pap   FDB     do_col                   ; : PAP
p_pap   FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_tild                   ; ~
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; P.
; ==================================================================
l_pdot  FDB     l_pap                    ; LFA -> PAP

n_pdot  FCB     $82                      ; NFA -> 2
        FCB     $50,$AE                  ; "P."

c_pdot  FDB     do_col                   ; : P.
p_pdot  FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z449F            ; ?BRANCH --Z449F--v
        FDB     c_drop                   ; DROP
        FDB     c_branc,Z44A7            ; BRANCH --Z44A7--v
Z449F   FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_tild                   ; ~
        FDB     c_tild                   ; ~
Z44A7   FDB     c_semi_                  ; (;)

; ==================================================================
; HOME
; ==================================================================
l_home  FDB     l_pdot                   ; LFA -> P.

n_home  FCB     $84                      ; NFA -> 4
        FCB     $48,$4F,$4D,$C5          ; "HOME"

c_home  FDB     do_col                   ; : HOME
p_home  FDB     c_cdiv0                  ; C/L0
        FDB     c_dup                    ; DUP
        FDB     c_1                      ; 1
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0006,c_lit_             ; 6
        FDB     $0005                    ; 5
        FDB     c_0                      ; 0
        FDB     c_do_,Z44CE              ; (DO) --Z44CE--v
Z44C8   FDB     c_tild                   ; ~
        FDB     c_loop_,Z44C8            ; (LOOP) --Z44C8--^
Z44CE   FDB     c_semi_                  ; (;)

; ==================================================================
; SSDUP
; ==================================================================
l_ssdup FDB     l_bs0                    ; LFA -> _BS

n_ssdup FCB     $85                      ; NFA -> 5
        FCB     $53,$53,$44,$55,$D0      ; "SSDUP"

c_ssdup FDB     do_col                   ; : SSDUP
p_ssdup FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_dup                    ; DUP
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_semi_                  ; (;)

; ==================================================================
; BMD
; ==================================================================
l_bmd   FDB     l_bs                     ; LFA -> BS

n_bmd   FCB     $83                      ; NFA -> 3
        FCB     $42,$4D,$C4              ; "BMD"

c_bmd   FDB     do_col                   ; : BMD
p_bmd   FDB     c_compi                  ; COMPILE
        FDB     c_qmbra,c_to_ma          ; ?BRANCH --' >MARK--^
        FDB     c_compi                  ; COMPILE
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; <OF
; ==================================================================
l_from5 FDB     l_home                   ; LFA -> HOME

n_from5 FCB     $C3                      ; NFA -> 3 IMMEDIATE
        FCB     $3C,$4F,$C6              ; "<OF"

c_from5 FDB     do_col                   ; : <OF
p_from5 FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_over                   ; OVER
        FDB     c_compi                  ; COMPILE
        FDB     c_gt                     ; >
        FDB     c_bmd,c_lit_             ; BMD
        FDB     $0005                    ; 5
        FDB     c_semi_                  ; (;)

; ==================================================================
; >OF
; ==================================================================
l_to_of FDB     l_bmd                    ; LFA -> BMD

n_to_of FCB     $C3                      ; NFA -> 3 IMMEDIATE
        FCB     $3E,$4F,$C6              ; ">OF"

c_to_of FDB     do_col                   ; : >OF
p_to_of FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_over                   ; OVER
        FDB     c_compi                  ; COMPILE
        FDB     c_lt                     ; <
        FDB     c_bmd,c_lit_             ; BMD
        FDB     $0005                    ; 5
        FDB     c_semi_                  ; (;)

; ==================================================================
; _OF
; ==================================================================
l_of0   FDB     l_ssdup                  ; LFA -> SSDUP

n_of0   FCB     $C3                      ; NFA -> 3 IMMEDIATE
        FCB     $5F,$4F,$C6              ; "_OF"

c_of0   FDB     do_col                   ; : _OF
p_of0   FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_qmpai                  ; ?PAIRS
        FDB     c_compi                  ; COMPILE
        FDB     c_ssdup                  ; SSDUP
        FDB     c_compi                  ; COMPILE
        FDB     c_eq_                    ; <=>
        FDB     c_bmd,c_lit_             ; BMD
        FDB     $0005                    ; 5
        FDB     c_semi_                  ; (;)

; ==================================================================
; <>
; ==================================================================
l_ltgt  FDB     l_from5                  ; LFA -> <OF

n_ltgt  FCB     $82                      ; NFA -> 2
        FCB     $3C,$BE                  ; "<>"

c_ltgt  FDB     do_col                   ; : <>
p_ltgt  FDB     c_eq                     ; =
        FDB     c_0eq                    ; 0=
        FDB     c_semi_                  ; (;)

; ==================================================================
; EE
; ==================================================================
l_ee    FDB     l_1cat                   ; LFA -> 1CAT

n_ee    FCB     $82                      ; NFA -> 2
        FCB     $45,$C5                  ; "EE"

c_ee    FDB     do_col                   ; : EE
p_ee    FDB     c_empty                  ; EMPTY-BUFFERS
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEC0
; ==================================================================
l_dec0  FDB     l_ltgt                   ; LFA -> <>

n_dec0  FCB     $83                      ; NFA -> 3
        FCB     $44,$45,$C3              ; "DEC"

c_dec0  FDB     do_col                   ; : DEC0
p_dec0  FDB     c_decim                  ; DECIMAL
        FDB     c_semi_                  ; (;)

; ==================================================================
; 0P
; ==================================================================
l_0p    FDB     l_dec0                   ; LFA -> DEC0

n_0p    FCB     $82                      ; NFA -> 2
        FCB     $30,$D0                  ; "0P"

c_0p    FDB     do_col                   ; : 0P
p_0p    FDB     c_0                      ; 0
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 5P
; ==================================================================
l_5p    FDB     l_ee                     ; LFA -> EE

n_5p    FCB     $82                      ; NFA -> 2
        FCB     $35,$D0                  ; "5P"

c_5p    FDB     do_col                   ; : 5P
p_5p    FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 10P
; ==================================================================
l_10p   FDB     l_5p                     ; LFA -> 5P

n_10p   FCB     $83                      ; NFA -> 3
        FCB     $31,$30,$D0              ; "10P"

c_10p   FDB     do_col                   ; : 10P
p_10p   FDB     c_lit_
        FDB     $000A                    ; 10
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 1P
; ==================================================================
l_1p    FDB     l_10p                    ; LFA -> 10P

n_1p    FCB     $82                      ; NFA -> 2
        FCB     $31,$D0                  ; "1P"

c_1p    FDB     do_col                   ; : 1P
p_1p    FDB     c_1                      ; 1
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 6P
; ==================================================================
l_6p    FDB     l_to_of                  ; LFA -> >OF

n_6p    FCB     $82                      ; NFA -> 2
        FCB     $36,$D0                  ; "6P"

c_6p    FDB     do_col                   ; : 6P
p_6p    FDB     c_lit_
        FDB     $0006                    ; 6
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 11P
; ==================================================================
l_11p   FDB     l_1p                     ; LFA -> 1P

n_11p   FCB     $83                      ; NFA -> 3
        FCB     $31,$31,$D0              ; "11P"

c_11p   FDB     do_col                   ; : 11P
p_11p   FDB     c_lit_
        FDB     $000B                    ; 11
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 2P
; ==================================================================
l_2p    FDB     l_6p                     ; LFA -> 6P

n_2p    FCB     $82                      ; NFA -> 2
        FCB     $32,$D0                  ; "2P"

c_2p    FDB     do_col                   ; : 2P
p_2p    FDB     c_2                      ; 2
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 7P
; ==================================================================
l_7p    FDB     l_of0                    ; LFA -> _OF

n_7p    FCB     $82                      ; NFA -> 2
        FCB     $37,$D0                  ; "7P"

c_7p    FDB     do_col                   ; : 7P
p_7p    FDB     c_lit_
        FDB     $0007                    ; 7
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 12P
; ==================================================================
l_12p   FDB     l_11p                    ; LFA -> 11P

n_12p   FCB     $83                      ; NFA -> 3
        FCB     $31,$32,$D0              ; "12P"

c_12p   FDB     do_col                   ; : 12P
p_12p   FDB     c_lit_
        FDB     $000C                    ; 12
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 3P
; ==================================================================
l_3p    FDB     l_7p                     ; LFA -> 7P

n_3p    FCB     $82                      ; NFA -> 2
        FCB     $33,$D0                  ; "3P"

c_3p    FDB     do_col                   ; : 3P
p_3p    FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 8P
; ==================================================================
l_8p    FDB     l_0p                     ; LFA -> 0P

n_8p    FCB     $82                      ; NFA -> 2
        FCB     $38,$D0                  ; "8P"

c_8p    FDB     do_col                   ; : 8P
p_8p    FDB     c_lit_
        FDB     $0008                    ; 8
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 13P
; ==================================================================
l_13p   FDB     l_12p                    ; LFA -> 12P

n_13p   FCB     $83                      ; NFA -> 3
        FCB     $31,$33,$D0              ; "13P"

c_13p   FDB     do_col                   ; : 13P
p_13p   FDB     c_lit_
        FDB     $000D                    ; 13
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 4P
; ==================================================================
l_4p    FDB     l_8p                     ; LFA -> 8P

n_4p    FCB     $82                      ; NFA -> 2
        FCB     $34,$D0                  ; "4P"

c_4p    FDB     do_col                   ; : 4P
p_4p    FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 9P
; ==================================================================
l_9p    FDB     l_13p                    ; LFA -> 13P

n_9p    FCB     $82                      ; NFA -> 2
        FCB     $39,$D0                  ; "9P"

c_9p    FDB     do_col                   ; : 9P
p_9p    FDB     c_lit_
        FDB     $0009                    ; 9
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; 14P
; ==================================================================
l_14p   FDB     l_9p                     ; LFA -> 9P

n_14p   FCB     $83                      ; NFA -> 3
        FCB     $31,$34,$D0              ; "14P"

c_14p   FDB     do_col                   ; : 14P
p_14p   FDB     c_lit_
        FDB     $000E                    ; 14
        FDB     c_pdot                   ; P.
        FDB     c_semi_                  ; (;)

; ==================================================================
; CEL
; ==================================================================
l_cel   FDB     l_3p                     ; LFA -> 3P

n_cel   FCB     $83                      ; NFA -> 3
        FCB     $43,$45,$CC              ; "CEL"

c_cel   FDB     do_col                   ; : CEL
p_cel   FDB     c_lit_
        FDB     $000E                    ; 14
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; BELL
; ==================================================================
l_bell  FDB     l_2p                     ; LFA -> 2P

n_bell  FCB     $84                      ; NFA -> 4
        FCB     $42,$45,$4C,$CC          ; "BELL"

c_bell  FDB     do_col                   ; : BELL
p_bell  FDB     c_lit_
        FDB     $0007                    ; 7
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; HT
; ==================================================================
l_ht    FDB     l_4p                     ; LFA -> 4P

n_ht    FCB     $82                      ; NFA -> 2
        FCB     $48,$D4                  ; "HT"

c_ht    FDB     do_col                   ; : HT
p_ht    FDB     c_lit_
        FDB     $0009                    ; 9
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; CES
; ==================================================================
l_ces   FDB     l_cel                    ; LFA -> CEL

n_ces   FCB     $83                      ; NFA -> 3
        FCB     $43,$45,$D3              ; "CES"

c_ces   FDB     do_col                   ; : CES
p_ces   FDB     c_lit_
        FDB     $000F                    ; 15
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; SCRO
; ==================================================================
l_scro  FDB     l_ces                    ; LFA -> CES

n_scro  FCB     $84                      ; NFA -> 4
        FCB     $53,$43,$52,$CF          ; "SCRO"

c_scro  FDB     do_col                   ; : SCRO
p_scro  FDB     c_lit_
        FDB     $0016                    ; 22
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; LF
; ==================================================================
l_lf    FDB     l_ht                     ; LFA -> HT

n_lf    FCB     $82                      ; NFA -> 2
        FCB     $4C,$C6                  ; "LF"

c_lf    FDB     do_col                   ; : LF
p_lf    FDB     c_lit_
        FDB     $000A                    ; 10
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; CLR0
; ==================================================================
l_clr0  FDB     l_scro                   ; LFA -> SCRO

n_clr0  FCB     $83                      ; NFA -> 3
        FCB     $43,$4C,$D2              ; "CLR"

c_clr0  FDB     do_col                   ; : CLR0
p_clr0  FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; SCPA
; ==================================================================
l_scpa  FDB     l_clr0                   ; LFA -> CLR0

n_scpa  FCB     $84                      ; NFA -> 4
        FCB     $53,$43,$50,$C1          ; "SCPA"

c_scpa  FDB     do_col                   ; : SCPA
p_scpa  FDB     c_lit_
        FDB     $0017                    ; 23
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; VT
; ==================================================================
l_vt    FDB     l_bell                   ; LFA -> BELL

n_vt    FCB     $82                      ; NFA -> 2
        FCB     $56,$D4                  ; "VT"

c_vt    FDB     do_col                   ; : VT
p_vt    FDB     c_lit_
        FDB     $000B                    ; 11
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; CLC
; ==================================================================
l_clc   FDB     l_scpa                   ; LFA -> SCPA

n_clc   FCB     $83                      ; NFA -> 3
        FCB     $43,$4C,$C3              ; "CLC"

c_clc   FDB     do_col                   ; : CLC
p_clc   FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z4700,c_lit_     ; ?BRANCH --Z4700--v
        FDB     $000C                    ; 12
        FDB     c_tild                   ; ~
Z4700   FDB     c_semi_                  ; (;)

; ==================================================================
; CLS
; ==================================================================
l_cls   FDB     l_clc                    ; LFA -> CLC

n_cls   FCB     $83                      ; NFA -> 3
        FCB     $43,$4C,$D3              ; "CLS"

c_cls   FDB     do_col                   ; : CLS
p_cls   FDB     c_home,c_lit_            ; HOME
        FDB     $0009                    ; 9
        FDB     c_pap                    ; PAP
        FDB     c_14p                    ; 14P
        FDB     c_clc                    ; CLC
        FDB     c_semi_                  ; (;)

; ==================================================================
; CLEAN
; ==================================================================
l_clean FDB     l_cls                    ; LFA -> CLS

n_clean FCB     $85                      ; NFA -> 5
        FCB     $43,$4C,$45,$41,$CE      ; "CLEAN"

c_clean FDB     do_col                   ; : CLEAN
p_clean FDB     c_home,c_lit_            ; HOME
        FDB     $000E                    ; 14
        FDB     c_pap                    ; PAP
        FDB     c_5p                     ; 5P
        FDB     c_clc                    ; CLC
        FDB     c_semi_                  ; (;)

; ==================================================================
; BOX
; ==================================================================
l_box   FDB     l_vt                     ; LFA -> VT

n_box   FCB     $83                      ; NFA -> 3
        FCB     $42,$4F,$D8              ; "BOX"

c_box   FDB     do_col                   ; : BOX
p_box   FDB     c_2swap,c_lit_           ; 2SWAP
        FDB     $0006                    ; 6
        FDB     c_tild,c_lit_            ; ~
        FDB     $0004                    ; 4
        FDB     c_0                      ; 0
        FDB     c_do_,Z4750              ; (DO) --Z4750--v
Z474A   FDB     c_tild                   ; ~
        FDB     c_loop_,Z474A            ; (LOOP) --Z474A--^
Z4750   FDB     c_semi_                  ; (;)

; ==================================================================
; AT
; ==================================================================
l_at0   FDB     l_14p                    ; LFA -> 14P

n_at0   FCB     $82                      ; NFA -> 2
        FCB     $41,$D4                  ; "AT"

c_at0   FDB     do_col                   ; : AT
p_at0   FDB     c_1                      ; 1
        FDB     c_tild                   ; ~
        FDB     c_tild                   ; ~
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; GR
; ==================================================================
l_gr    FDB     l_clean                  ; LFA -> CLEAN

n_gr    FCB     $82                      ; NFA -> 2
        FCB     $47,$D2                  ; "GR"

c_gr    FDB     p_gr                     ; ASSEMBLER
p_gr    SWI
        FCB     $21
        JMP     p_next

; ==================================================================
; TX
; ==================================================================
l_tx    FDB     l_lf                     ; LFA -> LF

n_tx    FCB     $82                      ; NFA -> 2
        FCB     $54,$D8                  ; "TX"

c_tx    FDB     p_tx                     ; ASSEMBLER
p_tx    SWI
        FCB     $22
        JMP     p_next

; ==================================================================
; EFF
; ==================================================================
l_eff   FDB     l_at0                    ; LFA -> AT

n_eff   FCB     $83                      ; NFA -> 3
        FCB     $45,$46,$C6              ; "EFF"

c_eff   FDB     p_eff                    ; ASSEMBLER
p_eff   SWI
        FCB     $23
        JMP     p_next

; ==================================================================
; XA
; ==================================================================
l_xa    FDB     l_tx                     ; LFA -> TX

n_xa    FCB     $82                      ; NFA -> 2
        FCB     $58,$C1                  ; "XA"

c_xa    FDB     do_var                   ; VARIABLE XA
p_xa    FDB     $000D                    ; 13

; ==================================================================
; YA
; ==================================================================
l_ya    FDB     l_eff                    ; LFA -> EFF

n_ya    FCB     $82                      ; NFA -> 2
        FCB     $59,$C1                  ; "YA"

c_ya    FDB     do_var                   ; VARIABLE YA
p_ya    FDB     $000A                    ; 10

; ==================================================================
; XZ
; ==================================================================
l_xz    FDB     l_xa                     ; LFA -> XA

n_xz    FCB     $82                      ; NFA -> 2
        FCB     $58,$DA                  ; "XZ"

c_xz    FDB     do_var                   ; VARIABLE XZ
p_xz    FDB     $0020                    ; 32

; ==================================================================
; YZ
; ==================================================================
l_yz    FDB     l_ya                     ; LFA -> YA

n_yz    FCB     $82                      ; NFA -> 2
        FCB     $59,$DA                  ; "YZ"

c_yz    FDB     do_var                   ; VARIABLE YZ
p_yz    FDB     $000A                    ; 10

; ==================================================================
; VPAD
; ==================================================================
l_vpad  FDB     l_box                    ; LFA -> BOX

n_vpad  FCB     $84                      ; NFA -> 4
        FCB     $56,$50,$41,$C4          ; "VPAD"

c_vpad  FDB     do_var                   ; VARIABLE VPAD
p_vpad  FDB     $0000                    ; 0

; ==================================================================
; WIN
; ==================================================================
l_win   FDB     l_gr                     ; LFA -> GR

n_win   FCB     $83                      ; NFA -> 3
        FCB     $57,$49,$CE              ; "WIN"

c_win   FDB     do_col                   ; : WIN
p_win   FDB     c_ncu                    ; NCU
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z47DA,c_lit_     ; ?BRANCH --Z47DA--v
        FDB     $0006                    ; 6
        FDB     c_0                      ; 0
        FDB     c_do_,Z47D6              ; (DO) --Z47D6--v
Z47D0   FDB     c_drop                   ; DROP
        FDB     c_loop_,Z47D0            ; (LOOP) --Z47D0--^
Z47D6   FDB     c_branc,Z481C            ; BRANCH --Z481C--v
Z47DA   FDB     c_pdot                   ; P.
        FDB     c_pap                    ; PAP
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r,c_lit_            ; >R
        FDB     $0028                    ; 40
        FDB     c_min                    ; MIN
        FDB     c_dup                    ; DUP
        FDB     c_xa                     ; XA
        FDB     c_exc                    ; !
        FDB     c_from_,c_lit_           ; R>
        FDB     $0019                    ; 25
        FDB     c_min                    ; MIN
        FDB     c_dup                    ; DUP
        FDB     c_ya                     ; YA
        FDB     c_exc                    ; !
        FDB     c_from_,c_lit_           ; R>
        FDB     $0028                    ; 40
        FDB     c_min                    ; MIN
        FDB     c_dup                    ; DUP
        FDB     c_xz                     ; XZ
        FDB     c_exc                    ; !
        FDB     c_from_,c_lit_           ; R>
        FDB     $0019                    ; 25
        FDB     c_min                    ; MIN
        FDB     c_dup                    ; DUP
        FDB     c_yz                     ; YZ
        FDB     c_exc                    ; !
        FDB     c_box                    ; BOX
Z481C   FDB     c_semi_                  ; (;)

; ==================================================================
; VISU
; ==================================================================
l_visu  FDB     l_vpad                   ; LFA -> VPAD

n_visu  FCB     $84                      ; NFA -> 4
        FCB     $56,$49,$53,$D5          ; "VISU"

c_visu  FDB     do_col                   ; : VISU
p_visu  FDB     c_0                      ; 0
        FDB     c_prnt                   ; PRNT
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_inpt                   ; INPT
        FDB     c_exc,c_lit_             ; !
        FDB     $1C3F,c_lit_             ; ' EMIT
        FDB     $3E4E                    ; ' ~
        FDB     c_chang                  ; CHANGEBODY
        FDB     c_semi_                  ; (;)

; ==================================================================
; TEMPO
; ==================================================================
l_tempo FDB     l_xz                     ; LFA -> XZ

n_tempo FCB     $85                      ; NFA -> 5
        FCB     $54,$45,$4D,$50,$CF      ; "TEMPO"

c_tempo FDB     do_col                   ; : TEMPO
p_tempo FDB     c_lit_
        FDB     $F000                    ; -4096
        FDB     c_cat                    ; C@
        FDB     c_2                      ; 2
        FDB     c_and                    ; AND
        FDB     c_qmbra,p_tempo          ; ?BRANCH --p_tempo--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; DATE'
; ==================================================================
l_datet FDB     l_tempo                  ; LFA -> TEMPO

n_datet FCB     $85                      ; NFA -> 5
        FCB     $44,$41,$54,$45,$A7      ; "DATE'"

c_datet FDB     p_datet                  ; ASSEMBLER
p_datet SWI
        ABX
        PSHU    X
        JMP     p_next

; ==================================================================
; 'DATE
; ==================================================================
l_tckda FDB     l_win                    ; LFA -> WIN

n_tckda FCB     $85                      ; NFA -> 5
        FCB     $27,$44,$41,$54,$C5      ; "'DATE"

c_tckda FDB     do_col                   ; : 'DATE
p_tckda FDB     c_datet,c_lit_           ; DATE'
        FDB     $0008                    ; 8
        FDB     c_type                   ; TYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; DATE
; ==================================================================
l_date  FDB     l_datet                  ; LFA -> DATE'

n_date  FCB     $84                      ; NFA -> 4
        FCB     $44,$41,$54,$C5          ; "DATE"

c_date  FDB     do_col                   ; : DATE
p_date  FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z489D            ; ?BRANCH --Z489D--v
        FDB     c_qt_                    ; (") len=6
        FCB     $06
        FCB     $50,$20,$44,$41,$54,$45  ; "P DATE"
        FDB     c_to_do                  ; >DOS
        FDB     c_branc,Z48A8            ; BRANCH --Z48A8--v
Z489D   FDB     c_ncu                    ; NCU
        FDB     c_qt_                    ; (") len=4
        FCB     $04
        FCB     $44,$41,$54,$45          ; "DATE"
        FDB     c_to_do                  ; >DOS
Z48A8   FDB     c_dos                    ; DOS
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; MOVE
; ==================================================================
l_move  FDB     l_yz                     ; LFA -> YZ

n_move  FCB     $84                      ; NFA -> 4
        FCB     $4D,$4F,$56,$C5          ; "MOVE"

c_move  FDB     p_move                   ; ASSEMBLER
p_move  PSHS    U,Y
        LDX     $02,U
        LDU     $04,U
        LDY     [$02,S]
        BMI     Z48CB
Z48C3   LDD     ,U++
        STD     ,X++
        LEAY    -$01,Y
        BNE     Z48C3
Z48CB   PULS    U,Y
        LEAU    $06,U
        JMP     p_next

; ==================================================================
; FOIS
; ==================================================================
l_fois  FDB     l_visu                   ; LFA -> VISU

n_fois  FCB     $84                      ; NFA -> 4
        FCB     $46,$4F,$49,$D3          ; "FOIS"

c_fois  FDB     do_const                 ; CONSTANT FOIS
p_fois  FDB     $555A                    ; 21850

; ==================================================================
; DELAI'
; ==================================================================
l_delai FDB     l_date                   ; LFA -> DATE

n_delai FCB     $86                      ; NFA -> 6
        FCB     $44,$45,$4C,$41,$49,$A7  ; "DELAI'"

c_delai FDB     do_col                   ; : DELAI'
p_delai FDB     c_fois                   ; FOIS
        FDB     c_0                      ; 0
        FDB     c_do_,Z48F3              ; (DO) --Z48F3--v
Z48EF   FDB     c_loop_,Z48EF            ; (LOOP) --Z48EF--v
Z48F3   FDB     c_semi_                  ; (;)

; ==================================================================
; DELAI
; ==================================================================
l_dela0 FDB     l_delai                  ; LFA -> DELAI'

n_dela0 FCB     $85                      ; NFA -> 5
        FCB     $44,$45,$4C,$41,$C9      ; "DELAI"

c_dela0 FDB     do_col                   ; : DELAI
p_dela0 FDB     c_0                      ; 0
        FDB     c_qmdo_,Z490B            ; (?DO) --Z490B--v
Z4905   FDB     c_delai                  ; DELAI'
        FDB     c_loop_,Z4905            ; (LOOP) --Z4905--^
Z490B   FDB     c_semi_                  ; (;)

; ==================================================================
; PILE
; ==================================================================
l_pile  FDB     l_dela0                  ; LFA -> DELAI

n_pile  FCB     $84                      ; NFA -> 4
        FCB     $50,$49,$4C,$C5          ; "PILE"

c_pile  FDB     do_col                   ; : PILE
p_pile  FDB     c_spexc,c_lit_           ; SP!
        FDB     $000B                    ; 11
        FDB     c_1                      ; 1
        FDB     c_do_,Z4928              ; (DO) --Z4928--v
Z4922   FDB     c_i                      ; I
        FDB     c_loop_,Z4922            ; (LOOP) --Z4922--^
Z4928   FDB     c_semi_                  ; (;)

; ==================================================================
; N:
; ==================================================================
l_ncol  FDB     l_fois                   ; LFA -> FOIS

n_ncol  FCB     $82                      ; NFA -> 2
        FCB     $4E,$BA                  ; "N:"

c_ncol  FDB     do_col                   ; : N:
p_ncol  FDB     c_dup                    ; DUP
        FDB     c_dot                    ; .
        FDB     c_semi_                  ; (;)

; ==================================================================
; U:
; ==================================================================
l_ucol  FDB     l_move                   ; LFA -> MOVE

n_ucol  FCB     $82                      ; NFA -> 2
        FCB     $55,$BA                  ; "U:"

c_ucol  FDB     do_col                   ; : U:
p_ucol  FDB     c_dup                    ; DUP
        FDB     c_dup,c_lit_             ; DUP
        FDB     $FFFF                    ; -1
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4952            ; ?BRANCH --Z4952--v
        FDB     c_dot                    ; .
        FDB     c_branc,Z4954            ; BRANCH --Z4954--v
Z4952   FDB     c_udot                   ; U.
Z4954   FDB     c_semi_                  ; (;)

; ==================================================================
; ??
; ==================================================================
l_qmqm  FDB     l_tckda                  ; LFA -> 'DATE

n_qmqm  FCB     $82                      ; NFA -> 2
        FCB     $3F,$BF                  ; "??"

c_qmqm  FDB     do_col                   ; : ??
p_qmqm  FDB     c_ncu                    ; NCU
        FDB     c_3p                     ; 3P
        FDB     c_cr                     ; CR
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_from_                  ; R>
        FDB     c_ncol                   ; N:
        FDB     c_6p,c_lit_              ; 6P
        FDB     $005F                    ; 95
        FDB     c_tild                   ; ~
        FDB     c_depth                  ; DEPTH
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=13
        FCB     $0D
        FDB     $5072,$6F66,$6F6E,$6465  ; "Profonde"
        FCB     $75,$72,$20,$3D,$20      ; "ur = "
        FDB     c_3p                     ; 3P
        FDB     c_dot                    ; .
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; //
; ==================================================================
l_divdi FDB     l_qmqm                   ; LFA -> ??

n_divdi FCB     $82                      ; NFA -> 2
        FCB     $2F,$AF                  ; "//"

c_divdi FDB     do_col                   ; : //
p_divdi FDB     c_ncu                    ; NCU
        FDB     c_3p                     ; 3P
        FDB     c_cr                     ; CR
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_from_                  ; R>
        FDB     c_ucol                   ; U:
        FDB     c_1p,c_lit_              ; 1P
        FDB     $005F                    ; 95
        FDB     c_tild                   ; ~
        FDB     c_cr                     ; CR
        FDB     c_6p                     ; 6P
        FDB     c_depth                  ; DEPTH
        FDB     c_dotq1                  ; (."F) len=13
        FCB     $0D
        FDB     $5072,$6F66,$6F6E,$6465  ; "Profonde"
        FCB     $75,$72,$20,$3D,$20      ; "ur = "
        FDB     c_3p                     ; 3P
        FDB     c_dot                    ; .
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; B!
; ==================================================================
l_bexc  FDB     l_ncol                   ; LFA -> N:

n_bexc  FCB     $82                      ; NFA -> 2
        FCB     $42,$A1                  ; "B!"

c_bexc  FDB     do_col                   ; : B!
p_bexc  FDB     c_swap                   ; SWAP
        FDB     c_space                  ; SPACE
        FDB     c_dot                    ; .
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; B@
; ==================================================================
l_bat   FDB     l_bexc                   ; LFA -> B!

n_bat   FCB     $82                      ; NFA -> 2
        FCB     $42,$C0                  ; "B@"

c_bat   FDB     do_col                   ; : B@
p_bat   FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_semi_                  ; (;)

; ==================================================================
; BIN
; ==================================================================
l_bin   FDB     l_bat                    ; LFA -> B@

n_bin   FCB     $83                      ; NFA -> 3
        FCB     $42,$49,$CE              ; "BIN"

c_bin   FDB     do_col                   ; : BIN
p_bin   FDB     c_2                      ; 2
        FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; .BIN
; ==================================================================
l_dotbi FDB     l_bin                    ; LFA -> BIN

n_dotbi FCB     $84                      ; NFA -> 4
        FCB     $2E,$42,$49,$CE          ; ".BIN"

c_dotbi FDB     do_col                   ; : .BIN
p_dotbi FDB     c_bat                    ; B@
        FDB     c_bin                    ; BIN
        FDB     c_bexc                   ; B!
        FDB     c_semi_                  ; (;)

; ==================================================================
; .DEC
; ==================================================================
l_dotde FDB     l_dotbi                  ; LFA -> .BIN

n_dotde FCB     $84                      ; NFA -> 4
        FCB     $2E,$44,$45,$C3          ; ".DEC"

c_dotde FDB     do_col                   ; : .DEC
p_dotde FDB     c_bat                    ; B@
        FDB     c_dec0                   ; DEC0
        FDB     c_bexc                   ; B!
        FDB     c_semi_                  ; (;)

; ==================================================================
; .HEX
; ==================================================================
l_dothe FDB     l_dotde                  ; LFA -> .DEC

n_dothe FCB     $84                      ; NFA -> 4
        FCB     $2E,$48,$45,$D8          ; ".HEX"

c_dothe FDB     do_col                   ; : .HEX
p_dothe FDB     c_bat                    ; B@
        FDB     c_hex                    ; HEX
        FDB     c_bexc                   ; B!
        FDB     c_semi_                  ; (;)

; ==================================================================
; ARR
; ==================================================================
l_arr   FDB     l_ucol                   ; LFA -> U:

n_arr   FCB     $83                      ; NFA -> 3
        FCB     $41,$52,$D2              ; "ARR"

c_arr   FDB     do_col                   ; : ARR
p_arr   FDB     c_bell,c_lit_            ; BELL
        FDB     $0011                    ; 17
        FDB     c_bs0                    ; _BS
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?ON
; ==================================================================
l_qmon  FDB     l_divdi                  ; LFA -> //

n_qmon  FCB     $83                      ; NFA -> 3
        FCB     $3F,$4F,$CE              ; "?ON"

c_qmon  FDB     do_col                   ; : ?ON
p_qmon  FDB     c_dup                    ; DUP
        FDB     c_to_r,c_lit_            ; >R
        FDB     $004E                    ; 78
        FDB     c_rat                    ; R@
        FDB     c_eq,c_lit_              ; =
        FDB     $004F                    ; 79
        FDB     c_rat                    ; R@
        FDB     c_eq,c_lit_              ; =
        FDB     $006E                    ; 110
        FDB     c_rat                    ; R@
        FDB     c_eq,c_lit_              ; =
        FDB     $006F                    ; 111
        FDB     c_from_                  ; R>
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_or                     ; OR
        FDB     c_or                     ; OR
        FDB     c_semi_                  ; (;)

; ==================================================================
; _ON
; ==================================================================
l_on    FDB     l_qmon                   ; LFA -> ?ON

n_on    FCB     $83                      ; NFA -> 3
        FCB     $5F,$4F,$CE              ; "_ON"

c_on    FDB     do_col                   ; : _ON
p_on    FDB     c_dotq1                  ; (."F) len=1
        FCB     $01
        FCB     $5B                      ; "["
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=1
        FCB     $01
        FCB     $4F                      ; "O"
        FDB     c_11p                    ; 11P
        FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $5D,$75,$69,$20,$6F,$75  ; "]ui ou"
        FCB     $20                      ; " "
        FDB     c_dotq1                  ; (."F) len=1
        FCB     $01
        FCB     $5B                      ; "["
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=1
        FCB     $01
        FCB     $4E                      ; "N"
        FDB     c_11p                    ; 11P
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $5D,$6F,$6E,$20,$3F,$20  ; "]on ? "
        FDB     c_semi_                  ; (;)

; ==================================================================
; O/N
; ==================================================================
l_odivn FDB     l_on                     ; LFA -> _ON

n_odivn FCB     $83                      ; NFA -> 3
        FCB     $4F,$2F,$CE              ; "O/N"

c_odivn FDB     do_col                   ; : O/N
p_odivn FDB     c_termi                  ; TERMINAL
        FDB     c_ncu                    ; NCU
        FDB     c_11p                    ; 11P
        FDB     c_on                     ; _ON
        FDB     c_key                    ; KEY
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0020                    ; 32
        FDB     c_gt                     ; >
        FDB     c_dup                    ; DUP
        FDB     c_qmbra,Z4BAE            ; ?BRANCH --Z4BAE--v
        FDB     c_over                   ; OVER
        FDB     c_3p                     ; 3P
        FDB     c_tild                   ; ~
        FDB     c_14p                    ; 14P
        FDB     c_swap                   ; SWAP
        FDB     c_qmon                   ; ?ON
        FDB     c_rot                    ; ROT
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z4BA6,c_lit_     ; ?BRANCH --Z4BA6--v
        FDB     $004E                    ; 78
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4B58            ; ?BRANCH --Z4B58--v
        FDB     c_drop                   ; DROP
        FDB     c_0                      ; 0
        FDB     c_ycu                    ; YCU
        FDB     c_exit                   ; EXIT
        FDB     c_branc,Z4BA2            ; BRANCH --Z4BA2--v
Z4B58   FDB     c_lit_
        FDB     $004F                    ; 79
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4B70            ; ?BRANCH --Z4B70--v
        FDB     c_drop                   ; DROP
        FDB     c_sub1                   ; -1
        FDB     c_ycu                    ; YCU
        FDB     c_exit                   ; EXIT
        FDB     c_branc,Z4BA2            ; BRANCH --Z4BA2--v
Z4B70   FDB     c_lit_
        FDB     $006E                    ; 110
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4B88            ; ?BRANCH --Z4B88--v
        FDB     c_drop                   ; DROP
        FDB     c_0                      ; 0
        FDB     c_ycu                    ; YCU
        FDB     c_exit                   ; EXIT
        FDB     c_branc,Z4BA2            ; BRANCH --Z4BA2--v
Z4B88   FDB     c_lit_
        FDB     $006F                    ; 111
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z4BA0            ; ?BRANCH --Z4BA0--v
        FDB     c_drop                   ; DROP
        FDB     c_sub1                   ; -1
        FDB     c_ycu                    ; YCU
        FDB     c_exit                   ; EXIT
        FDB     c_branc,Z4BA2            ; BRANCH --Z4BA2--v
Z4BA0   FDB     c_drop                   ; DROP
Z4BA2   FDB     c_branc,Z4BAA            ; BRANCH --Z4BAA--v
Z4BA6   FDB     c_drop                   ; DROP
        FDB     c_bs                     ; BS
Z4BAA   FDB     c_branc,Z4BB0            ; BRANCH --Z4BB0--v
Z4BAE   FDB     c_2drop                  ; 2DROP
Z4BB0   FDB     c_arr                    ; ARR
        FDB     c_branc,p_odivn          ; BRANCH --p_odivn--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; Y/N
; ==================================================================
l_ydivn FDB     l_arr                    ; LFA -> ARR

n_ydivn FCB     $83                      ; NFA -> 3
        FCB     $59,$2F,$CE              ; "Y/N"

c_ydivn FDB     do_col                   ; : Y/N
p_ydivn FDB     c_cr,c_lit_              ; CR
        FDB     $0005                    ; 5
        FDB     c_sp                     ; SP
        FDB     c_odivn                  ; O/N
        FDB     c_semi_                  ; (;)

; ==================================================================
; KECAR
; ==================================================================
l_kecar FDB     l_odivn                  ; LFA -> O/N

n_kecar FCB     $85                      ; NFA -> 5
        FCB     $4B,$45,$43,$41,$D2      ; "KECAR"

c_kecar FDB     p_kecar                  ; ASSEMBLER
p_kecar PSHS    Y
        PULU    Y
        PULU    X
Z4BDC   LDB     ,X
        CMPB    #$7F
        BNE     Z4BE6
        LDB     #$20
        BRA     Z4BF4
Z4BE6   CMPB    #$20
        BCC     Z4BEE
        LDB     #$20
        BRA     Z4BF4
Z4BEE   CMPB    #$94
        BLS     Z4BF4
        LDB     #$20
Z4BF4   STB     ,X+
        LEAY    -$01,Y
        BNE     Z4BDC
        PULS    Y
        JMP     p_next

; ==================================================================
; CHAINE
; ==================================================================
l_chain FDB     l_kecar                  ; LFA -> KECAR

n_chain FCB     $86                      ; NFA -> 6
        FCB     $43,$48,$41,$49,$4E,$C5  ; "CHAINE"

c_chain FDB     p_chain                  ; ASSEMBLER
p_chain PSHS    Y
        PULU    Y
        PULU    X
Z4C0F   LDA     ,X+
        SWI
        FCB     $01
        LEAY    -$01,Y
        BNE     Z4C0F
        PULS    Y
        JMP     p_next

; ==================================================================
; CROSS
; ==================================================================
l_cross FDB     l_chain                  ; LFA -> CHAINE

n_cross FCB     $85                      ; NFA -> 5
        FCB     $43,$52,$4F,$53,$D3      ; "CROSS"

c_cross FDB     p_cross                  ; ASSEMBLER
p_cross PSHS    Y
        PULU    Y,X,D
        PSHS    D
        PULU    D
        PSHU    X,D
        PULS    X
        PSHU    X
        PSHU    Y
        PULS    Y
        JMP     p_next

; ==================================================================
; ?AT
; ==================================================================
l_qmat  FDB     l_cross                  ; LFA -> CROSS

n_qmat  FCB     $83                      ; NFA -> 3
        FCB     $3F,$41,$D4              ; "?AT"

c_qmat  FDB     do_col                   ; : ?AT
p_qmat  FDB     c_2                      ; 2
        FDB     c_tild                   ; ~
        FDB     c_key,c_lit_             ; KEY
        FDB     $0040                    ; 64
        FDB     c_sub                    ; -
        FDB     c_key,c_lit_             ; KEY
        FDB     $0040                    ; 64
        FDB     c_sub                    ; -
        FDB     c_swap                   ; SWAP
        FDB     c_semi_                  ; (;)

; ==================================================================
; ELD
; ==================================================================
l_eld   FDB     l_ydivn                  ; LFA -> Y/N

n_eld   FCB     $83                      ; NFA -> 3
        FCB     $45,$4C,$C4              ; "ELD"

c_eld   FDB     do_col                   ; : ELD
p_eld   FDB     c_ncu                    ; NCU
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_load                   ; LOAD
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; SITE
; ==================================================================
l_site  FDB     l_qmat                   ; LFA -> ?AT

n_site  FCB     $84                      ; NFA -> 4
        FCB     $53,$49,$54,$C5          ; "SITE"

c_site  FDB     do_var                   ; VARIABLE SITE
p_site  FDB     $BAB7                    ; 47799

; ==================================================================
; CUR
; ==================================================================
l_cur   FDB     l_site                   ; LFA -> SITE

n_cur   FCB     $83                      ; NFA -> 3
        FCB     $43,$55,$D2              ; "CUR"

c_cur   FDB     do_var                   ; VARIABLE CUR
p_cur   FDB     $BA95                    ; 47765

; ==================================================================
; VEC
; ==================================================================
l_vec   FDB     l_dothe                  ; LFA -> .HEX

n_vec   FCB     $83                      ; NFA -> 3
        FCB     $56,$45,$C3              ; "VEC"

c_vec   FDB     do_var                   ; VARIABLE VEC
p_vec   FDB     $5E17                    ; 24087

; ==================================================================
; VEC1
; ==================================================================
l_vec1  FDB     l_vec                    ; LFA -> VEC

n_vec1  FCB     $84                      ; NFA -> 4
        FCB     $56,$45,$43,$B1          ; "VEC1"

c_vec1  FDB     do_var                   ; VARIABLE VEC1
p_vec1  FDB     $5E08                    ; 24072

; ==================================================================
; VEC2
; ==================================================================
l_vec2  FDB     l_vec1                   ; LFA -> VEC1

n_vec2  FCB     $84                      ; NFA -> 4
        FCB     $56,$45,$43,$B2          ; "VEC2"

c_vec2  FDB     do_var                   ; VARIABLE VEC2
p_vec2  FDB     $5E17                    ; 24087

; ==================================================================
; VEC3
; ==================================================================
l_vec3  FDB     l_vec2                   ; LFA -> VEC2

n_vec3  FCB     $84                      ; NFA -> 4
        FCB     $56,$45,$43,$B3          ; "VEC3"

c_vec3  FDB     do_var                   ; VARIABLE VEC3
p_vec3  FDB     $5D59                    ; 23897

; ==================================================================
; `LP
; ==================================================================
l_bqlp  FDB     l_pile                   ; LFA -> PILE

n_bqlp  FCB     $83                      ; NFA -> 3
        FCB     $60,$4C,$D0              ; "`LP"

c_bqlp  FDB     do_var                   ; VARIABLE `LP
p_bqlp  FDB     $80E6                    ; 32998

; ==================================================================
; `LP2
; ==================================================================
l_bqlp2 FDB     l_bqlp                   ; LFA -> `LP

n_bqlp2 FCB     $84                      ; NFA -> 4
        FCB     $60,$4C,$50,$B2          ; "`LP2"

c_bqlp2 FDB     do_var                   ; VARIABLE `LP2
p_bqlp2 FDB     $810F                    ; 33039

; ==================================================================
; `LP3
; ==================================================================
l_bqlp3 FDB     l_bqlp2                  ; LFA -> `LP2

n_bqlp3 FCB     $84                      ; NFA -> 4
        FCB     $60,$4C,$50,$B3          ; "`LP3"

c_bqlp3 FDB     do_var                   ; VARIABLE `LP3
p_bqlp3 FDB     $8138                    ; 33080

; ==================================================================
; COM0
; ==================================================================
l_com0  FDB     l_cur                    ; LFA -> CUR

n_com0  FCB     $83                      ; NFA -> 3
        FCB     $43,$4F,$CD              ; "COM"

c_com0  FDB     do_var                   ; VARIABLE COM0
p_com0  FDB     $003E                    ; 62

; ==================================================================
; `EDPAD
; ==================================================================
l_bqedp FDB     l_bqlp3                  ; LFA -> `LP3

n_bqedp FCB     $86                      ; NFA -> 6
        FCB     $60,$45,$44,$50,$41,$C4  ; "`EDPAD"

c_bqedp FDB     do_var                   ; VARIABLE `EDPAD
p_bqedp FDB     $8161                    ; 33121

; ==================================================================
; `FIN
; ==================================================================
l_bqfin FDB     l_bqedp                  ; LFA -> `EDPAD

n_bqfin FCB     $84                      ; NFA -> 4
        FCB     $60,$46,$49,$CE          ; "`FIN"

c_bqfin FDB     do_var                   ; VARIABLE `FIN
p_bqfin FDB     $0001                    ; 1

; ==================================================================
; `VAL
; ==================================================================
l_bqval FDB     l_bqfin                  ; LFA -> `FIN

n_bqval FCB     $84                      ; NFA -> 4
        FCB     $60,$56,$41,$CC          ; "`VAL"

c_bqval FDB     do_var                   ; VARIABLE `VAL
p_bqval FDB     $FFFF                    ; 65535

; ==================================================================
; VBDAT
; ==================================================================
l_vbdat FDB     l_vec3                   ; LFA -> VEC3

n_vbdat FCB     $85                      ; NFA -> 5
        FCB     $56,$42,$44,$41,$D4      ; "VBDAT"

c_vbdat FDB     do_var                   ; VARIABLE VBDAT
p_vbdat FDB     $6DC9                    ; 28105

; ==================================================================
; DISPEC
; ==================================================================
l_dispe FDB     l_bqval                  ; LFA -> `VAL

n_dispe FCB     $86                      ; NFA -> 6
        FCB     $44,$49,$53,$50,$45,$C3  ; "DISPEC"

c_dispe FDB     do_var                   ; VARIABLE DISPEC
p_dispe FDB     $0000                    ; 0

; ==================================================================
; 94~
; ==================================================================
l_94til FDB     l_eld                    ; LFA -> ELD

n_94til FCB     $83                      ; NFA -> 3
        FCB     $39,$34,$FE              ; "94~"

c_94til FDB     do_col                   ; : 94~
p_94til FDB     c_lit_
        FDB     $005E                    ; 94
        FDB     c_emit                   ; EMIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; LP
; ==================================================================
l_lp1   FDB     l_dispe                  ; LFA -> DISPEC

n_lp1   FCB     $82                      ; NFA -> 2
        FCB     $4C,$D0                  ; "LP"

c_lp1   FDB     do_col                   ; : LP
p_lp1   FDB     c_bqlp                   ; `LP
        FDB     c_at                     ; @
        FDB     c_semi_                  ; (;)

; ==================================================================
; LP2
; ==================================================================
l_lp2   FDB     l_lp1                    ; LFA -> LP

n_lp2   FCB     $83                      ; NFA -> 3
        FCB     $4C,$50,$B2              ; "LP2"

c_lp2   FDB     do_col                   ; : LP2
p_lp2   FDB     c_bqlp2                  ; `LP2
        FDB     c_at                     ; @
        FDB     c_semi_                  ; (;)

; ==================================================================
; LP3
; ==================================================================
l_lp3   FDB     l_lp2                    ; LFA -> LP2

n_lp3   FCB     $83                      ; NFA -> 3
        FCB     $4C,$50,$B3              ; "LP3"

c_lp3   FDB     do_col                   ; : LP3
p_lp3   FDB     c_bqlp3                  ; `LP3
        FDB     c_at                     ; @
        FDB     c_semi_                  ; (;)

; ==================================================================
; EDPAD
; ==================================================================
l_edpad FDB     l_94til                  ; LFA -> 94~

n_edpad FCB     $85                      ; NFA -> 5
        FCB     $45,$44,$50,$41,$C4      ; "EDPAD"

c_edpad FDB     do_col                   ; : EDPAD
p_edpad FDB     c_bqedp                  ; `EDPAD
        FDB     c_at                     ; @
        FDB     c_semi_                  ; (;)

; ==================================================================
; MANKMEM
; ==================================================================
l_mankm FDB     l_edpad                  ; LFA -> EDPAD

n_mankm FCB     $87                      ; NFA -> 7
        FCB     $4D,$41,$4E,$4B,$4D,$45  ; "MANKME"
        FCB     $CD                      ; "M"

c_mankm FDB     do_col                   ; : MANKMEM
p_mankm FDB     c_dotq1                  ; (."F) len=22
        FCB     $16
        FDB     $4D80,$6D6F,$6972,$6520  ; "Mémoire "
        FDB     $696E,$7375,$6666,$6973  ; "insuffis"
        FCB     $61,$6E,$74,$65,$20,$21  ; "ante !"
        FDB     c_semi_                  ; (;)

; ==================================================================
; CURAD
; ==================================================================
l_curad FDB     l_com0                   ; LFA -> COM0

n_curad FCB     $85                      ; NFA -> 5
        FCB     $43,$55,$52,$41,$C4      ; "CURAD"

c_curad FDB     do_col                   ; : CURAD
p_curad FDB     c_site                   ; SITE
        FDB     c_at                     ; @
        FDB     c_semi_                  ; (;)

; ==================================================================
; ECRAD
; ==================================================================
l_ecrad FDB     l_mankm                  ; LFA -> MANKMEM

n_ecrad FCB     $85                      ; NFA -> 5
        FCB     $45,$43,$52,$41,$C4      ; "ECRAD"

c_ecrad FDB     do_col                   ; : ECRAD
p_ecrad FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_block                  ; BLOCK
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEBAD
; ==================================================================
l_debad FDB     l_lp3                    ; LFA -> LP3

n_debad FCB     $85                      ; NFA -> 5
        FCB     $44,$45,$42,$41,$C4      ; "DEBAD"

c_debad FDB     do_col                   ; : DEBAD
p_debad FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_deb                    ; DEB
        FDB     c_semi_                  ; (;)

; ==================================================================
; LIMAD
; ==================================================================
l_limad FDB     l_debad                  ; LFA -> DEBAD

n_limad FCB     $85                      ; NFA -> 5
        FCB     $4C,$49,$4D,$41,$C4      ; "LIMAD"

c_limad FDB     do_col                   ; : LIMAD
p_limad FDB     c_debad                  ; DEBAD
        FDB     c_nbcar                  ; NBCAR
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; RASEDPAD
; ==================================================================
l_rased FDB     l_vbdat                  ; LFA -> VBDAT

n_rased FCB     $88                      ; NFA -> 8
        FDB     $5241,$5345,$4450,$41C4  ; "RASEDPAD"

c_rased FDB     do_col                   ; : RASEDPAD
p_rased FDB     c_nbcar                  ; NBCAR
        FDB     c_edpad                  ; EDPAD
        FDB     c_2dup                   ; 2DUP
        FDB     c_2add                   ; 2+
        FDB     c_swap                   ; SWAP
        FDB     c_blank                  ; BLANK
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; MEM
; ==================================================================
l_mem   FDB     l_ecrad                  ; LFA -> ECRAD

n_mem   FCB     $83                      ; NFA -> 3
        FCB     $4D,$45,$CD              ; "MEM"

c_mem   FDB     do_col                   ; : MEM
p_mem   FDB     c_site                   ; SITE
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; ADLICU
; ==================================================================
l_adlic FDB     l_mem                    ; LFA -> MEM

n_adlic FCB     $86                      ; NFA -> 6
        FCB     $41,$44,$4C,$49,$43,$D5  ; "ADLICU"

c_adlic FDB     do_col                   ; : ADLICU
p_adlic FDB     c_curad                  ; CURAD
        FDB     c_dup                    ; DUP
        FDB     c_debad                  ; DEBAD
        FDB     c_sub                    ; -
        FDB     c_cdiv0                  ; C/L0
        FDB     c_mod                    ; MOD
        FDB     c_sub                    ; -
        FDB     c_semi_                  ; (;)

; ==================================================================
; CAR?
; ==================================================================
l_is_ca FDB     l_curad                  ; LFA -> CURAD

n_is_ca FCB     $84                      ; NFA -> 4
        FCB     $43,$41,$52,$BF          ; "CAR?"

c_is_ca FDB     do_col                   ; : CAR?
p_is_ca FDB     c_dup,c_lit_             ; DUP
        FDB     $0020,c_lit_             ; 32
        FDB     $007E                    ; 126
        FDB     c_eq_                    ; <=>
        FDB     c_swap,c_lit_            ; SWAP
        FDB     $0080,c_lit_             ; 128
        FDB     $0093                    ; 147
        FDB     c_eq_                    ; <=>
        FDB     c_or                     ; OR
        FDB     c_semi_                  ; (;)

; ==================================================================
; BOTM
; ==================================================================
l_botm  FDB     l_rased                  ; LFA -> RASEDPAD

n_botm  FCB     $84                      ; NFA -> 4
        FCB     $42,$4F,$54,$CD          ; "BOTM"

c_botm  FDB     do_col                   ; : BOTM
p_botm  FDB     c_limad                  ; LIMAD
        FDB     c_cdiv0                  ; C/L0
        FDB     c_sub                    ; -
        FDB     c_mem                    ; MEM
        FDB     c_1,c_lit_               ; 1
        FDB     $0019                    ; 25
        FDB     c_at0                    ; AT
        FDB     c_semi_                  ; (;)

; ==================================================================
; CUR!
; ==================================================================
l_curex FDB     l_is_ca                  ; LFA -> CAR?

n_curex FCB     $84                      ; NFA -> 4
        FCB     $43,$55,$52,$A1          ; "CUR!"

c_curex FDB     do_col                   ; : CUR!
p_curex FDB     c_adlic                  ; ADLICU
        FDB     c_cur                    ; CUR
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; XY?
; ==================================================================
l_is_xy FDB     l_limad                  ; LFA -> LIMAD

n_is_xy FCB     $83                      ; NFA -> 3
        FCB     $58,$59,$BF              ; "XY?"

c_is_xy FDB     do_col                   ; : XY?
p_is_xy FDB     c_curad                  ; CURAD
        FDB     c_debad                  ; DEBAD
        FDB     c_sub                    ; -
        FDB     c_cdiv0                  ; C/L0
        FDB     c_divmo                  ; /MOD
        FDB     c_swap                   ; SWAP
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_2add                   ; 2+
        FDB     c_semi_                  ; (;)

; ==================================================================
; SORT
; ==================================================================
l_sort  FDB     l_curex                  ; LFA -> CUR!

n_sort  FCB     $84                      ; NFA -> 4
        FCB     $53,$4F,$52,$D4          ; "SORT"

c_sort  FDB     do_col                   ; : SORT
p_sort  FDB     c_botm                   ; BOTM
        FDB     c_exit                   ; EXIT
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; LI>PA2
; ==================================================================
l_ligtp FDB     l_is_xy                  ; LFA -> XY?

n_ligtp FCB     $86                      ; NFA -> 6
        FCB     $4C,$49,$3E,$50,$41,$B2  ; "LI>PA2"

c_ligtp FDB     do_col                   ; : LI>PA2
p_ligtp FDB     c_cdiv0                  ; C/L0
        FDB     c_lp2                    ; LP2
        FDB     c_2dup                   ; 2DUP
        FDB     c_cexc                   ; C!
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_2dup                   ; 2DUP
        FDB     c_blank                  ; BLANK
        FDB     c_adlic                  ; ADLICU
        FDB     c_subro                  ; -ROT
        FDB     c_cmove                  ; CMOVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; NAME=
; ==================================================================
l_namee FDB     l_botm                   ; LFA -> BOTM

n_namee FCB     $85                      ; NFA -> 5
        FCB     $4E,$41,$4D,$45,$BD      ; "NAME="

c_namee FDB     do_col                   ; : NAME=
p_namee FDB     c_lit_
        FDB     $000E                    ; 14
        FDB     c_1                      ; 1
        FDB     c_at0,c_lit_             ; AT
        FDB     $000E                    ; 14
        FDB     c_pap                    ; PAP
        FDB     c_cel                    ; CEL
        FDB     c_5p                     ; 5P
        FDB     c_ecrad                  ; ECRAD
        FDB     c_csubn                  ; C-NAME
        FDB     c_2dup                   ; 2DUP
        FDB     c_blank                  ; BLANK
        FDB     c_expec                  ; EXPECT
        FDB     c_1                      ; 1
        FDB     c_2                      ; 2
        FDB     c_at0,c_lit_             ; AT
        FDB     $0009                    ; 9
        FDB     c_pap                    ; PAP
        FDB     c_11p                    ; 11P
        FDB     c_ycu                    ; YCU
        FDB     c_debad                  ; DEBAD
        FDB     c_mem                    ; MEM
        FDB     c_semi_                  ; (;)

; ==================================================================
; <DEBLI
; ==================================================================
l_from6 FDB     l_ligtp                  ; LFA -> LI>PA2

n_from6 FCB     $86                      ; NFA -> 6
        FCB     $3C,$44,$45,$42,$4C,$C9  ; "<DEBLI"

c_from6 FDB     do_col                   ; : <DEBLI
p_from6 FDB     c_lit_
        FDB     $000D                    ; 13
        FDB     c_tild                   ; ~
        FDB     c_adlic                  ; ADLICU
        FDB     c_mem                    ; MEM
        FDB     c_semi_                  ; (;)

; ==================================================================
; ENDLI
; ==================================================================
l_endli FDB     l_adlic                  ; LFA -> ADLICU

n_endli FCB     $85                      ; NFA -> 5
        FCB     $45,$4E,$44,$4C,$C9      ; "ENDLI"

c_endli FDB     do_col                   ; : ENDLI
p_endli FDB     c_adlic                  ; ADLICU
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_semi_                  ; (;)

; ==================================================================
; LASTLI
; ==================================================================
l_lastl FDB     l_from6                  ; LFA -> <DEBLI

n_lastl FCB     $86                      ; NFA -> 6
        FCB     $4C,$41,$53,$54,$4C,$C9  ; "LASTLI"

c_lastl FDB     do_col                   ; : LASTLI
p_lastl FDB     c_limad                  ; LIMAD
        FDB     c_cdiv0                  ; C/L0
        FDB     c_sub                    ; -
        FDB     c_semi_                  ; (;)

; ==================================================================
; LASTLI?
; ==================================================================
l_is_la FDB     l_lastl                  ; LFA -> LASTLI

n_is_la FCB     $87                      ; NFA -> 7
        FCB     $4C,$41,$53,$54,$4C,$49  ; "LASTLI"
        FCB     $BF                      ; "?"

c_is_la FDB     do_col                   ; : LASTLI?
p_is_la FDB     c_adlic                  ; ADLICU
        FDB     c_lastl                  ; LASTLI
        FDB     c_eq                     ; =
        FDB     c_semi_                  ; (;)

; ==================================================================
; RECPAD
; ==================================================================
l_recpa FDB     l_namee                  ; LFA -> NAME=

n_recpa FCB     $86                      ; NFA -> 6
        FCB     $52,$45,$43,$50,$41,$C4  ; "RECPAD"

c_recpa FDB     do_col                   ; : RECPAD
p_recpa FDB     c_lp1                    ; LP
        FDB     c_0                      ; 0
        FDB     c_over                   ; OVER
        FDB     c_cexc                   ; C!
        FDB     c_1add                   ; 1+
        FDB     c_cdiv0                  ; C/L0
        FDB     c_blank                  ; BLANK
        FDB     c_semi_                  ; (;)

; ==================================================================
; SORT?
; ==================================================================
l_is_so FDB     l_sort                   ; LFA -> SORT

n_is_so FCB     $85                      ; NFA -> 5
        FCB     $53,$4F,$52,$54,$BF      ; "SORT?"

c_is_so FDB     do_col                   ; : SORT?
p_is_so FDB     c_curad                  ; CURAD
        FDB     c_add                    ; +
        FDB     c_dup                    ; DUP
        FDB     c_debad                  ; DEBAD
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z4F75            ; ?BRANCH --Z4F75--v
        FDB     c_bell                   ; BELL
        FDB     c_nbcar                  ; NBCAR
        FDB     c_add                    ; +
Z4F75   FDB     c_dup                    ; DUP
        FDB     c_limad                  ; LIMAD
        FDB     c_to_eq                  ; >=
        FDB     c_qmbra,Z4F85            ; ?BRANCH --Z4F85--v
        FDB     c_bell                   ; BELL
        FDB     c_nbcar                  ; NBCAR
        FDB     c_sub                    ; -
Z4F85   FDB     c_mem                    ; MEM
        FDB     c_semi_                  ; (;)

; ==================================================================
; ENT
; ==================================================================
l_ent   FDB     l_endli                  ; LFA -> ENDLI

n_ent   FCB     $83                      ; NFA -> 3
        FCB     $45,$4E,$D4              ; "ENT"

c_ent   FDB     do_col                   ; : ENT
p_ent   FDB     c_ncu                    ; NCU
        FDB     c_1                      ; 1
        FDB     c_1                      ; 1
        FDB     c_at0,c_lit_             ; AT
        FDB     $000E                    ; 14
        FDB     c_pap                    ; PAP
        FDB     c_clr0                   ; CLR0
        FDB     c_12p                    ; 12P
        FDB     c_dotq1                  ; (."F) len=8
        FCB     $08
        FDB     $4563,$7261,$6E20,$4E88  ; "Ecran N°"
        FDB     c_3p                     ; 3P
        FDB     c_scr                    ; SCR
        FDB     c_at,c_lit_              ; @
        FDB     $0004                    ; 4
        FDB     c_dotr                   ; .R
        FDB     c_5p                     ; 5P
        FDB     c_space                  ; SPACE
        FDB     c_debad                  ; DEBAD
        FDB     c_dup                    ; DUP
        FDB     c_mem                    ; MEM
        FDB     c_ecrad                  ; ECRAD
        FDB     c_do_,Z4FEA              ; (DO) --Z4FEA--v
Z4FCA   FDB     c_i                      ; I
        FDB     c_cat                    ; C@
        FDB     c_dup                    ; DUP
        FDB     c_is_ca                  ; CAR?
        FDB     c_not                    ; NOT
        FDB     c_qmbra,Z4FE4            ; ?BRANCH --Z4FE4--v
        FDB     c_drop                   ; DROP
        FDB     c_bl                     ; BL
        FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_cexc                   ; C!
        FDB     c_updat                  ; UPDATE
Z4FE4   FDB     c_tild                   ; ~
        FDB     c_loop_,Z4FCA            ; (LOOP) --Z4FCA--^
Z4FEA   FDB     c_vbdat                  ; VBDAT
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; SETCUR
; ==================================================================
l_setcu FDB     l_is_so                  ; LFA -> SORT?

n_setcu FCB     $86                      ; NFA -> 6
        FCB     $53,$45,$54,$43,$55,$D2  ; "SETCUR"

c_setcu FDB     do_col                   ; : SETCUR
p_setcu FDB     c_is_so                  ; SORT?
        FDB     c_is_xy                  ; XY?
        FDB     c_at0                    ; AT
        FDB     c_semi_                  ; (;)

; ==================================================================
; PA>LI
; ==================================================================
l_pagtl FDB     l_is_la                  ; LFA -> LASTLI?

n_pagtl FCB     $85                      ; NFA -> 5
        FCB     $50,$41,$3E,$4C,$C9      ; "PA>LI"

c_pagtl FDB     do_col                   ; : PA>LI
p_pagtl FDB     c_ncu                    ; NCU
        FDB     c_curad                  ; CURAD
        FDB     c_endli                  ; ENDLI
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_lp1                    ; LP
        FDB     c_cat                    ; C@
        FDB     c_min                    ; MIN
        FDB     c_2dup                   ; 2DUP
        FDB     c_lp1                    ; LP
        FDB     c_1add                   ; 1+
        FDB     c_subro                  ; -ROT
        FDB     c_cmove                  ; CMOVE
        FDB     c_type                   ; TYPE
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; VERPAD
; ==================================================================
l_verpa FDB     l_recpa                  ; LFA -> RECPAD

n_verpa FCB     $86                      ; NFA -> 6
        FCB     $56,$45,$52,$50,$41,$C4  ; "VERPAD"

c_verpa FDB     do_col                   ; : VERPAD
p_verpa FDB     c_curad                  ; CURAD
        FDB     c_endli                  ; ENDLI
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_rot                    ; ROT
        FDB     c_2dup                   ; 2DUP
        FDB     c_cexc                   ; C!
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; LI>PA
; ==================================================================
l_ligt0 FDB     l_pagtl                  ; LFA -> PA>LI

n_ligt0 FCB     $85                      ; NFA -> 5
        FCB     $4C,$49,$3E,$50,$C1      ; "LI>PA"

c_ligt0 FDB     do_col                   ; : LI>PA
p_ligt0 FDB     c_recpa                  ; RECPAD
        FDB     c_lp1                    ; LP
        FDB     c_verpa                  ; VERPAD
        FDB     c_semi_                  ; (;)

; ==================================================================
; LI>PA3
; ==================================================================
l_ligt1 FDB     l_ligt0                  ; LFA -> LI>PA

n_ligt1 FCB     $86                      ; NFA -> 6
        FCB     $4C,$49,$3E,$50,$41,$B3  ; "LI>PA3"

c_ligt1 FDB     do_col                   ; : LI>PA3
p_ligt1 FDB     c_lp3                    ; LP3
        FDB     c_verpa                  ; VERPAD
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEBU
; ==================================================================
l_debu  FDB     l_ligt1                  ; LFA -> LI>PA3

n_debu  FCB     $84                      ; NFA -> 4
        FCB     $44,$45,$42,$D5          ; "DEBU"

c_debu  FDB     do_col                   ; : DEBU
p_debu  FDB     c_debad                  ; DEBAD
        FDB     c_mem                    ; MEM
        FDB     c_1,c_lit_               ; 1
        FDB     $0002                    ; 2
        FDB     c_at0                    ; AT
        FDB     c_semi_                  ; (;)

; ==================================================================
; MILU
; ==================================================================
l_milu  FDB     l_ent                    ; LFA -> ENT

n_milu  FCB     $84                      ; NFA -> 4
        FCB     $4D,$49,$4C,$D5          ; "MILU"

c_milu  FDB     do_col                   ; : MILU
p_milu  FDB     c_debad                  ; DEBAD
        FDB     c_cdiv0,c_lit_           ; C/L0
        FDB     $000B                    ; 11
        FDB     c_ast                    ; *
        FDB     c_add                    ; +
        FDB     c_mem                    ; MEM
        FDB     c_1,c_lit_               ; 1
        FDB     $000D                    ; 13
        FDB     c_at0                    ; AT
        FDB     c_semi_                  ; (;)

; ==================================================================
; ECRI
; ==================================================================
l_ecri  FDB     l_milu                   ; LFA -> MILU

n_ecri  FCB     $84                      ; NFA -> 4
        FCB     $45,$43,$52,$C9          ; "ECRI"

c_ecri  FDB     do_col                   ; : ECRI
p_ecri  FDB     c_ncu                    ; NCU
        FDB     c_dup                    ; DUP
        FDB     c_curad                  ; CURAD
        FDB     c_cexc                   ; C!
        FDB     c_tild                   ; ~
        FDB     c_1                      ; 1
        FDB     c_setcu                  ; SETCUR
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; BLINE
; ==================================================================
l_bline FDB     l_verpa                  ; LFA -> VERPAD

n_bline FCB     $85                      ; NFA -> 5
        FCB     $42,$4C,$49,$4E,$C5      ; "BLINE"

c_bline FDB     do_col                   ; : BLINE
p_bline FDB     c_from6                  ; <DEBLI
        FDB     c_curad                  ; CURAD
        FDB     c_cdiv0                  ; C/L0
        FDB     c_blank                  ; BLANK
        FDB     c_clr0                   ; CLR0
        FDB     c_semi_                  ; (;)

; ==================================================================
; VEXEC
; ==================================================================
l_vexec FDB     l_bline                  ; LFA -> BLINE

n_vexec FCB     $85                      ; NFA -> 5
        FCB     $56,$45,$58,$45,$C3      ; "VEXEC"

c_vexec FDB     do_col                   ; : VEXEC
p_vexec FDB     c_scr                    ; SCR
        FDB     c_addex                  ; +!
        FDB     c_vec                    ; VEC
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEB-MIL
; ==================================================================
l_debsu FDB     l_debu                   ; LFA -> DEBU

n_debsu FCB     $87                      ; NFA -> 7
        FCB     $44,$45,$42,$2D,$4D,$49  ; "DEB-MI"
        FCB     $CC                      ; "L"

c_debsu FDB     do_col                   ; : DEB-MIL
p_debsu FDB     c_ncu                    ; NCU
        FDB     c_curad                  ; CURAD
        FDB     c_debad                  ; DEBAD
        FDB     c_sub                    ; -
        FDB     c_0,c_lit_               ; 0
        FDB     $00EF                    ; 239
        FDB     c_ssdup                  ; SSDUP
        FDB     c_eq_                    ; <=>
        FDB     c_qmbra,Z5120            ; ?BRANCH --Z5120--v
        FDB     c_drop                   ; DROP
        FDB     c_milu                   ; MILU
        FDB     c_branc,Z516A            ; BRANCH --Z516A--v
Z5120   FDB     c_lit_
        FDB     $00F0,c_lit_             ; 240
        FDB     $01DF                    ; 479
        FDB     c_ssdup                  ; SSDUP
        FDB     c_eq_                    ; <=>
        FDB     c_qmbra,Z5138            ; ?BRANCH --Z5138--v
        FDB     c_drop                   ; DROP
        FDB     c_botm                   ; BOTM
        FDB     c_branc,Z516A            ; BRANCH --Z516A--v
Z5138   FDB     c_lit_
        FDB     $01E0,c_lit_             ; 480
        FDB     $02CF                    ; 719
        FDB     c_ssdup                  ; SSDUP
        FDB     c_eq_                    ; <=>
        FDB     c_qmbra,Z5150            ; ?BRANCH --Z5150--v
        FDB     c_drop                   ; DROP
        FDB     c_debu                   ; DEBU
        FDB     c_branc,Z516A            ; BRANCH --Z516A--v
Z5150   FDB     c_lit_
        FDB     $02D0,c_lit_             ; ' OR
        FDB     $03C0                    ; 960
        FDB     c_ssdup                  ; SSDUP
        FDB     c_eq_                    ; <=>
        FDB     c_qmbra,Z5168            ; ?BRANCH --Z5168--v
        FDB     c_drop                   ; DROP
        FDB     c_milu                   ; MILU
        FDB     c_branc,Z516A            ; BRANCH --Z516A--v
Z5168   FDB     c_drop                   ; DROP
Z516A   FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; MEMIN
; ==================================================================
l_memin FDB     l_ecri                   ; LFA -> ECRI

n_memin FCB     $85                      ; NFA -> 5
        FCB     $4D,$45,$4D,$49,$CE      ; "MEMIN"

c_memin FDB     do_col                   ; : MEMIN
p_memin FDB     c_2dup                   ; 2DUP
        FDB     c_min                    ; MIN
        FDB     c_subro                  ; -ROT
        FDB     c_max                    ; MAX
        FDB     c_rased                  ; RASEDPAD
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_edpad                  ; EDPAD
        FDB     c_2dup                   ; 2DUP
        FDB     c_exc                    ; !
        FDB     c_2add                   ; 2+
        FDB     c_swap                   ; SWAP
        FDB     c_2div                   ; 2/
        FDB     c_move                   ; MOVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; PAD>SC
; ==================================================================
l_padgt FDB     l_debsu                  ; LFA -> DEB-MIL

n_padgt FCB     $86                      ; NFA -> 6
        FCB     $50,$41,$44,$3E,$53,$C3  ; "PAD>SC"

c_padgt FDB     do_col                   ; : PAD>SC
p_padgt FDB     c_from6                  ; <DEBLI
        FDB     c_curad                  ; CURAD
        FDB     c_limad                  ; LIMAD
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_dup                    ; DUP
        FDB     c_0gt                    ; 0>
        FDB     c_qmbra,Z51E5            ; ?BRANCH --Z51E5--v
        FDB     c_edpad                  ; EDPAD
        FDB     c_dup                    ; DUP
        FDB     c_2add                   ; 2+
        FDB     c_swap                   ; SWAP
        FDB     c_at                     ; @
        FDB     c_rot                    ; ROT
        FDB     c_2dup                   ; 2DUP
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z51D1            ; ?BRANCH --Z51D1--v
        FDB     c_nip                    ; NIP
        FDB     c_branc,Z51D3            ; BRANCH --Z51D3--v
Z51D1   FDB     c_drop                   ; DROP
Z51D3   FDB     c_2dup,c_lit_            ; 2DUP
        FDB     $0004                    ; 4
        FDB     c_pick                   ; PICK
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_chain                  ; CHAINE
        FDB     c_branc,Z51E7            ; BRANCH --Z51E7--v
Z51E5   FDB     c_drop                   ; DROP
Z51E7   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; RZ>LI
; ==================================================================
l_rzgtl FDB     l_vexec                  ; LFA -> VEXEC

n_rzgtl FCB     $85                      ; NFA -> 5
        FCB     $52,$5A,$3E,$4C,$C9      ; "RZ>LI"

c_rzgtl FDB     do_col                   ; : RZ>LI
p_rzgtl FDB     c_ligt1                  ; LI>PA3
        FDB     c_ligt0                  ; LI>PA
        FDB     c_curex                  ; CUR!
        FDB     c_ncu                    ; NCU
        FDB     c_curad                  ; CURAD
        FDB     c_endli                  ; ENDLI
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_blank                  ; BLANK
        FDB     c_cel                    ; CEL
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; UCUR
; ==================================================================
l_ucur  FDB     l_memin                  ; LFA -> MEMIN

n_ucur  FCB     $84                      ; NFA -> 4
        FCB     $55,$43,$55,$D2          ; "UCUR"

c_ucur  FDB     do_col                   ; : UCUR
p_ucur  FDB     c_lit_
        FDB     $FFD8                    ; -40
        FDB     c_setcu                  ; SETCUR
        FDB     c_semi_                  ; (;)

; ==================================================================
; DCUR
; ==================================================================
l_dcur  FDB     l_padgt                  ; LFA -> PAD>SC

n_dcur  FCB     $84                      ; NFA -> 4
        FCB     $44,$43,$55,$D2          ; "DCUR"

c_dcur  FDB     do_col                   ; : DCUR
p_dcur  FDB     c_cdiv0                  ; C/L0
        FDB     c_setcu                  ; SETCUR
        FDB     c_semi_                  ; (;)

; ==================================================================
; LCUR
; ==================================================================
l_lcur  FDB     l_dcur                   ; LFA -> DCUR

n_lcur  FCB     $84                      ; NFA -> 4
        FCB     $4C,$43,$55,$D2          ; "LCUR"

c_lcur  FDB     do_col                   ; : LCUR
p_lcur  FDB     c_sub1                   ; -1
        FDB     c_setcu                  ; SETCUR
        FDB     c_semi_                  ; (;)

; ==================================================================
; RCUR
; ==================================================================
l_rcur  FDB     l_rzgtl                  ; LFA -> RZ>LI

n_rcur  FCB     $84                      ; NFA -> 4
        FCB     $52,$43,$55,$D2          ; "RCUR"

c_rcur  FDB     do_col                   ; : RCUR
p_rcur  FDB     c_1                      ; 1
        FDB     c_setcu                  ; SETCUR
        FDB     c_semi_                  ; (;)

; ==================================================================
; PA3>LI
; ==================================================================
l_pa3gt FDB     l_lcur                   ; LFA -> LCUR

n_pa3gt FCB     $86                      ; NFA -> 6
        FCB     $50,$41,$33,$3E,$4C,$C9  ; "PA3>LI"

c_pa3gt FDB     do_col                   ; : PA3>LI
p_pa3gt FDB     c_ncu                    ; NCU
        FDB     c_curad                  ; CURAD
        FDB     c_endli                  ; ENDLI
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_lp3                    ; LP3
        FDB     c_cat                    ; C@
        FDB     c_min                    ; MIN
        FDB     c_2dup                   ; 2DUP
        FDB     c_lp3                    ; LP3
        FDB     c_1add                   ; 1+
        FDB     c_subro                  ; -ROT
        FDB     c_cmove                  ; CMOVE
        FDB     c_type                   ; TYPE
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; +LINE
; ==================================================================
l_addli FDB     l_setcu                  ; LFA -> SETCUR

n_addli FCB     $85                      ; NFA -> 5
        FCB     $2B,$4C,$49,$4E,$C5      ; "+LINE"

c_addli FDB     do_col                   ; : +LINE
p_addli FDB     c_is_la                  ; LASTLI?
        FDB     c_not                    ; NOT
        FDB     c_qmbra,Z52B4            ; ?BRANCH --Z52B4--v
        FDB     c_curex                  ; CUR!
        FDB     c_ncu                    ; NCU
        FDB     c_from6                  ; <DEBLI
        FDB     c_curad                  ; CURAD
        FDB     c_dup                    ; DUP
        FDB     c_lastl                  ; LASTLI
        FDB     c_memin                  ; MEMIN
        FDB     c_bline                  ; BLINE
        FDB     c_lf                     ; LF
        FDB     c_dup                    ; DUP
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_mem                    ; MEM
        FDB     c_padgt                  ; PAD>SC
        FDB     c_mem                    ; MEM
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_ycu                    ; YCU
        FDB     c_branc,Z52B6            ; BRANCH --Z52B6--v
Z52B4   FDB     c_bell                   ; BELL
Z52B6   FDB     c_semi_                  ; (;)

; ==================================================================
; -LINE
; ==================================================================
l_subli FDB     l_ucur                   ; LFA -> UCUR

n_subli FCB     $85                      ; NFA -> 5
        FCB     $2D,$4C,$49,$4E,$C5      ; "-LINE"

c_subli FDB     do_col                   ; : -LINE
p_subli FDB     c_curex                  ; CUR!
        FDB     c_ncu                    ; NCU
        FDB     c_ligtp                  ; LI>PA2
        FDB     c_ligt0                  ; LI>PA
        FDB     c_from6                  ; <DEBLI
        FDB     c_cdiv0                  ; C/L0
        FDB     c_is_la                  ; LASTLI?
        FDB     c_qmbra,Z52DA            ; ?BRANCH --Z52DA--v
        FDB     c_rzgtl                  ; RZ>LI
        FDB     c_branc,Z52F4            ; BRANCH --Z52F4--v
Z52DA   FDB     c_lastl                  ; LASTLI
        FDB     c_over                   ; OVER
        FDB     c_curad                  ; CURAD
        FDB     c_add                    ; +
        FDB     c_over                   ; OVER
        FDB     c_memin                  ; MEMIN
        FDB     c_padgt                  ; PAD>SC
        FDB     c_swap                   ; SWAP
        FDB     c_blank                  ; BLANK
        FDB     c_clr0                   ; CLR0
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_ycu                    ; YCU
Z52F4   FDB     c_semi_                  ; (;)

; ==================================================================
; VALID
; ==================================================================
l_valid FDB     l_rcur                   ; LFA -> RCUR

n_valid FCB     $85                      ; NFA -> 5
        FCB     $56,$41,$4C,$49,$C4      ; "VALID"

c_valid FDB     do_col                   ; : VALID
p_valid FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_deb                    ; DEB
        FDB     c_nbcar                  ; NBCAR
        FDB     c_add,c_lit_             ; +
        FDB     $0025                    ; 37
        FDB     c_blank                  ; BLANK
        FDB     c_ncu                    ; NCU
        FDB     c_updat                  ; UPDATE
        FDB     c_semi_                  ; (;)

; ==================================================================
; ALINEA
; ==================================================================
l_aline FDB     l_subli                  ; LFA -> -LINE

n_aline FCB     $86                      ; NFA -> 6
        FCB     $41,$4C,$49,$4E,$45,$C1  ; "ALINEA"

c_aline FDB     do_col                   ; : ALINEA
p_aline FDB     c_adlic                  ; ADLICU
        FDB     c_ncu                    ; NCU
        FDB     c_mem,c_lit_             ; MEM
        FDB     $000D                    ; 13
        FDB     c_tild                   ; ~
        FDB     c_dcur                   ; DCUR
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; REDEX
; ==================================================================
l_redex FDB     l_valid                  ; LFA -> VALID

n_redex FCB     $85                      ; NFA -> 5
        FCB     $52,$45,$44,$45,$D8      ; "REDEX"

c_redex FDB     do_col                   ; : REDEX
p_redex FDB     c_vec1                   ; VEC1
        FDB     c_at                     ; @
        FDB     c_vec                    ; VEC
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_bqval                  ; `VAL
        FDB     c_exc                    ; !
        FDB     c_vec3                   ; VEC3
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; TOBOT
; ==================================================================
l_tobot FDB     l_pa3gt                  ; LFA -> PA3>LI

n_tobot FCB     $85                      ; NFA -> 5
        FCB     $54,$4F,$42,$4F,$D4      ; "TOBOT"

c_tobot FDB     do_col                   ; : TOBOT
p_tobot FDB     c_lit_
        FDB     $000E                    ; 14
        FDB     c_1                      ; 1
        FDB     c_at0,c_lit_             ; AT
        FDB     $000E                    ; 14
        FDB     c_pap                    ; PAP
        FDB     c_1p                     ; 1P
        FDB     c_ncu                    ; NCU
        FDB     c_2                      ; 2
        FDB     c_sp                     ; SP
        FDB     c_valid                  ; VALID
        FDB     c_dotq1                  ; (."F) len=23
        FCB     $17
        FDB     $5F20,$4563,$7261,$6E20  ; "_ Ecran "
        FDB     $436F,$7572,$616E,$7420  ; "Courant "
        FCB     $2F,$20,$46,$4F,$52,$54  ; "/ FORT"
        FCB     $48                      ; "H"
        FDB     c_14p,c_lit_             ; 14P
        FDB     $0004                    ; 4
        FDB     c_sp,c_lit_              ; SP
        FDB     $0009                    ; 9
        FDB     c_pap                    ; PAP
        FDB     c_scro                   ; SCRO
        FDB     c_1,c_lit_               ; 1
        FDB     $0018                    ; 24
        FDB     c_at0                    ; AT
        FDB     c_ycu                    ; YCU
        FDB     c_1                      ; 1
        FDB     c_bqfin                  ; `FIN
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_addex                  ; +!
        FDB     c_from_                  ; R>
        FDB     c_at                     ; @
        FDB     c_1                      ; 1
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z53CB            ; ?BRANCH --Z53CB--v
        FDB     c_bqval                  ; `VAL
        FDB     c_at                     ; @
        FDB     c_qmbra,Z53C9            ; ?BRANCH --Z53C9--v
        FDB     c_saves                  ; SAVE-BUFFERS
Z53C9   FDB     c_quit                   ; QUIT
Z53CB   FDB     c_semi_                  ; (;)

; ==================================================================
; DELCAR
; ==================================================================
l_delca FDB     l_tobot                  ; LFA -> TOBOT

n_delca FCB     $86                      ; NFA -> 6
        FCB     $44,$45,$4C,$43,$41,$D2  ; "DELCAR"

c_delca FDB     do_col                   ; : DELCAR
p_delca FDB     c_ncu                    ; NCU
        FDB     c_curad                  ; CURAD
        FDB     c_dup                    ; DUP
        FDB     c_1add                   ; 1+
        FDB     c_mem                    ; MEM
        FDB     c_ligt0                  ; LI>PA
        FDB     c_lp1                    ; LP
        FDB     c_dup                    ; DUP
        FDB     c_cat                    ; C@
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_cexc                   ; C!
        FDB     c_mem                    ; MEM
        FDB     c_pagtl                  ; PA>LI
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?JMP
; ==================================================================
l_qmjmp FDB     l_addli                  ; LFA -> +LINE

n_qmjmp FCB     $84                      ; NFA -> 4
        FCB     $3F,$4A,$4D,$D0          ; "?JMP"

c_qmjmp FDB     do_col                   ; : ?JMP
p_qmjmp FDB     c_qmbra,Z5411            ; ?BRANCH --Z5411--v
        FDB     c_sub1                   ; -1
        FDB     c_debad                  ; DEBAD
        FDB     c_1add                   ; 1+
        FDB     c_curad                  ; CURAD
        FDB     c_branc,Z5417            ; BRANCH --Z5417--v
Z5411   FDB     c_1                      ; 1
        FDB     c_limad                  ; LIMAD
        FDB     c_curad                  ; CURAD
Z5417   FDB     c_semi_                  ; (;)

; ==================================================================
; SP>
; ==================================================================
l_spgt  FDB     l_qmjmp                  ; LFA -> ?JMP

n_spgt  FCB     $83                      ; NFA -> 3
        FCB     $53,$50,$BE              ; "SP>"

c_spgt  FDB     do_col                   ; : SP>
p_spgt  FDB     c_qmjmp                  ; ?JMP
        FDB     c_do_,Z543B              ; (DO) --Z543B--v
Z5427   FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_dup                    ; DUP
        FDB     c_mem                    ; MEM
        FDB     c_cat                    ; C@
        FDB     c_bl                     ; BL
        FDB     c_ltgt                   ; <>
        FDB     c_qmlea                  ; (?LEAVE)
        FDB     c_addlo,Z5427            ; (+LOOP) --Z5427--^
Z543B   FDB     c_1                      ; 1
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5449            ; ?BRANCH --Z5449--v
        FDB     c_rcur                   ; RCUR
        FDB     c_branc,Z544B            ; BRANCH --Z544B--v
Z5449   FDB     c_lcur                   ; LCUR
Z544B   FDB     c_sub1                   ; -1
        FDB     c_eq                     ; =
        FDB     c_semi_                  ; (;)

; ==================================================================
; >SP
; ==================================================================
l_to_sp FDB     l_redex                  ; LFA -> REDEX

n_to_sp FCB     $83                      ; NFA -> 3
        FCB     $3E,$53,$D0              ; ">SP"

c_to_sp FDB     do_col                   ; : >SP
p_to_sp FDB     c_qmjmp                  ; ?JMP
        FDB     c_do_,Z5473              ; (DO) --Z5473--v
Z545F   FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_dup                    ; DUP
        FDB     c_mem                    ; MEM
        FDB     c_cat                    ; C@
        FDB     c_bl                     ; BL
        FDB     c_eq                     ; =
        FDB     c_qmlea                  ; (?LEAVE)
        FDB     c_addlo,Z545F            ; (+LOOP) --Z545F--^
Z5473   FDB     c_drop                   ; DROP
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_sub1                   ; -1
        FDB     c_eq                     ; =
        FDB     c_semi_                  ; (;)

; ==================================================================
; TO>SP
; ==================================================================
l_togts FDB     l_delca                  ; LFA -> DELCAR

n_togts FCB     $85                      ; NFA -> 5
        FCB     $54,$4F,$3E,$53,$D0      ; "TO>SP"

c_togts FDB     do_col                   ; : TO>SP
p_togts FDB     c_ncu                    ; NCU
        FDB     c_spgt                   ; SP>
        FDB     c_curad                  ; CURAD
        FDB     c_swap                   ; SWAP
        FDB     c_to_sp                  ; >SP
        FDB     c_curad                  ; CURAD
        FDB     c_swap                   ; SWAP
        FDB     c_qmbra,Z549D            ; ?BRANCH --Z549D--v
        FDB     c_swap                   ; SWAP
Z549D   FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_lp1                    ; LP
        FDB     c_2dup                   ; 2DUP
        FDB     c_cexc                   ; C!
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; RAZEND
; ==================================================================
l_razen FDB     l_to_sp                  ; LFA -> >SP

n_razen FCB     $86                      ; NFA -> 6
        FCB     $52,$41,$5A,$45,$4E,$C4  ; "RAZEND"

c_razen FDB     do_col                   ; : RAZEND
p_razen FDB     c_limad                  ; LIMAD
        FDB     c_adlic                  ; ADLICU
        FDB     c_dup                    ; DUP
        FDB     c_lastl                  ; LASTLI
        FDB     c_memin                  ; MEMIN
        FDB     c_ncu                    ; NCU
        FDB     c_rzgtl                  ; RZ>LI
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_dup                    ; DUP
        FDB     c_subro                  ; -ROT
        FDB     c_sub                    ; -
        FDB     c_blank                  ; BLANK
        FDB     c_ces                    ; CES
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; CURCOM
; ==================================================================
l_curco FDB     l_spgt                   ; LFA -> SP>

n_curco FCB     $86                      ; NFA -> 6
        FCB     $43,$55,$52,$43,$4F,$CD  ; "CURCOM"

c_curco FDB     do_col                   ; : CURCOM
p_curco FDB     c_lit_
        FDB     $0008                    ; 8
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z54FB            ; ?BRANCH --Z54FB--v
        FDB     c_drop                   ; DROP
        FDB     c_lcur                   ; LCUR
        FDB     c_branc,Z554D            ; BRANCH --Z554D--v
Z54FB   FDB     c_lit_
        FDB     $0009                    ; 9
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z550F            ; ?BRANCH --Z550F--v
        FDB     c_drop                   ; DROP
        FDB     c_rcur                   ; RCUR
        FDB     c_branc,Z554D            ; BRANCH --Z554D--v
Z550F   FDB     c_lit_
        FDB     $000A                    ; 10
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5523            ; ?BRANCH --Z5523--v
        FDB     c_drop                   ; DROP
        FDB     c_dcur                   ; DCUR
        FDB     c_branc,Z554D            ; BRANCH --Z554D--v
Z5523   FDB     c_lit_
        FDB     $000B                    ; 11
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5537            ; ?BRANCH --Z5537--v
        FDB     c_drop                   ; DROP
        FDB     c_ucur                   ; UCUR
        FDB     c_branc,Z554D            ; BRANCH --Z554D--v
Z5537   FDB     c_lit_
        FDB     $000D                    ; 13
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z554B            ; ?BRANCH --Z554B--v
        FDB     c_drop                   ; DROP
        FDB     c_aline                  ; ALINEA
        FDB     c_branc,Z554D            ; BRANCH --Z554D--v
Z554B   FDB     c_drop                   ; DROP
Z554D   FDB     c_semi_                  ; (;)

; ==================================================================
; -SC>PAD
; ==================================================================
l_subsc FDB     l_aline                  ; LFA -> ALINEA

n_subsc FCB     $87                      ; NFA -> 7
        FCB     $2D,$53,$43,$3E,$50,$41  ; "-SC>PA"
        FCB     $C4                      ; "D"

c_subsc FDB     do_col                   ; : -SC>PAD
p_subsc FDB     c_com0                   ; COM0
        FDB     c_at                     ; @
        FDB     c_dup,c_lit_             ; DUP
        FDB     $000E                    ; 14
        FDB     c_eq                     ; =
        FDB     c_over,c_lit_            ; OVER
        FDB     $000F                    ; 15
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_over,c_lit_            ; OVER
        FDB     $0010                    ; 16
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_swap,c_lit_            ; SWAP
        FDB     $001A                    ; 26
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_qmbra,Z5591            ; ?BRANCH --Z5591--v
        FDB     c_ncu                    ; NCU
        FDB     c_padgt                  ; PAD>SC
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
Z5591   FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; INSCAR
; ==================================================================
l_insca FDB     l_subsc                  ; LFA -> -SC>PAD

n_insca FCB     $86                      ; NFA -> 6
        FCB     $49,$4E,$53,$43,$41,$D2  ; "INSCAR"

c_insca FDB     do_col                   ; : INSCAR
p_insca FDB     c_endli                  ; ENDLI
Z55A2   FDB     c_curad                  ; CURAD
        FDB     c_over                   ; OVER
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z55FC            ; ?BRANCH --Z55FC--v
        FDB     c_3p                     ; 3P
        FDB     c_ycu                    ; YCU
        FDB     c_11p                    ; 11P
        FDB     c_key                    ; KEY
        FDB     c_dup                    ; DUP
        FDB     c_is_ca                  ; CAR?
        FDB     c_qmbra,Z55C8            ; ?BRANCH --Z55C8--v
        FDB     c_ligt0                  ; LI>PA
        FDB     c_ncu                    ; NCU
        FDB     c_ecri                   ; ECRI
        FDB     c_pagtl                  ; PA>LI
        FDB     c_branc,Z55F8            ; BRANCH --Z55F8--v
Z55C8   FDB     c_dup,c_lit_             ; DUP
        FDB     $007F                    ; 127
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z55DC            ; ?BRANCH --Z55DC--v
        FDB     c_drop                   ; DROP
        FDB     c_delca                  ; DELCAR
        FDB     c_branc,Z55F8            ; BRANCH --Z55F8--v
Z55DC   FDB     c_dup,c_lit_             ; DUP
        FDB     $000D                    ; 13
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z55F2            ; ?BRANCH --Z55F2--v
        FDB     c_aline                  ; ALINEA
        FDB     c_drop                   ; DROP
        FDB     c_insca                  ; INSCAR
        FDB     c_branc,Z55F8            ; BRANCH --Z55F8--v
Z55F2   FDB     c_curco                  ; CURCOM
        FDB     c_drop                   ; DROP
        FDB     c_curad                  ; CURAD
Z55F8   FDB     c_branc,Z55A2            ; BRANCH --Z55A2--^
Z55FC   FDB     c_ycu                    ; YCU
        FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; INSLI
; ==================================================================
l_insli FDB     l_insca                  ; LFA -> INSCAR

n_insli FCB     $85                      ; NFA -> 5
        FCB     $49,$4E,$53,$4C,$C9      ; "INSLI"

c_insli FDB     do_col                   ; : INSLI
p_insli FDB     c_is_la                  ; LASTLI?
        FDB     c_qmbra,Z5618            ; ?BRANCH --Z5618--v
        FDB     c_bell                   ; BELL
        FDB     c_branc,Z561C            ; BRANCH --Z561C--v
Z5618   FDB     c_addli                  ; +LINE
        FDB     c_pagtl                  ; PA>LI
Z561C   FDB     c_semi_                  ; (;)

; ==================================================================
; FREE
; ==================================================================
l_free  FDB     l_razen                  ; LFA -> RAZEND

n_free  FCB     $84                      ; NFA -> 4
        FCB     $46,$52,$45,$C5          ; "FREE"

c_free  FDB     do_col                   ; : FREE
p_free  FDB     c_s0                     ; S0
        FDB     c_at,c_lit_              ; @
        FDB     $0100                    ; 256
        FDB     c_sub                    ; -
        FDB     c_here                   ; HERE
        FDB     c_sub                    ; -
        FDB     c_bdivb                  ; B/BUF
        FDB     c_div                    ; /
        FDB     c_cr                     ; CR
        FDB     c_3p                     ; 3P
        FDB     c_dotq1                  ; (."F) len=17
        FCB     $11
        FDB     $4E6F,$6D62,$7265,$2064  ; "Nombre d"
        FDB     $6520,$626C,$6F63,$6B73  ; "e blocks"
        FCB     $20                      ; " "
        FDB     c_dotq1                  ; (."F) len=8
        FCB     $08
        FDB     $6C69,$6272,$6573,$3A20  ; "libres: "
        FDB     c_6p                     ; 6P
        FDB     c_dot                    ; .
        FDB     c_cr                     ; CR
        FDB     c_14p                    ; 14P
        FDB     c_semi_                  ; (;)

; ==================================================================
; SC>PAD
; ==================================================================
l_scgtp FDB     l_curco                  ; LFA -> CURCOM

n_scgtp FCB     $86                      ; NFA -> 6
        FCB     $53,$43,$3E,$50,$41,$C4  ; "SC>PAD"

c_scgtp FDB     do_col                   ; : SC>PAD
p_scgtp FDB     c_adlic                  ; ADLICU
Z5673   FDB     c_2p                     ; 2P
        FDB     c_11p                    ; 11P
        FDB     c_key                    ; KEY
        FDB     c_dup,c_lit_             ; DUP
        FDB     $000E                    ; 14
        FDB     c_eq                     ; =
        FDB     c_not                    ; NOT
        FDB     c_qmbra,Z56CD            ; ?BRANCH --Z56CD--v
        FDB     c_ycu,c_lit_             ; YCU
        FDB     $001B                    ; 27
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z569F            ; ?BRANCH --Z569F--v
        FDB     c_drop                   ; DROP
        FDB     c_drop                   ; DROP
        FDB     c_exit                   ; EXIT
        FDB     c_branc,Z56C9            ; BRANCH --Z56C9--v
Z569F   FDB     c_lit_
        FDB     $000A                    ; 10
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z56B3            ; ?BRANCH --Z56B3--v
        FDB     c_drop                   ; DROP
        FDB     c_dcur                   ; DCUR
        FDB     c_branc,Z56C9            ; BRANCH --Z56C9--v
Z56B3   FDB     c_lit_
        FDB     $000B                    ; 11
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z56C7            ; ?BRANCH --Z56C7--v
        FDB     c_drop                   ; DROP
        FDB     c_ucur                   ; UCUR
        FDB     c_branc,Z56C9            ; BRANCH --Z56C9--v
Z56C7   FDB     c_drop                   ; DROP
Z56C9   FDB     c_branc,Z5673            ; BRANCH --Z5673--^
Z56CD   FDB     c_drop                   ; DROP
        FDB     c_adlic                  ; ADLICU
        FDB     c_dup                    ; DUP
        FDB     c_mem                    ; MEM
        FDB     c_memin                  ; MEMIN
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; /LI
; ==================================================================
l_divli FDB     l_scgtp                  ; LFA -> SC>PAD

n_divli FCB     $83                      ; NFA -> 3
        FCB     $2F,$4C,$C9              ; "/LI"

c_divli FDB     do_col                   ; : /LI
p_divli FDB     c_is_la                  ; LASTLI?
        FDB     c_qmbra,Z56F3            ; ?BRANCH --Z56F3--v
        FDB     c_bell                   ; BELL
        FDB     c_branc,Z5709            ; BRANCH --Z5709--v
Z56F3   FDB     c_curad                  ; CURAD
        FDB     c_ncu                    ; NCU
        FDB     c_rzgtl                  ; RZ>LI
        FDB     c_dcur                   ; DCUR
        FDB     c_addli                  ; +LINE
        FDB     c_pagtl                  ; PA>LI
        FDB     c_mem                    ; MEM
        FDB     c_curex                  ; CUR!
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_ycu                    ; YCU
Z5709   FDB     c_semi_                  ; (;)

; ==================================================================
; COL
; ==================================================================
l_col0  FDB     l_divli                  ; LFA -> /LI

n_col0  FCB     $83                      ; NFA -> 3
        FCB     $43,$4F,$CC              ; "COL"

c_col0  FDB     p_col0                   ; ASSEMBLER
p_col0  LEAU    $01,U
        PULU    A
        SWI
        FCB     $24
        JMP     p_next

; ==================================================================
; PILE?
; ==================================================================
l_is_pi FDB     l_togts                  ; LFA -> TO>SP

n_is_pi FCB     $85                      ; NFA -> 5
        FCB     $50,$49,$4C,$45,$BF      ; "PILE?"

c_is_pi FDB     do_col                   ; : PILE?
p_is_pi FDB     c_hex                    ; HEX
        FDB     c_depth                  ; DEPTH
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z575B,c_lit_     ; ?BRANCH --Z575B--v
        FDB     $0008                    ; 8
        FDB     c_min                    ; MIN
        FDB     c_1sub                   ; 1-
        FDB     c_0                      ; 0
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z575B              ; (DO) --Z575B--v
Z573F   FDB     c_i                      ; I
        FDB     c_pick                   ; PICK
        FDB     c_0                      ; 0
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_gt                   ; #>
        FDB     c_type                   ; TYPE
        FDB     c_space                  ; SPACE
        FDB     c_sub1                   ; -1
        FDB     c_addlo,Z573F            ; (+LOOP) --Z573F--^
Z575B   FDB     c_dec0                   ; DEC0
        FDB     c_semi_                  ; (;)

; ==================================================================
; OPWIN
; ==================================================================
l_opwin FDB     l_col0                   ; LFA -> COL

n_opwin FCB     $85                      ; NFA -> 5
        FCB     $4F,$50,$57,$49,$CE      ; "OPWIN"

c_opwin FDB     do_col                   ; : OPWIN
p_opwin FDB     c_0                      ; 0
        FDB     c_vpad                   ; VPAD
        FDB     c_exc                    ; !
        FDB     c_win                    ; WIN
        FDB     c_xz                     ; XZ
        FDB     c_at                     ; @
        FDB     c_xa                     ; XA
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_1add                   ; 1+
        FDB     c_vpad                   ; VPAD
        FDB     c_exc                    ; !
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_deb                    ; DEB
        FDB     c_ya                     ; YA
        FDB     c_at                     ; @
        FDB     c_2sub                   ; 2-
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ast                    ; *
        FDB     c_add                    ; +
        FDB     c_xa                     ; XA
        FDB     c_at                     ; @
        FDB     c_1sub                   ; 1-
        FDB     c_add                    ; +
        FDB     c_edpad                  ; EDPAD
        FDB     c_yz                     ; YZ
        FDB     c_at                     ; @
        FDB     c_ya                     ; YA
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_1add                   ; 1+
        FDB     c_0                      ; 0
        FDB     c_do_,Z57D1              ; (DO) --Z57D1--v
Z57AF   FDB     c_over                   ; OVER
        FDB     c_cdiv0                  ; C/L0
        FDB     c_i                      ; I
        FDB     c_ast                    ; *
        FDB     c_add                    ; +
        FDB     c_over                   ; OVER
        FDB     c_vpad                   ; VPAD
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_subro                  ; -ROT
        FDB     c_i                      ; I
        FDB     c_ast                    ; *
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_loop_,Z57AF            ; (LOOP) --Z57AF--^
Z57D1   FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; CLWIN
; ==================================================================
l_clwin FDB     l_opwin                  ; LFA -> OPWIN

n_clwin FCB     $85                      ; NFA -> 5
        FCB     $43,$4C,$57,$49,$CE      ; "CLWIN"

c_clwin FDB     do_col                   ; : CLWIN
p_clwin FDB     c_scpa,c_lit_            ; SCPA
        FDB     $0009                    ; 9
        FDB     c_pap                    ; PAP
        FDB     c_11p                    ; 11P
        FDB     c_clc                    ; CLC
        FDB     c_ncu                    ; NCU
        FDB     c_edpad                  ; EDPAD
        FDB     c_xz                     ; XZ
        FDB     c_at                     ; @
        FDB     c_xa                     ; XA
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_1add                   ; 1+
        FDB     c_yz                     ; YZ
        FDB     c_at                     ; @
        FDB     c_ya                     ; YA
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_1add                   ; 1+
        FDB     c_ast                    ; *
        FDB     c_chain                  ; CHAINE
        FDB     c_home                   ; HOME
        FDB     c_1,c_lit_               ; 1
        FDB     $0018                    ; 24
        FDB     c_at0                    ; AT
        FDB     c_scro                   ; SCRO
        FDB     c_ycu                    ; YCU
        FDB     c_14p                    ; 14P
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; COM!
; ==================================================================
l_comex FDB     l_clwin                  ; LFA -> CLWIN

n_comex FCB     $84                      ; NFA -> 4
        FCB     $43,$4F,$4D,$A1          ; "COM!"

c_comex FDB     do_col                   ; : COM!
p_comex FDB     c_dup                    ; DUP
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0008,c_lit_             ; 8
        FDB     $000D                    ; 13
        FDB     c_eq_                    ; <=>
        FDB     c_over,c_lit_            ; OVER
        FDB     $0015                    ; 21
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_over,c_lit_            ; OVER
        FDB     $001B                    ; 27
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_over,c_lit_            ; OVER
        FDB     $001C                    ; 28
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_over,c_lit_            ; OVER
        FDB     $001E                    ; 30
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_over,c_lit_            ; OVER
        FDB     $00FD                    ; 253
        FDB     c_gt                     ; >
        FDB     c_or                     ; OR
        FDB     c_qmbra,Z5872            ; ?BRANCH --Z5872--v
        FDB     c_drop                   ; DROP
        FDB     c_branc,Z5876            ; BRANCH --Z5876--v
Z5872   FDB     c_com0                   ; COM0
        FDB     c_exc                    ; !
Z5876   FDB     c_semi_                  ; (;)

; ==================================================================
; PA2>LI
; ==================================================================
l_pa2gt FDB     l_is_pi                  ; LFA -> PILE?

n_pa2gt FCB     $86                      ; NFA -> 6
        FCB     $50,$41,$32,$3E,$4C,$C9  ; "PA2>LI"

c_pa2gt FDB     do_col                   ; : PA2>LI
p_pa2gt FDB     c_from6                  ; <DEBLI
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_curad                  ; CURAD
        FDB     c_lp2                    ; LP2
        FDB     c_count                  ; COUNT
        FDB     c_2dup                   ; 2DUP
        FDB     c_curad                  ; CURAD
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_chain                  ; CHAINE
        FDB     c_mem                    ; MEM
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_semi_                  ; (;)

; ==================================================================
; -RAZEND
; ==================================================================
l_subra FDB     l_insli                  ; LFA -> INSLI

n_subra FCB     $87                      ; NFA -> 7
        FCB     $2D,$52,$41,$5A,$45,$4E  ; "-RAZEN"
        FCB     $C4                      ; "D"

c_subra FDB     do_col                   ; : -RAZEND
p_subra FDB     c_cur                    ; CUR
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_mem                    ; MEM
        FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_limad                  ; LIMAD
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z58C7            ; ?BRANCH --Z58C7--v
        FDB     c_subsc                  ; -SC>PAD
Z58C7   FDB     c_semi_                  ; (;)

; ==================================================================
; UN-LINE
; ==================================================================
l_unsub FDB     l_subra                  ; LFA -> -RAZEND

n_unsub FCB     $87                      ; NFA -> 7
        FCB     $55,$4E,$2D,$4C,$49,$4E  ; "UN-LIN"
        FCB     $C5                      ; "E"

c_unsub FDB     do_col                   ; : UN-LINE
p_unsub FDB     c_ncu                    ; NCU
        FDB     c_cur                    ; CUR
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_mem                    ; MEM
        FDB     c_pa2gt                  ; PA2>LI
        FDB     c_curad                  ; CURAD
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_limad                  ; LIMAD
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z58F5            ; ?BRANCH --Z58F5--v
        FDB     c_dcur                   ; DCUR
        FDB     c_padgt                  ; PAD>SC
        FDB     c_mem                    ; MEM
Z58F5   FDB     c_0                      ; 0
        FDB     c_setcu                  ; SETCUR
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; -RZ>LI
; ==================================================================
l_subrz FDB     l_unsub                  ; LFA -> UN-LINE

n_subrz FCB     $86                      ; NFA -> 6
        FCB     $2D,$52,$5A,$3E,$4C,$C9  ; "-RZ>LI"

c_subrz FDB     do_col                   ; : -RZ>LI
p_subrz FDB     c_ncu                    ; NCU
        FDB     c_cur                    ; CUR
        FDB     c_at                     ; @
        FDB     c_mem                    ; MEM
        FDB     c_pa2gt                  ; PA2>LI
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; UNCOM
; ==================================================================
l_uncom FDB     l_subrz                  ; LFA -> -RZ>LI

n_uncom FCB     $85                      ; NFA -> 5
        FCB     $55,$4E,$43,$4F,$CD      ; "UNCOM"

c_uncom FDB     do_col                   ; : UNCOM
p_uncom FDB     c_com0                   ; COM0
        FDB     c_at,c_lit_              ; @
        FDB     $000F                    ; 15
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5938            ; ?BRANCH --Z5938--v
        FDB     c_drop                   ; DROP
        FDB     c_subra                  ; -RAZEND
        FDB     c_branc,Z599E            ; BRANCH --Z599E--v
Z5938   FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z594C            ; ?BRANCH --Z594C--v
        FDB     c_drop                   ; DROP
        FDB     c_subra                  ; -RAZEND
        FDB     c_branc,Z599E            ; BRANCH --Z599E--v
Z594C   FDB     c_lit_
        FDB     $0013                    ; 19
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5960            ; ?BRANCH --Z5960--v
        FDB     c_drop                   ; DROP
        FDB     c_unsub                  ; UN-LINE
        FDB     c_branc,Z599E            ; BRANCH --Z599E--v
Z5960   FDB     c_lit_
        FDB     $0014                    ; 20
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5974            ; ?BRANCH --Z5974--v
        FDB     c_drop                   ; DROP
        FDB     c_subrz                  ; -RZ>LI
        FDB     c_branc,Z599E            ; BRANCH --Z599E--v
Z5974   FDB     c_lit_
        FDB     $0017                    ; 23
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5988            ; ?BRANCH --Z5988--v
        FDB     c_drop                   ; DROP
        FDB     c_unsub                  ; UN-LINE
        FDB     c_branc,Z599E            ; BRANCH --Z599E--v
Z5988   FDB     c_lit_
        FDB     $001A                    ; 26
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z599C            ; ?BRANCH --Z599C--v
        FDB     c_drop                   ; DROP
        FDB     c_subra                  ; -RAZEND
        FDB     c_branc,Z599E            ; BRANCH --Z599E--v
Z599C   FDB     c_drop                   ; DROP
Z599E   FDB     c_semi_                  ; (;)

; ==================================================================
; INIPAD
; ==================================================================
l_inipa FDB     l_uncom                  ; LFA -> UNCOM

n_inipa FCB     $86                      ; NFA -> 6
        FCB     $49,$4E,$49,$50,$41,$C4  ; "INIPAD"

c_inipa FDB     do_col                   ; : INIPAD
p_inipa FDB     c_here                   ; HERE
        FDB     c_s0                     ; S0
        FDB     c_at                     ; @
        FDB     c_over                   ; OVER
        FDB     c_sub                    ; -
        FDB     c_cdiv0                  ; C/L0
        FDB     c_2div                   ; 2/
        FDB     c_sub                    ; -
        FDB     c_nbcar                  ; NBCAR
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z59FD            ; ?BRANCH --Z59FD--v
        FDB     c_dup                    ; DUP
        FDB     c_dup                    ; DUP
        FDB     c_dup                    ; DUP
        FDB     c_bqlp                   ; `LP
        FDB     c_exc                    ; !
        FDB     c_cdiv0                  ; C/L0
        FDB     c_1add                   ; 1+
        FDB     c_add                    ; +
        FDB     c_bqlp2                  ; `LP2
        FDB     c_exc                    ; !
        FDB     c_cdiv0                  ; C/L0
        FDB     c_2ast                   ; 2*
        FDB     c_2add                   ; 2+
        FDB     c_add                    ; +
        FDB     c_bqlp3                  ; `LP3
        FDB     c_exc                    ; !
        FDB     c_cdiv0,c_lit_           ; C/L0
        FDB     $0003                    ; 3
        FDB     c_ast,c_lit_             ; *
        FDB     $0003                    ; 3
        FDB     c_add                    ; +
        FDB     c_add                    ; +
        FDB     c_bqedp                  ; `EDPAD
        FDB     c_exc                    ; !
        FDB     c_true                   ; TRUE
        FDB     c_branc,Z59FF            ; BRANCH --Z59FF--v
Z59FD   FDB     c_false                  ; FALSE
Z59FF   FDB     c_semi_                  ; (;)

; ==================================================================
; L-VIDE?
; ==================================================================
l_is_ls FDB     l_pa2gt                  ; LFA -> PA2>LI

n_is_ls FCB     $87                      ; NFA -> 7
        FCB     $4C,$2D,$56,$49,$44,$45  ; "L-VIDE"
        FCB     $BF                      ; "?"

c_is_ls FDB     do_col                   ; : L-VIDE?
p_is_ls FDB     c_deb                    ; DEB
        FDB     c_nbcar                  ; NBCAR
        FDB     c_add                    ; +
        FDB     c_cdiv0                  ; C/L0
        FDB     c_sub                    ; -
        FDB     c_sub1                   ; -1
        FDB     c_cdiv0                  ; C/L0
        FDB     c_0                      ; 0
        FDB     c_do_,Z5A3B              ; (DO) --Z5A3B--v
Z5A21   FDB     c_over                   ; OVER
        FDB     c_i                      ; I
        FDB     c_add                    ; +
        FDB     c_cat,c_lit_             ; C@
        FDB     $0020                    ; 32
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z5A37            ; ?BRANCH --Z5A37--v
        FDB     c_1add                   ; 1+
        FDB     c_leave                  ; (LEAVE)
Z5A37   FDB     c_loop_,Z5A21            ; (LOOP) --Z5A21--^
Z5A3B   FDB     c_nip                    ; NIP
        FDB     c_semi_                  ; (;)

; ==================================================================
; VA>VIDE
; ==================================================================
l_vagtv FDB     l_free                   ; LFA -> FREE

n_vagtv FCB     $87                      ; NFA -> 7
        FCB     $56,$41,$3E,$56,$49,$44  ; "VA>VID"
        FCB     $C5                      ; "E"

c_vagtv FDB     do_col                   ; : VA>VIDE
p_vagtv FDB     c_0                      ; 0
        FDB     c_dispe                  ; DISPEC
        FDB     c_exc                    ; !
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
Z5A55   FDB     c_1add                   ; 1+
        FDB     c_dup                    ; DUP
        FDB     c_is_ls                  ; L-VIDE?
        FDB     c_qmbra,Z5A55            ; ?BRANCH --Z5A55--^
        FDB     c_dispe                  ; DISPEC
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; ECARTEX
; ==================================================================
l_ecart FDB     l_inipa                  ; LFA -> INIPAD

n_ecart FCB     $87                      ; NFA -> 7
        FCB     $45,$43,$41,$52,$54,$45  ; "ECARTE"
        FCB     $D8                      ; "X"

c_ecart FDB     do_col                   ; : ECARTEX
p_ecart FDB     c_lit_
        FDB     $0006,c_lit_             ; 6
        FDB     $1888                    ; ' #BUFS
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc                    ; !
        FDB     c_ncu                    ; NCU
        FDB     c_vagtv                  ; VA>VIDE
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_1add                   ; 1+
        FDB     c_dispe                  ; DISPEC
        FDB     c_at                     ; @
        FDB     c_do_,Z5AC1              ; (DO) --Z5AC1--v
Z5A8F   FDB     c_i                      ; I
        FDB     c_deb                    ; DEB
        FDB     c_dup                    ; DUP
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_nbcar                  ; NBCAR
        FDB     c_cdiv0                  ; C/L0
        FDB     c_sub                    ; -
        FDB     c_cmov0                  ; CMOVE>
        FDB     c_updat                  ; UPDATE
        FDB     c_i                      ; I
        FDB     c_1sub                   ; 1-
        FDB     c_deb                    ; DEB
        FDB     c_nbcar                  ; NBCAR
        FDB     c_add                    ; +
        FDB     c_cdiv0                  ; C/L0
        FDB     c_sub                    ; -
        FDB     c_i                      ; I
        FDB     c_deb                    ; DEB
        FDB     c_cdiv0                  ; C/L0
        FDB     c_cmove                  ; CMOVE
        FDB     c_updat                  ; UPDATE
        FDB     c_sub1                   ; -1
        FDB     c_addlo,Z5A8F            ; (+LOOP) --Z5A8F--^
Z5AC1   FDB     c_lastl                  ; LASTLI
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_1add                   ; 1+
        FDB     c_deb                    ; DEB
        FDB     c_cdiv0                  ; C/L0
        FDB     c_cmove                  ; CMOVE
        FDB     c_addli                  ; +LINE
        FDB     c_updat                  ; UPDATE
        FDB     c_saves                  ; SAVE-BUFFERS
        FDB     c_11p                    ; 11P
        FDB     c_semi_                  ; (;)

; ==================================================================
; COMM
; ==================================================================
l_comm  FDB     l_comex                  ; LFA -> COM!

n_comm  FCB     $84                      ; NFA -> 4
        FCB     $43,$4F,$4D,$CD          ; "COMM"

c_comm  FDB     do_col                   ; : COMM
p_comm  FDB     c_lit_
        FDB     $0008,c_lit_             ; 8
        FDB     $000D                    ; 13
        FDB     c_ssdup                  ; SSDUP
        FDB     c_eq_                    ; <=>
        FDB     c_qmbra,Z5AFA            ; ?BRANCH --Z5AFA--v
        FDB     c_drop                   ; DROP
        FDB     c_curco                  ; CURCOM
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5AFA   FDB     c_2                      ; 2
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5B0E            ; ?BRANCH --Z5B0E--v
        FDB     c_drop                   ; DROP
        FDB     c_0                      ; 0
        FDB     c_togts                  ; TO>SP
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5B0E   FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5B24            ; ?BRANCH --Z5B24--v
        FDB     c_drop                   ; DROP
        FDB     c_sub1                   ; -1
        FDB     c_togts                  ; TO>SP
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5B24   FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5B38            ; ?BRANCH --Z5B38--v
        FDB     c_drop                   ; DROP
        FDB     c_pa3gt                  ; PA3>LI
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5B38   FDB     c_lit_
        FDB     $000E                    ; 14
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5B4C            ; ?BRANCH --Z5B4C--v
        FDB     c_drop                   ; DROP
        FDB     c_scgtp                  ; SC>PAD
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5B4C   FDB     c_lit_
        FDB     $000F                    ; 15
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5B60            ; ?BRANCH --Z5B60--v
        FDB     c_drop                   ; DROP
        FDB     c_razen                  ; RAZEND
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5B60   FDB     c_lit_
        FDB     $0010                    ; 16
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5B74            ; ?BRANCH --Z5B74--v
        FDB     c_drop                   ; DROP
        FDB     c_insli                  ; INSLI
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5B74   FDB     c_lit_
        FDB     $0011                    ; 17
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5B88            ; ?BRANCH --Z5B88--v
        FDB     c_drop                   ; DROP
        FDB     c_pagtl                  ; PA>LI
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5B88   FDB     c_lit_
        FDB     $0012                    ; 18
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5B9C            ; ?BRANCH --Z5B9C--v
        FDB     c_drop                   ; DROP
        FDB     c_ecart                  ; ECARTEX
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5B9C   FDB     c_lit_
        FDB     $0013                    ; 19
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5BB0            ; ?BRANCH --Z5BB0--v
        FDB     c_drop                   ; DROP
        FDB     c_subli                  ; -LINE
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5BB0   FDB     c_lit_
        FDB     $0014                    ; 20
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5BC4            ; ?BRANCH --Z5BC4--v
        FDB     c_drop                   ; DROP
        FDB     c_rzgtl                  ; RZ>LI
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5BC4   FDB     c_lit_
        FDB     $0015                    ; 21
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5BD8            ; ?BRANCH --Z5BD8--v
        FDB     c_drop                   ; DROP
        FDB     c_subsc                  ; -SC>PAD
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5BD8   FDB     c_lit_
        FDB     $0016                    ; 22
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5BEC            ; ?BRANCH --Z5BEC--v
        FDB     c_drop                   ; DROP
        FDB     c_namee                  ; NAME=
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5BEC   FDB     c_lit_
        FDB     $0017                    ; 23
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5C00            ; ?BRANCH --Z5C00--v
        FDB     c_drop                   ; DROP
        FDB     c_divli                  ; /LI
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5C00   FDB     c_lit_
        FDB     $0019                    ; 25
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5C14            ; ?BRANCH --Z5C14--v
        FDB     c_drop                   ; DROP
        FDB     c_ligt1                  ; LI>PA3
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5C14   FDB     c_lit_
        FDB     $001A                    ; 26
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5C28            ; ?BRANCH --Z5C28--v
        FDB     c_drop                   ; DROP
        FDB     c_addli                  ; +LINE
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5C28   FDB     c_lit_
        FDB     $001B                    ; 27
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5C3C            ; ?BRANCH --Z5C3C--v
        FDB     c_drop                   ; DROP
        FDB     c_tobot                  ; TOBOT
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5C3C   FDB     c_lit_
        FDB     $001C                    ; 28
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5C54            ; ?BRANCH --Z5C54--v
        FDB     c_drop                   ; DROP
        FDB     c_valid                  ; VALID
        FDB     c_sub1                   ; -1
        FDB     c_vexec                  ; VEXEC
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5C54   FDB     c_lit_
        FDB     $001D                    ; 29
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5C6A            ; ?BRANCH --Z5C6A--v
        FDB     c_drop                   ; DROP
        FDB     c_1p                     ; 1P
        FDB     c_redex                  ; REDEX
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5C6A   FDB     c_lit_
        FDB     $001E                    ; 30
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5C82            ; ?BRANCH --Z5C82--v
        FDB     c_drop                   ; DROP
        FDB     c_valid                  ; VALID
        FDB     c_1                      ; 1
        FDB     c_vexec                  ; VEXEC
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5C82   FDB     c_lit_
        FDB     $001F                    ; 31
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5C96            ; ?BRANCH --Z5C96--v
        FDB     c_drop                   ; DROP
        FDB     c_debsu                  ; DEB-MIL
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5C96   FDB     c_lit_
        FDB     $007F                    ; 127
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5CAA            ; ?BRANCH --Z5CAA--v
        FDB     c_drop                   ; DROP
        FDB     c_delca                  ; DELCAR
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5CAA   FDB     c_lit_
        FDB     $00FF                    ; 255
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5CBE            ; ?BRANCH --Z5CBE--v
        FDB     c_drop                   ; DROP
        FDB     c_uncom                  ; UNCOM
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5CBE   FDB     c_lit_
        FDB     $00FE                    ; 254
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5CD2            ; ?BRANCH --Z5CD2--v
        FDB     c_drop                   ; DROP
        FDB     c_insca                  ; INSCAR
        FDB     c_branc,Z5CD4            ; BRANCH --Z5CD4--v
Z5CD2   FDB     c_drop                   ; DROP
Z5CD4   FDB     c_semi_                  ; (;)

; ==================================================================
; $$
; ==================================================================
l_doldo FDB     l_is_ls                  ; LFA -> L-VIDE?

n_doldo FCB     $82                      ; NFA -> 2
        FCB     $24,$A4                  ; "$$"

c_doldo FDB     do_col                   ; : $$
p_doldo FDB     c_cr,c_lit_              ; CR
        FDB     $0005                    ; 5
        FDB     c_sp                     ; SP
        FDB     c_semi_                  ; (;)

; ==================================================================
; _WRIT
; ==================================================================
l_writ  FDB     l_comm                   ; LFA -> COMM

n_writ  FCB     $85                      ; NFA -> 5
        FCB     $5F,$57,$52,$49,$D4      ; "_WRIT"

c_writ  FDB     do_col                   ; : _WRIT
p_writ  FDB     c_debu                   ; DEBU
        FDB     c_scpa                   ; SCPA
        FDB     c_ycu                    ; YCU
        FDB     c_sub1                   ; -1
        FDB     c_bqval                  ; `VAL
        FDB     c_exc                    ; !
Z5CFD   FDB     c_key                    ; KEY
        FDB     c_comex                  ; COM!
        FDB     c_dup                    ; DUP
        FDB     c_is_ca                  ; CAR?
        FDB     c_qmbra,Z5D0F            ; ?BRANCH --Z5D0F--v
        FDB     c_ecri                   ; ECRI
        FDB     c_branc,Z5D31            ; BRANCH --Z5D31--v
Z5D0F   FDB     c_dup,c_lit_             ; DUP
        FDB     $0008,c_lit_             ; 8
        FDB     $000D                    ; 13
        FDB     c_eq_                    ; <=>
        FDB     c_qmbra,Z5D21            ; ?BRANCH --Z5D21--v
        FDB     c_dup                    ; DUP
Z5D21   FDB     c_comm                   ; COMM
        FDB     c_bqfin                  ; `FIN
        FDB     c_at                     ; @
        FDB     c_1                      ; 1
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z5D31            ; ?BRANCH --Z5D31--v
        FDB     c_exit                   ; EXIT
Z5D31   FDB     c_branc,Z5CFD            ; BRANCH --Z5CFD--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; TOWRITE
; ==================================================================
l_towri FDB     l_doldo                  ; LFA -> $$

n_towri FCB     $87                      ; NFA -> 7
        FCB     $54,$4F,$57,$52,$49,$54  ; "TOWRIT"
        FCB     $C5                      ; "E"

c_towri FDB     do_col                   ; : TOWRITE
p_towri FDB     c_vec2                   ; VEC2
        FDB     c_at                     ; @
        FDB     c_vec                    ; VEC
        FDB     c_exc                    ; !
        FDB     c_11p                    ; 11P
        FDB     c_writ                   ; _WRIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; _READ
; ==================================================================
l_read  FDB     l_writ                   ; LFA -> _WRIT

n_read  FCB     $85                      ; NFA -> 5
        FCB     $5F,$52,$45,$41,$C4      ; "_READ"

c_read  FDB     do_col                   ; : _READ
p_read  FDB     c_1                      ; 1
        FDB     c_col0                   ; COL
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ldive                  ; L/E
        FDB     c_1add                   ; 1+
        FDB     c_at0                    ; AT
        FDB     c_ycu                    ; YCU
Z5D69   FDB     c_key,c_lit_             ; KEY
        FDB     $001B                    ; 27
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5D7F            ; ?BRANCH --Z5D7F--v
        FDB     c_drop                   ; DROP
        FDB     c_tobot                  ; TOBOT
        FDB     c_branc,Z5DC1            ; BRANCH --Z5DC1--v
Z5D7F   FDB     c_lit_
        FDB     $0008                    ; 8
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5D95            ; ?BRANCH --Z5D95--v
        FDB     c_drop                   ; DROP
        FDB     c_sub1                   ; -1
        FDB     c_vexec                  ; VEXEC
        FDB     c_branc,Z5DC1            ; BRANCH --Z5DC1--v
Z5D95   FDB     c_lit_
        FDB     $0009                    ; 9
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5DAB            ; ?BRANCH --Z5DAB--v
        FDB     c_drop                   ; DROP
        FDB     c_1                      ; 1
        FDB     c_vexec                  ; VEXEC
        FDB     c_branc,Z5DC1            ; BRANCH --Z5DC1--v
Z5DAB   FDB     c_lit_
        FDB     $005F                    ; 95
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z5DBF            ; ?BRANCH --Z5DBF--v
        FDB     c_drop                   ; DROP
        FDB     c_towri                  ; TOWRITE
        FDB     c_branc,Z5DC1            ; BRANCH --Z5DC1--v
Z5DBF   FDB     c_drop                   ; DROP
Z5DC1   FDB     c_branc,Z5D69            ; BRANCH --Z5D69--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; AFE
; ==================================================================
l_afe   FDB     l_ecart                  ; LFA -> ECARTEX

n_afe   FCB     $83                      ; NFA -> 3
        FCB     $41,$46,$C5              ; "AFE"

c_afe   FDB     do_col                   ; : AFE
p_afe   FDB     c_ent                    ; ENT
        FDB     c_ncu                    ; NCU
        FDB     c_1                      ; 1
        FDB     c_2                      ; 2
        FDB     c_cdiv0,c_lit_           ; C/L0
        FDB     $0019                    ; 25
        FDB     c_box                    ; BOX
        FDB     c_scpa,c_lit_            ; SCPA
        FDB     $0009                    ; 9
        FDB     c_pap                    ; PAP
        FDB     c_clc                    ; CLC
        FDB     c_debad                  ; DEBAD
        FDB     c_dup                    ; DUP
        FDB     c_mem                    ; MEM
        FDB     c_nbcar                  ; NBCAR
        FDB     c_2dup                   ; 2DUP
        FDB     c_kecar                  ; KECAR
        FDB     c_11p                    ; 11P
        FDB     c_chain                  ; CHAINE
        FDB     c_0                      ; 0
        FDB     c_bqfin                  ; `FIN
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; READ
; ==================================================================
l_read0 FDB     l_vagtv                  ; LFA -> VA>VIDE

n_read0 FCB     $84                      ; NFA -> 4
        FCB     $52,$45,$41,$C4          ; "READ"

c_read0 FDB     do_col                   ; : READ
p_read0 FDB     c_afe                    ; AFE
        FDB     c_read                   ; _READ
        FDB     c_semi_                  ; (;)

; ==================================================================
; WRIT
; ==================================================================
l_writ0 FDB     l_read                   ; LFA -> _READ

n_writ0 FCB     $84                      ; NFA -> 4
        FCB     $57,$52,$49,$D4          ; "WRIT"

c_writ0 FDB     do_col                   ; : WRIT
p_writ0 FDB     c_afe                    ; AFE
        FDB     c_writ                   ; _WRIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; RE
; ==================================================================
l_re    FDB     l_read0                  ; LFA -> READ

n_re    FCB     $82                      ; NFA -> 2
        FCB     $52,$C5                  ; "RE"

c_re    FDB     do_col                   ; : RE
p_re    FDB     c_lit_
        FDB     $0138                    ; 312
        FDB     c_min                    ; MIN
        FDB     c_clean                  ; CLEAN
        FDB     c_scr                    ; SCR
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_bqval                  ; `VAL
        FDB     c_exc                    ; !
        FDB     c_inipa                  ; INIPAD
        FDB     c_qmbra,Z5E4E,c_lit_     ; ?BRANCH --Z5E4E--v
        FDB     $5E08                    ; ' READ
        FDB     c_vec                    ; VEC
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_vexec                  ; VEXEC
        FDB     c_branc,Z5E50            ; BRANCH --Z5E50--v
Z5E4E   FDB     c_mankm                  ; MANKMEM
Z5E50   FDB     c_semi_                  ; (;)

; ==================================================================
; ERE
; ==================================================================
l_ere   FDB     l_afe                    ; LFA -> AFE

n_ere   FCB     $83                      ; NFA -> 3
        FCB     $45,$52,$C5              ; "ERE"

c_ere   FDB     do_col                   ; : ERE
p_ere   FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_re                     ; RE
        FDB     c_semi_                  ; (;)

; ==================================================================
; WW
; ==================================================================
l_ww    FDB     l_writ0                  ; LFA -> WRIT

n_ww    FCB     $82                      ; NFA -> 2
        FCB     $57,$D7                  ; "WW"

c_ww    FDB     do_col                   ; : WW
p_ww    FDB     c_lit_
        FDB     $0138                    ; 312
        FDB     c_min                    ; MIN
        FDB     c_clean                  ; CLEAN
        FDB     c_scr                    ; SCR
        FDB     c_exc                    ; !
        FDB     c_1                      ; 1
        FDB     c_bqval                  ; `VAL
        FDB     c_exc                    ; !
        FDB     c_inipa                  ; INIPAD
        FDB     c_qmbra,Z5E91,c_lit_     ; ?BRANCH --Z5E91--v
        FDB     $5E17                    ; ' WRIT
        FDB     c_vec                    ; VEC
        FDB     c_exc                    ; !
        FDB     c_0                      ; 0
        FDB     c_vexec                  ; VEXEC
        FDB     c_branc,Z5E93            ; BRANCH --Z5E93--v
Z5E91   FDB     c_mankm                  ; MANKMEM
Z5E93   FDB     c_semi_                  ; (;)

; ==================================================================
; EWR
; ==================================================================
l_ewr   FDB     l_ere                    ; LFA -> ERE

n_ewr   FCB     $83                      ; NFA -> 3
        FCB     $45,$57,$D2              ; "EWR"

c_ewr   FDB     do_col                   ; : EWR
p_ewr   FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_ww                     ; WW
        FDB     c_semi_                  ; (;)

; ==================================================================
; SCA
; ==================================================================
l_sca   FDB     l_ww                     ; LFA -> WW

n_sca   FCB     $83                      ; NFA -> 3
        FCB     $53,$43,$C1              ; "SCA"

c_sca   FDB     do_var                   ; VARIABLE SCA
p_sca   FDB     $0000                    ; 0

; ==================================================================
; NBS
; ==================================================================
l_nbs   FDB     l_re                     ; LFA -> RE

n_nbs   FCB     $83                      ; NFA -> 3
        FCB     $4E,$42,$D3              ; "NBS"

c_nbs   FDB     do_var                   ; VARIABLE NBS
p_nbs   FDB     $0000                    ; 0

; ==================================================================
; CTR
; ==================================================================
l_ctr   FDB     l_sca                    ; LFA -> SCA

n_ctr   FCB     $83                      ; NFA -> 3
        FCB     $43,$54,$D2              ; "CTR"

c_ctr   FDB     do_var                   ; VARIABLE CTR
p_ctr   FDB     $0000                    ; 0

; ==================================================================
; TIT
; ==================================================================
l_tit   FDB     l_towri                  ; LFA -> TOWRITE

n_tit   FCB     $83                      ; NFA -> 3
        FCB     $54,$49,$D4              ; "TIT"

c_tit   FDB     do_const                 ; CONSTANT TIT
p_tit   FDB     $0011                    ; 17

; ==================================================================
; NOMME:
; ==================================================================
l_nomme FDB     l_nbs                    ; LFA -> NBS

n_nomme FCB     $86                      ; NFA -> 6
        FCB     $4E,$4F,$4D,$4D,$45,$BA  ; "NOMME:"

c_nomme FDB     do_col                   ; : NOMME:
p_nomme FDB     c_pad,c_lit_             ; PAD
        FDB     $0014                    ; 20
        FDB     c_blank                  ; BLANK
        FDB     c_dup                    ; DUP
        FDB     c_rot                    ; ROT
        FDB     c_dup                    ; DUP
        FDB     c_sca                    ; SCA
        FDB     c_exc                    ; !
        FDB     c_sub                    ; -
        FDB     c_1add                   ; 1+
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0063                    ; 99
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z5F08            ; ?BRANCH --Z5F08--v
        FDB     c_dotq1                  ; (."F) len=9
        FCB     $09
        FDB     $3939,$206D,$6178,$6920  ; "99 maxi "
        FCB     $21                      ; "!"
        FDB     c_cr                     ; CR
Z5F08   FDB     c_lit_
        FDB     $0063                    ; 99
        FDB     c_min                    ; MIN
        FDB     c_nbs                    ; NBS
        FDB     c_exc                    ; !
        FDB     c_pad,c_lit_             ; PAD
        FDB     $007C                    ; ' !
        FDB     c_over                   ; OVER
        FDB     c_cexc                   ; C!
        FDB     c_dup                    ; DUP
        FDB     c_2add                   ; 2+
        FDB     c_tit                    ; TIT
        FDB     c_expec                  ; EXPECT
        FDB     c_2                      ; 2
        FDB     c_span                   ; SPAN
        FDB     c_addex                  ; +!
        FDB     c_swap                   ; SWAP
        FDB     c_1add                   ; 1+
        FDB     c_sca                    ; SCA
        FDB     c_at                     ; @
        FDB     c_ncu                    ; NCU
        FDB     c_do_,Z5F8C              ; (DO) --Z5F8C--v
Z5F38   FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_block                  ; BLOCK
        FDB     c_dup,c_lit_             ; DUP
        FDB     $001B                    ; 27
        FDB     c_blank                  ; BLANK
        FDB     c_span                   ; SPAN
        FDB     c_at                     ; @
        FDB     c_2dup                   ; 2DUP
        FDB     c_add                    ; +
        FDB     c_2add                   ; 2+
        FDB     c_to_r                   ; >R
        FDB     c_cmove                  ; CMOVE
        FDB     c_from_                  ; R>
        FDB     c_i                      ; I
        FDB     c_sca                    ; SCA
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_1add,c_lit_            ; 1+
        FDB     $0064                    ; ' SP@
        FDB     c_ast                    ; *
        FDB     c_nbs                    ; NBS
        FDB     c_at                     ; @
        FDB     c_add                    ; +
        FDB     c_0                      ; 0
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_,c_lit_              ; #
        FDB     $002F                    ; 47
        FDB     c_hold                   ; HOLD
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_gt                   ; #>
        FDB     c_rot                    ; ROT
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_updat                  ; UPDATE
        FDB     c_loop_,Z5F38            ; (LOOP) --Z5F38--^
Z5F8C   FDB     c_drop                   ; DROP
        FDB     c_saves                  ; SAVE-BUFFERS
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; .IN
; ==================================================================
l_dotin FDB     l_nomme                  ; LFA -> NOMME:

n_dotin FCB     $C3                      ; NFA -> 3 IMMEDIATE
        FCB     $2E,$49,$CE              ; ".IN"

c_dotin FDB     do_col                   ; : .IN
p_dotin FDB     c_termi                  ; TERMINAL
        FDB     c_6p                     ; 6P
        FDB     c_doldo                  ; $$
        FDB     c_dotq1,Z1345,$6E74      ; (."F)
        FDB     $7265,$7220,$756E,p_defi0,$6F6D,$6272,$6520,$3A20
        FDB     c_ycu                    ; YCU
        FDB     c_query                  ; QUERY
        FDB     c_bl                     ; BL
        FDB     c_word                   ; WORD
        FDB     c_numbe                  ; NUMBER
        FDB     c_drop                   ; DROP
        FDB     c_ncu,c_lit_             ; NCU
        FDB     $0006                    ; 6
        FDB     c_n_tib                  ; #TIB
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_2ast                   ; 2*
        FDB     c_sp                     ; SP
        FDB     c_3p                     ; 3P
        FDB     c_tib                    ; TIB
        FDB     c_n_tib                  ; #TIB
        FDB     c_at                     ; @
        FDB     c_type                   ; TYPE
        FDB     c_14p                    ; 14P
        FDB     c_space,c_lit_           ; SPACE
        FDB     $005F                    ; 95
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; SS
; ==================================================================
l_ss    FDB     l_ctr                    ; LFA -> CTR

n_ss    FCB     $82                      ; NFA -> 2
        FCB     $53,$D3                  ; "SS"

c_ss    FDB     do_col                   ; : SS
p_ss    FDB     c_secdi                  ; SEC/TRK
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0014                    ; 20
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z600D            ; ?BRANCH --Z600D--v
        FDB     c_2div,c_lit_            ; 2/
        FDB     $1D6F                    ; ' SEC/TRK
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc                    ; !
        FDB     c_branc,Z600F            ; BRANCH --Z600F--v
Z600D   FDB     c_drop                   ; DROP
Z600F   FDB     c_semi_                  ; (;)

; ==================================================================
; DS
; ==================================================================
l_ds    FDB     l_tit                    ; LFA -> TIT

n_ds    FCB     $82                      ; NFA -> 2
        FCB     $44,$D3                  ; "DS"

c_ds    FDB     do_col                   ; : DS
p_ds    FDB     c_secdi                  ; SEC/TRK
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0011                    ; 17
        FDB     c_lt                     ; <
        FDB     c_qmbra,Z6034            ; ?BRANCH --Z6034--v
        FDB     c_2ast,c_lit_            ; 2*
        FDB     $1D6F                    ; ' SEC/TRK
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc                    ; !
        FDB     c_branc,Z6036            ; BRANCH --Z6036--v
Z6034   FDB     c_drop                   ; DROP
Z6036   FDB     c_semi_                  ; (;)

; ==================================================================
; TRAIT
; ==================================================================
l_trait FDB     l_ds                     ; LFA -> DS

n_trait FCB     $85                      ; NFA -> 5
        FCB     $54,$52,$41,$49,$D4      ; "TRAIT"

c_trait FDB     do_col                   ; : TRAIT
p_trait FDB     c_6p                     ; 6P
        FDB     c_cr,c_lit_              ; CR
        FDB     $0027                    ; ' PAUSE
        FDB     c_0                      ; 0
        FDB     c_do_,Z605A              ; (DO) --Z605A--v
Z6050   FDB     c_lit_
        FDB     $002D                    ; 45
        FDB     c_tild                   ; ~
        FDB     c_loop_,Z6050            ; (LOOP) --Z6050--^
Z605A   FDB     c_cr                     ; CR
        FDB     c_14p                    ; 14P
        FDB     c_semi_                  ; (;)

; ==================================================================
; BST
; ==================================================================
l_bst   FDB     l_dotin                  ; LFA -> .IN

n_bst   FCB     $83                      ; NFA -> 3
        FCB     $42,$53,$D4              ; "BST"

c_bst   FDB     do_col                   ; : BST
p_bst   FDB     c_lit_
        FDB     $1D53                    ; ' TRK/DRV
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc,c_lit_             ; !
        FDB     $1D6F                    ; ' SEC/TRK
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc,c_lit_             ; !
        FDB     $1D61                    ; ' BASETRK
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; DFDD
; ==================================================================
l_dfdd  FDB     l_trait                  ; LFA -> TRAIT

n_dfdd  FCB     $84                      ; NFA -> 4
        FCB     $44,$46,$44,$C4          ; "DFDD"

c_dfdd  FDB     do_col                   ; : DFDD
p_dfdd  FDB     c_1,c_lit_               ; 1
        FDB     $0020,c_lit_             ; 32
        FDB     $0028                    ; 40
        FDB     c_bst                    ; BST
        FDB     c_semi_                  ; (;)

; ==================================================================
; SFDD
; ==================================================================
l_sfdd  FDB     l_ss                     ; LFA -> SS

n_sfdd  FCB     $84                      ; NFA -> 4
        FCB     $53,$46,$44,$C4          ; "SFDD"

c_sfdd  FDB     do_col                   ; : SFDD
p_sfdd  FDB     c_1,c_lit_               ; 1
        FDB     $0010,c_lit_             ; 16
        FDB     $0028                    ; 40
        FDB     c_bst                    ; BST
        FDB     c_semi_                  ; (;)

; ==================================================================
; SFSD
; ==================================================================
l_sfsd  FDB     l_sfdd                   ; LFA -> SFDD

n_sfsd  FCB     $84                      ; NFA -> 4
        FCB     $53,$46,$53,$C4          ; "SFSD"

c_sfsd  FDB     do_col                   ; : SFSD
p_sfsd  FDB     c_0,c_lit_               ; 0
        FDB     $000A,c_lit_             ; 10
        FDB     $0023                    ; 35
        FDB     c_bst                    ; BST
        FDB     c_semi_                  ; (;)

; ==================================================================
; CR8
; ==================================================================
l_cr8   FDB     l_sfsd                   ; LFA -> SFSD

n_cr8   FCB     $83                      ; NFA -> 3
        FCB     $43,$52,$B8              ; "CR8"

c_cr8   FDB     do_col                   ; : CR8
p_cr8   FDB     c_cr,c_lit_              ; CR
        FDB     $0008                    ; 8
        FDB     c_sp                     ; SP
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?DON
; ==================================================================
l_qmdon FDB     l_cr8                    ; LFA -> CR8

n_qmdon FCB     $84                      ; NFA -> 4
        FCB     $3F,$44,$4F,$CE          ; "?DON"

c_qmdon FDB     do_col                   ; : ?DON
p_qmdon FDB     c_cls                    ; CLS
        FDB     c_ncu                    ; NCU
        FDB     c_trait                  ; TRAIT
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $5069,$7374,$6520,$6465  ; "Piste de"
        FDB     $2062,$6173,$6520,$3A20  ; " base : "
        FDB     c_baset,c_lit_           ; BASETRK
        FDB     $0004                    ; 4
        FDB     c_dotr                   ; .R
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $4E62,$2053,$6563,$742F  ; "Nb Sect/"
        FDB     $5069,$7374,$6520,$3A20  ; "Piste : "
        FDB     c_secdi,c_lit_           ; SEC/TRK
        FDB     $0004                    ; 4
        FDB     c_dotr                   ; .R
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $4E62,$2050,$6973,$742F  ; "Nb Pist/"
        FDB     $4472,$6976,$6520,$3A20  ; "Drive : "
        FDB     c_trkdi,c_lit_           ; TRK/DRV
        FDB     $0004                    ; 4
        FDB     c_dotr                   ; .R
        FDB     c_hex                    ; HEX
        FDB     c_trait                  ; TRAIT
        FDB     c_here                   ; HERE
        FDB     c_1sub                   ; 1-
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $4845,$5245,$2031,$2D20  ; "HERE 1- "
        FDB     $2848,$6578,$2920,$3A20  ; "(Hex) : "
        FDB     c_udot                   ; U.
        FDB     c_dec0                   ; DEC0
        FDB     c_3p                     ; 3P
        FDB     c_cr8                    ; CR8
        FDB     c_dotq1                  ; (."F) len=28
        FCB     $1C
        FDB     $5361,$7576,$6567,$6172  ; "Sauvegar"
        FDB     $6465,$206E,$6F79,$6175  ; "de noyau"
        FDB     $2073,$6F75,$7320,$466C  ; " sous Fl"
        FCB     $65,$78,$20,$3A          ; "ex :"
        FDB     c_cr8                    ; CR8
        FDB     c_dotq1                  ; (."F) len=30
        FCB     $1E
        FDB     $2B2B,$2B73,$6176,$6520  ; "+++save "
        FDB     $302E,$666F,$7274,$682E  ; "0.forth."
        FDB     $636D,$6420,$3020,$4845  ; "cmd 0 HE"
        FCB     $52,$45,$2D,$31,$20,$30  ; "RE-1 0"
        FDB     c_cr8                    ; CR8
        FDB     c_dotq1                  ; (."F) len=12
        FCB     $0C
        FDB     $2B2B,$2B6A,$756D,$7020  ; "+++jump "
        FCB     $30,$30,$30,$33          ; "0003"
        FDB     c_14p                    ; 14P
        FDB     c_cr8                    ; CR8
        FDB     c_dotq1                  ; (."F) len=24
        FCB     $18
        FDB     $2850,$6F75,$7220,$7265  ; "(Pour re"
        FDB     $746F,$7572,$2073,$6F75  ; "tour sou"
        FDB     $7320,$466F,$7274,$6829  ; "s Forth)"
        FDB     c_trait                  ; TRAIT
        FDB     c_offse                  ; OFFSET
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $4472,$6976,$6520,$7472  ; "Drive tr"
        FDB     $6176,$6169,$6C20,$3A20  ; "avail : "
        FDB     c_qmbra,Z61F7            ; ?BRANCH --Z61F7--v
        FDB     c_0                      ; 0
        FDB     c_branc,Z61F9            ; BRANCH --Z61F9--v
Z61F7   FDB     c_1                      ; 1
Z61F9   FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_dotr                   ; .R
        FDB     c_trait                  ; TRAIT
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; <DO-BL>
; ==================================================================
l_dosub FDB     l_dfdd                   ; LFA -> DFDD

n_dosub FCB     $87                      ; NFA -> 7
        FCB     $3C,$44,$4F,$2D,$42,$4C  ; "<DO-BL"
        FCB     $BE                      ; ">"

c_dosub FDB     do_col                   ; : <DO-BL>
p_dosub FDB     c_ncu                    ; NCU
        FDB     c_dup                    ; DUP
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z621F            ; ?BRANCH --Z621F--v
        FDB     c_drop                   ; DROP
        FDB     c_1                      ; 1
Z621F   FDB     c_do_,Z6249              ; (DO) --Z6249--v
Z6223   FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=9
        FCB     $09
        FDB     $426C,$616E,$6368,$6974  ; "Blanchit"
        FCB     $20                      ; " "
        FDB     c_i,c_lit_               ; I
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_i                      ; I
        FDB     c_block                  ; BLOCK
        FDB     c_bdivb                  ; B/BUF
        FDB     c_blank                  ; BLANK
        FDB     c_updat                  ; UPDATE
        FDB     c_saves                  ; SAVE-BUFFERS
        FDB     c_loop_,Z6223            ; (LOOP) --Z6223--^
Z6249   FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; BLDSK
; ==================================================================
l_bldsk FDB     l_bst                    ; LFA -> BST

n_bldsk FCB     $85                      ; NFA -> 5
        FCB     $42,$4C,$44,$53,$CB      ; "BLDSK"

c_bldsk FDB     do_col                   ; : BLDSK
p_bldsk FDB     c_cls                    ; CLS
        FDB     c_dotq1                  ; (."F) len=29
        FCB     $1D
        FDB     $4174,$7465,$6E74,$696F  ; "Attentio"
        FDB     $6E20,$5379,$7374,$846D  ; "n Systèm"
        FDB     $6520,$656E,$2044,$5230  ; "e en DR0"
        FCB     $3F,$20,$4F,$4B,$3F      ; "? OK?"
        FDB     c_key,c_lit_             ; KEY
        FDB     $006F                    ; 111
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z628D            ; ?BRANCH --Z628D--v
        FDB     c_dr1                    ; DR1
        FDB     c_blkdi                  ; BLK/DRV
        FDB     c_1                      ; 1
        FDB     c_dosub                  ; <DO-BL>
Z628D   FDB     c_semi_                  ; (;)

; ==================================================================
; BLSCR
; ==================================================================
l_blscr FDB     l_bldsk                  ; LFA -> BLDSK

n_blscr FCB     $85                      ; NFA -> 5
        FCB     $42,$4C,$53,$43,$D2      ; "BLSCR"

c_blscr FDB     do_col                   ; : BLSCR
p_blscr FDB     c_dr1                    ; DR1
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_dosub                  ; <DO-BL>
        FDB     c_semi_                  ; (;)

; ==================================================================
; WIPE0
; ==================================================================
l_wipe0 FDB     l_qmdon                  ; LFA -> ?DON

n_wipe0 FCB     $84                      ; NFA -> 4
        FCB     $57,$49,$50,$C5          ; "WIPE"

c_wipe0 FDB     do_col                   ; : WIPE0
p_wipe0 FDB     c_dr1                    ; DR1
        FDB     c_scr                    ; SCR
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_blscr                  ; BLSCR
        FDB     c_semi_                  ; (;)

; ==================================================================
; COPIES
; ==================================================================
l_copie FDB     l_wipe0                  ; LFA -> WIPE0

n_copie FCB     $86                      ; NFA -> 6
        FCB     $43,$4F,$50,$49,$45,$D3  ; "COPIES"

c_copie FDB     do_col                   ; : COPIES
p_copie FDB     c_subro                  ; -ROT
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_ncu                    ; NCU
        FDB     c_do_,Z6302              ; (DO) --Z6302--v
Z62CF   FDB     c_i                      ; I
        FDB     c_over                   ; OVER
        FDB     c_2dup                   ; 2DUP
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $43,$6F,$70,$69,$65,$20  ; "Copie "
        FDB     c_swap,c_lit_            ; SWAP
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_dotq1                  ; (."F) len=5
        FCB     $05
        FCB     $20,$73,$75,$72,$20      ; " sur "
        FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_copy                   ; COPY
        FDB     c_updat                  ; UPDATE
        FDB     c_saves                  ; SAVE-BUFFERS
        FDB     c_1add                   ; 1+
        FDB     c_loop_,Z62CF            ; (LOOP) --Z62CF--^
Z6302   FDB     c_flush                  ; FLUSH
        FDB     c_drop                   ; DROP
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; SAUVE
; ==================================================================
l_sauve FDB     l_copie                  ; LFA -> COPIES

n_sauve FCB     $85                      ; NFA -> 5
        FCB     $53,$41,$55,$56,$C5      ; "SAUVE"

c_sauve FDB     do_col                   ; : SAUVE
p_sauve FDB     c_cls                    ; CLS
        FDB     c_ncu,c_lit_             ; NCU
        FDB     $0004                    ; 4
        FDB     c_al                     ; AL
        FDB     c_2,c_lit_               ; 2
        FDB     $0005,c_lit_             ; 5
        FDB     $0027,c_lit_             ; ' PAUSE
        FDB     $0023,c_lit_             ; 35
        FDB     $000C,c_lit_             ; 12
        FDB     $0003                    ; 3
        FDB     c_win                    ; WIN
        FDB     c_clc                    ; CLC
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=20
        FCB     $14
        FDB     $4469,$736B,$2054,$5241  ; "Disk TRA"
        FDB     $5641,$494C,$2065,$6E20  ; "VAIL en "
        FCB     $44,$52,$30,$3F          ; "DR0?"
        FDB     c_ydivn                  ; Y/N
        FDB     c_14p,c_lit_             ; 14P
        FDB     $0003                    ; 3
        FDB     c_al                     ; AL
        FDB     c_qmbra,Z63F2            ; ?BRANCH --Z63F2--v
        FDB     c_dotq1                  ; (."F) len=23
        FCB     $17
        FDB     $5072,$656D,$6965,$7220  ; "Premier "
        FDB     $8063,$7261,$6E20,$8720  ; "écran à "
        FCB     $63,$6F,$70,$69,$65,$72  ; "copier"
        FCB     $3F                      ; "?"
        FDB     c_dotin                  ; .IN
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=23
        FCB     $17
        FDB     $4465,$726E,$6965,$7220  ; "Dernier "
        FDB     $8063,$7261,$6E20,$8720  ; "écran à "
        FCB     $63,$6F,$70,$69,$65,$72  ; "copier"
        FCB     $3F                      ; "?"
        FDB     c_dotin                  ; .IN
        FDB     c_cr                     ; CR
        FDB     c_dr0                    ; DR0
        FDB     c_over,c_lit_            ; OVER
        FDB     $0138                    ; 312
        FDB     c_add                    ; +
        FDB     c_copie                  ; COPIES
        FDB     c_dr1                    ; DR1
        FDB     c_3p,c_lit_              ; 3P
        FDB     $0004                    ; 4
        FDB     c_al                     ; AL
        FDB     c_dotq1                  ; (."F) len=27
        FCB     $1B
        FDB     $5361,$7576,$6567,$6172  ; "Sauvegar"
        FDB     $6465,$2074,$6572,$6D69  ; "de termi"
        FDB     $6E80,$652C,$2072,$6570  ; "née, rep"
        FCB     $6C,$61,$2D              ; "la-"
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=26
        FCB     $1A
        FDB     $6365,$7220,$6C65,$2073  ; "cer le s"
        FDB     $7973,$7484,$6D65,$2065  ; "ystème e"
        FDB     $6E20,$6472,$6976,$6520  ; "n drive "
        FCB     $30,$2E                  ; "0."
        FDB     c_branc,Z6423            ; BRANCH --Z6423--v
Z63F2   FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_sp                     ; SP
        FDB     c_3p                     ; 3P
        FDB     c_dotq1                  ; (."F) len=9
        FCB     $09
        FDB     $4D45,$5453,$2D4C,$452C  ; "METS-LE,"
        FCB     $20                      ; " "
        FDB     c_4p                     ; 4P
        FDB     c_dotq1                  ; (."F) len=18
        FCB     $12
        FDB     $6372,$906D,$6520,$6427  ; "crême d'"
        FDB     $616E,$646F,$7569,$6C6C  ; "andouill"
        FCB     $65,$21                  ; "e!"
        FDB     c_2                      ; 2
        FDB     c_dela0                  ; DELAI
        FDB     c_sauve                  ; SAUVE
Z6423   FDB     c_home,c_lit_            ; HOME
        FDB     $0009                    ; 9
        FDB     c_pap                    ; PAP
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; INITMT
; ==================================================================
l_initm FDB     l_ewr                    ; LFA -> EWR

n_initm FCB     $86                      ; NFA -> 6
        FCB     $49,$4E,$49,$54,$4D,$D4  ; "INITMT"

c_initm FDB     do_col                   ; : INITMT
p_initm FDB     c_print                  ; PRINTER
        FDB     c_dec0                   ; DEC0
        FDB     c_us,c_lit_              ; US
        FDB     $001B                    ; 27
        FDB     c_tild,c_lit_            ; ~
        FDB     $0040                    ; 64
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; SEPA
; ==================================================================
l_sepa  FDB     l_sauve                  ; LFA -> SAUVE

n_sepa  FCB     $84                      ; NFA -> 4
        FCB     $53,$45,$50,$C1          ; "SEPA"

c_sepa  FDB     do_col                   ; : SEPA
p_sepa  FDB     c_lit_
        FDB     $000B                    ; 11
        FDB     c_sp                     ; SP
        FDB     c_semi_                  ; (;)

; ==================================================================
; CARETR
; ==================================================================
l_caret FDB     l_sepa                   ; LFA -> SEPA

n_caret FCB     $86                      ; NFA -> 6
        FCB     $43,$41,$52,$45,$54,$D2  ; "CARETR"

c_caret FDB     do_col                   ; : CARETR
p_caret FDB     c_lit_
        FDB     $000F                    ; 15
        FDB     c_ptild                  ; P~
        FDB     c_semi_                  ; (;)

; ==================================================================
; CARNOR
; ==================================================================
l_carno FDB     l_caret                  ; LFA -> CARETR

n_carno FCB     $86                      ; NFA -> 6
        FCB     $43,$41,$52,$4E,$4F,$D2  ; "CARNOR"

c_carno FDB     do_col                   ; : CARNOR
p_carno FDB     c_lit_
        FDB     $0012                    ; 18
        FDB     c_ptild                  ; P~
        FDB     c_semi_                  ; (;)

; ==================================================================
; PRTIT
; ==================================================================
l_prtit FDB     l_dosub                  ; LFA -> <DO-BL>

n_prtit FCB     $85                      ; NFA -> 5
        FCB     $50,$52,$54,$49,$D4      ; "PRTIT"

c_prtit FDB     do_col                   ; : PRTIT
p_prtit FDB     c_dup                    ; DUP
        FDB     c_print                  ; PRINTER
        FDB     c_dotq1                  ; (."F) len=9
        FCB     $09
        FDB     $4563,$7261,$6E20,$4E6F  ; "Ecran No"
        FCB     $20                      ; " "
        FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_space                  ; SPACE
        FDB     c_block                  ; BLOCK
        FDB     c_csubn                  ; C-NAME
        FDB     c_ftype                  ; FTYPE
        FDB     c_semi_                  ; (;)

; ==================================================================
; PRINT
; ==================================================================
l_prin0 FDB     l_prtit                  ; LFA -> PRTIT

n_prin0 FCB     $85                      ; NFA -> 5
        FCB     $50,$52,$49,$4E,$D4      ; "PRINT"

c_prin0 FDB     do_col                   ; : PRINT
p_prin0 FDB     c_nbs                    ; NBS
        FDB     c_at                     ; @
        FDB     c_qmbra,Z64C9            ; ?BRANCH --Z64C9--v
        FDB     c_caret                  ; CARETR
        FDB     c_branc,Z64CB            ; BRANCH --Z64CB--v
Z64C9   FDB     c_carno                  ; CARNOR
Z64CB   FDB     c_dup                    ; DUP
        FDB     c_prtit                  ; PRTIT
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_deb                    ; DEB
        FDB     c_ldive                  ; L/E
        FDB     c_0                      ; 0
        FDB     c_do_,Z64ED              ; (DO) --Z64ED--v
Z64DD   FDB     c_dup                    ; DUP
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ftype                  ; FTYPE
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_cr                     ; CR
        FDB     c_loop_,Z64DD            ; (LOOP) --Z64DD--^
Z64ED   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; SAVE
; ==================================================================
l_save  FDB     l_carno                  ; LFA -> CARNOR

n_save  FCB     $84                      ; NFA -> 4
        FCB     $53,$41,$56,$C5          ; "SAVE"

c_save  FDB     do_col                   ; : SAVE
p_save  FDB     c_qmdon                  ; ?DON
        FDB     c_flex                   ; FLEX
        FDB     c_semi_                  ; (;)

; ==================================================================
; 2PRINT
; ==================================================================
l_2prin FDB     l_blscr                  ; LFA -> BLSCR

n_2prin FCB     $86                      ; NFA -> 6
        FCB     $32,$50,$52,$49,$4E,$D4  ; "2PRINT"

c_2prin FDB     do_col                   ; : 2PRINT
p_2prin FDB     c_dup                    ; DUP
        FDB     c_cr                     ; CR
        FDB     c_caret                  ; CARETR
        FDB     c_prtit                  ; PRTIT
        FDB     c_sepa                   ; SEPA
        FDB     c_dup                    ; DUP
        FDB     c_1add                   ; 1+
        FDB     c_prtit                  ; PRTIT
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_dup                    ; DUP
        FDB     c_deb                    ; DEB
        FDB     c_swap                   ; SWAP
        FDB     c_1add                   ; 1+
        FDB     c_deb                    ; DEB
        FDB     c_ldive                  ; L/E
        FDB     c_0                      ; 0
        FDB     c_do_,Z6551              ; (DO) --Z6551--v
Z6531   FDB     c_over                   ; OVER
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ftype                  ; FTYPE
        FDB     c_sepa                   ; SEPA
        FDB     c_dup                    ; DUP
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ftype                  ; FTYPE
        FDB     c_cr                     ; CR
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_loop_,Z6531            ; (LOOP) --Z6531--^
Z6551   FDB     c_2drop                  ; 2DROP
        FDB     c_carno                  ; CARNOR
        FDB     c_semi_                  ; (;)

; ==================================================================
; 3PRINT
; ==================================================================
l_3prin FDB     l_save                   ; LFA -> SAVE

n_3prin FCB     $86                      ; NFA -> 6
        FCB     $33,$50,$52,$49,$4E,$D4  ; "3PRINT"

c_3prin FDB     do_col                   ; : 3PRINT
p_3prin FDB     c_to_r                   ; >R
        FDB     c_caret                  ; CARETR
        FDB     c_rat                    ; R@
        FDB     c_prtit                  ; PRTIT
        FDB     c_sepa                   ; SEPA
        FDB     c_rat                    ; R@
        FDB     c_1add                   ; 1+
        FDB     c_prtit                  ; PRTIT
        FDB     c_sepa                   ; SEPA
        FDB     c_rat                    ; R@
        FDB     c_2add                   ; 2+
        FDB     c_prtit                  ; PRTIT
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_rat                    ; R@
        FDB     c_deb                    ; DEB
        FDB     c_rat                    ; R@
        FDB     c_1add                   ; 1+
        FDB     c_deb                    ; DEB
        FDB     c_from_                  ; R>
        FDB     c_2add                   ; 2+
        FDB     c_deb                    ; DEB
        FDB     c_ldive                  ; L/E
        FDB     c_0                      ; 0
        FDB     c_do_,Z65C4              ; (DO) --Z65C4--v
Z6596   FDB     c_2                      ; 2
        FDB     c_pick                   ; PICK
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ftype                  ; FTYPE
        FDB     c_sepa                   ; SEPA
        FDB     c_over                   ; OVER
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ftype                  ; FTYPE
        FDB     c_sepa                   ; SEPA
        FDB     c_dup                    ; DUP
        FDB     c_cdiv0                  ; C/L0
        FDB     c_ftype                  ; FTYPE
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_rot                    ; ROT
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_rot                    ; ROT
        FDB     c_cdiv0                  ; C/L0
        FDB     c_add                    ; +
        FDB     c_rot                    ; ROT
        FDB     c_loop_,Z6596            ; (LOOP) --Z6596--^
Z65C4   FDB     c_cr                     ; CR
        FDB     c_2drop                  ; 2DROP
        FDB     c_drop                   ; DROP
        FDB     c_carno                  ; CARNOR
        FDB     c_semi_                  ; (;)

; ==================================================================
; PR<3
; ==================================================================
l_prlt3 FDB     l_prin0                  ; LFA -> PRINT

n_prlt3 FCB     $84                      ; NFA -> 4
        FCB     $50,$52,$3C,$B3          ; "PR<3"

c_prlt3 FDB     do_col                   ; : PR<3
p_prlt3 FDB     c_2                      ; 2
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z65E9            ; ?BRANCH --Z65E9--v
        FDB     c_sca                    ; SCA
        FDB     c_at                     ; @
        FDB     c_2prin                  ; 2PRINT
        FDB     c_branc,Z65EF            ; BRANCH --Z65EF--v
Z65E9   FDB     c_sca                    ; SCA
        FDB     c_at                     ; @
        FDB     c_prin0                  ; PRINT
Z65EF   FDB     c_semi_                  ; (;)

; ==================================================================
; FAIT=
; ==================================================================
l_faite FDB     l_2prin                  ; LFA -> 2PRINT

n_faite FCB     $85                      ; NFA -> 5
        FCB     $46,$41,$49,$54,$BD      ; "FAIT="

c_faite FDB     do_col                   ; : FAIT=
p_faite FDB     c_caret,c_lit_           ; CARETR
        FDB     $008E                    ; 142
        FDB     c_0                      ; 0
        FDB     c_do_,Z6611              ; (DO) --Z6611--v
Z6607   FDB     c_lit_
        FDB     $003D                    ; 61
        FDB     c_ptild                  ; P~
        FDB     c_loop_,Z6607            ; (LOOP) --Z6607--^
Z6611   FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_semi_                  ; (;)

; ==================================================================
; .MT
; ==================================================================
l_dotmt FDB     l_faite                  ; LFA -> FAIT=

n_dotmt FCB     $83                      ; NFA -> 3
        FCB     $2E,$4D,$D4              ; ".MT"

c_dotmt FDB     do_col                   ; : .MT
p_dotmt FDB     c_initm                  ; INITMT
        FDB     c_0                      ; 0
        FDB     c_ctr                    ; CTR
        FDB     c_exc                    ; !
        FDB     c_over                   ; OVER
        FDB     c_sca                    ; SCA
        FDB     c_exc                    ; !
        FDB     c_swap                   ; SWAP
        FDB     c_sub                    ; -
        FDB     c_1add,c_lit_            ; 1+
        FDB     $001B                    ; 27
        FDB     c_ptild                  ; P~
        FDB     c_dec0,c_lit_            ; DEC0
        FDB     $0051                    ; 81
        FDB     c_ptild,c_lit_           ; P~
        FDB     $008E                    ; 142
        FDB     c_ptild                  ; P~
        FDB     c_faite                  ; FAIT=
        FDB     c_nbs                    ; NBS
        FDB     c_exc                    ; !
Z664D   FDB     c_nbs                    ; NBS
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_2                      ; 2
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z669B            ; ?BRANCH --Z669B--v
        FDB     c_drop                   ; DROP
        FDB     c_sca                    ; SCA
        FDB     c_at                     ; @
        FDB     c_3prin,c_lit_           ; 3PRINT
        FDB     $FFFD                    ; -3
        FDB     c_nbs                    ; NBS
        FDB     c_addex,c_lit_           ; +!
        FDB     $0003                    ; 3
        FDB     c_ctr                    ; CTR
        FDB     c_addex,c_lit_           ; +!
        FDB     $0003                    ; 3
        FDB     c_sca                    ; SCA
        FDB     c_addex                  ; +!
        FDB     c_cr                     ; CR
        FDB     c_faite                  ; FAIT=
        FDB     c_ctr                    ; CTR
        FDB     c_at,c_lit_              ; @
        FDB     $0009                    ; 9
        FDB     c_mod                    ; MOD
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z6697,c_lit_     ; ?BRANCH --Z6697--v
        FDB     $0007                    ; 7
        FDB     c_al                     ; AL
        FDB     c_faite                  ; FAIT=
Z6697   FDB     c_branc,Z664D            ; BRANCH --Z664D--^
Z669B   FDB     c_dup                    ; DUP
        FDB     c_0gt                    ; 0>
        FDB     c_qmbra,Z66AB            ; ?BRANCH --Z66AB--v
        FDB     c_prlt3                  ; PR<3
        FDB     c_faite                  ; FAIT=
        FDB     c_branc,Z66AD            ; BRANCH --Z66AD--v
Z66AB   FDB     c_drop                   ; DROP
Z66AD   FDB     c_carno                  ; CARNOR
        FDB     c_semi_                  ; (;)

; ==================================================================
; CAR~
; ==================================================================
l_carti FDB     l_3prin                  ; LFA -> 3PRINT

n_carti FCB     $84                      ; NFA -> 4
        FCB     $43,$41,$52,$FE          ; "CAR~"

c_carti FDB     do_col                   ; : CAR~
p_carti FDB     c_dup                    ; DUP
        FDB     c_is_ca                  ; CAR?
        FDB     c_not                    ; NOT
        FDB     c_qmbra,Z66CA            ; ?BRANCH --Z66CA--v
        FDB     c_drop,c_lit_            ; DROP
        FDB     $002E                    ; 46
Z66CA   FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; SYSDT
; ==================================================================
l_sysdt FDB     l_carti                  ; LFA -> CAR~

n_sysdt FCB     $85                      ; NFA -> 5
        FCB     $53,$59,$53,$44,$D4      ; "SYSDT"

c_sysdt FDB     do_const                 ; CONSTANT SYSDT
p_sysdt FDB     SYSDAT                   ; 52238

; ==================================================================
; ?N
; ==================================================================
l_qmn   FDB     l_sysdt                  ; LFA -> SYSDT

n_qmn   FCB     $82                      ; NFA -> 2
        FCB     $3F,$CE                  ; "?N"

c_qmn   FDB     do_col                   ; : ?N
p_qmn   FDB     c_dotq1                  ; (."F) len=8
        FCB     $08
        FDB     $2832,$2044,$6967,$6974  ; "(2 Digit"
        FDB     c_dotq1                  ; (."F) len=3
        FCB     $03
        FCB     $73,$29,$20              ; "s) "
        FDB     c_dotin                  ; .IN
        FDB     c_semi_                  ; (;)

; ==================================================================
; N>P
; ==================================================================
l_ngtp  FDB     l_dotmt                  ; LFA -> .MT

n_ngtp  FCB     $83                      ; NFA -> 3
        FCB     $4E,$3E,$D0              ; "N>P"

c_ngtp  FDB     do_col                   ; : N>P
p_ngtp  FDB     c_qmn                    ; ?N
        FDB     c_hex                    ; HEX
        FDB     c_pad                    ; PAD
        FDB     c_rot                    ; ROT
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; >SQ
; ==================================================================
l_to_sq FDB     l_ngtp                   ; LFA -> N>P

n_to_sq FCB     $83                      ; NFA -> 3
        FCB     $3E,$53,$D1              ; ">SQ"

c_to_sq FDB     do_col                   ; : >SQ
p_to_sq FDB     c_sgtd                   ; S>D
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_gt                   ; #>
        FDB     c_rot                    ; ROT
        FDB     c_swap                   ; SWAP
        FDB     c_cmove                  ; CMOVE
        FDB     c_semi_                  ; (;)

; ==================================================================
; ACDAT
; ==================================================================
l_acdat FDB     l_initm                  ; LFA -> INITMT

n_acdat FCB     $85                      ; NFA -> 5
        FCB     $41,$43,$44,$41,$D4      ; "ACDAT"

c_acdat FDB     do_col                   ; : ACDAT
p_acdat FDB     c_datet,c_lit_           ; DATE'
        FDB     $0003                    ; 3
        FDB     c_add                    ; +
        FDB     c_sysdt                  ; SYSDT
        FDB     c_cat                    ; C@
        FDB     c_to_sq                  ; >SQ
        FDB     c_datet                  ; DATE'
        FDB     c_sysdt                  ; SYSDT
        FDB     c_1add                   ; 1+
        FDB     c_cat                    ; C@
        FDB     c_to_sq                  ; >SQ
        FDB     c_datet,c_lit_           ; DATE'
        FDB     $0006                    ; 6
        FDB     c_add                    ; +
        FDB     c_sysdt                  ; SYSDT
        FDB     c_2add                   ; 2+
        FDB     c_cat                    ; C@
        FDB     c_to_sq                  ; >SQ
        FDB     c_semi_                  ; (;)

; ==================================================================
; DODAT
; ==================================================================
l_dodat FDB     l_prlt3                  ; LFA -> PR<3

n_dodat FCB     $85                      ; NFA -> 5
        FCB     $44,$4F,$44,$41,$D4      ; "DODAT"

c_dodat FDB     do_col                   ; : DODAT
p_dodat FDB     c_cls                    ; CLS
        FDB     c_ncu,c_lit_             ; NCU
        FDB     $0006                    ; 6
        FDB     c_al                     ; AL
        FDB     c_1p                     ; 1P
        FDB     c_date                   ; DATE
        FDB     c_ncu                    ; NCU
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_14p                    ; 14P
        FDB     c_dotq1                  ; (."F) len=26
        FCB     $1A
        FDB     $456E,$7472,$6572,$206C  ; "Entrer l"
        FDB     $6120,$6E6F,$7576,$656C  ; "a nouvel"
        FDB     $6C65,$2064,$6174,$6520  ; "le date "
        FCB     $3A,$20                  ; ": "
        FDB     c_cr                     ; CR
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $4A,$6F,$75,$72,$3F,$20  ; "Jour? "
        FCB     $20                      ; " "
        FDB     c_1                      ; 1
        FDB     c_ngtp                   ; N>P
        FDB     c_dec0                   ; DEC0
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $4D,$6F,$69,$73,$3F,$20  ; "Mois? "
        FCB     $20                      ; " "
        FDB     c_0                      ; 0
        FDB     c_ngtp                   ; N>P
        FDB     c_dec0                   ; DEC0
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $41,$6E,$6E,$80,$65,$3F  ; "Année?"
        FCB     $20                      ; " "
        FDB     c_2                      ; 2
        FDB     c_ngtp                   ; N>P
        FDB     c_pad                    ; PAD
        FDB     c_sysdt,c_lit_           ; SYSDT
        FDB     $0003                    ; 3
        FDB     c_cmove                  ; CMOVE
        FDB     c_dec0                   ; DEC0
        FDB     c_acdat                  ; ACDAT
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_1p                     ; 1P
        FDB     c_date                   ; DATE
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_6p                     ; 6P
        FDB     c_dotq1                  ; (."F) len=30
        FCB     $1E
        FDB     $4D65,$7263,$6921,$2041  ; "Merci! A"
        FDB     $2076,$6F75,$732C,$2065  ; " vous, e"
        FDB     $7420,$626F,$6E20,$7472  ; "t bon tr"
        FCB     $61,$76,$61,$69,$6C,$21  ; "avail!"
        FDB     c_vbdat                  ; VBDAT
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_cr                     ; CR
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; <5D>
; ==================================================================
l_5d_   FDB     l_dodat                  ; LFA -> DODAT

n_5d_   FCB     $84                      ; NFA -> 4
        FCB     $3C,$35,$44,$BE          ; "<5D>"

c_5d_   FDB     do_col                   ; : <5D>
p_5d_   FDB     c_3p                     ; 3P
        FDB     c_0                      ; 0
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_gt                   ; #>
        FDB     c_type                   ; TYPE
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z6845            ; ?BRANCH --Z6845--v
        FDB     c_bl                     ; BL
        FDB     c_ptild                  ; P~
        FDB     c_branc,Z684F            ; BRANCH --Z684F--v
Z6845   FDB     c_space,c_lit_           ; SPACE
        FDB     $FFFE                    ; -2
        FDB     c_out                    ; OUT
        FDB     c_addex                  ; +!
Z684F   FDB     c_semi_                  ; (;)

; ==================================================================
; DECDSPLY3
; ==================================================================
l_decds FDB     l_5d_                    ; LFA -> <5D>

n_decds FCB     $89                      ; NFA -> 9
        FDB     $4445,$4344,$5350,$4C59  ; "DECDSPLY"
        FCB     $B3                      ; "3"

c_decds FDB     do_col                   ; : DECDSPLY3
p_decds FDB     c_to_r                   ; >R
        FDB     c_cr                     ; CR
        FDB     c_ncu                    ; NCU
        FDB     c_dec0                   ; DEC0
        FDB     c_0                      ; 0
        FDB     c_out                    ; OUT
        FDB     c_exc                    ; !
        FDB     c_depth                  ; DEPTH
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z6883,c_lit_     ; ?BRANCH --Z6883--v
        FDB     $000A                    ; 10
        FDB     c_min                    ; MIN
        FDB     c_caret                  ; CARETR
        FDB     c_branc,Z6889            ; BRANCH --Z6889--v
Z6883   FDB     c_lit_
        FDB     $0006                    ; 6
        FDB     c_min                    ; MIN
Z6889   FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z68A5            ; ?BRANCH --Z68A5--v
        FDB     c_1sub                   ; 1-
        FDB     c_0                      ; 0
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z68A5              ; (DO) --Z68A5--v
Z6899   FDB     c_i                      ; I
        FDB     c_pick                   ; PICK
        FDB     c_5d_                    ; <5D>
        FDB     c_sub1                   ; -1
        FDB     c_addlo,Z6899            ; (+LOOP) --Z6899--^
Z68A5   FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z68CB,c_lit_     ; ?BRANCH --Z68CB--v
        FDB     $0050                    ; 80
        FDB     c_out                    ; OUT
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_0                      ; 0
        FDB     c_do_,Z68C7              ; (DO) --Z68C7--v
Z68BD   FDB     c_lit_
        FDB     $002E                    ; 46
        FDB     c_tild                   ; ~
        FDB     c_loop_,Z68BD            ; (LOOP) --Z68BD--^
Z68C7   FDB     c_branc,Z68D9            ; BRANCH --Z68D9--v
Z68CB   FDB     c_lit_
        FDB     $002E                    ; 46
        FDB     c_out                    ; OUT
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_sp                     ; SP
        FDB     c_14p                    ; 14P
Z68D9   FDB     c_0                      ; 0
        FDB     c_out                    ; OUT
        FDB     c_exc                    ; !
        FDB     c_from_                  ; R>
        FDB     c_at                     ; @
        FDB     c_to_na                  ; >NAME
        FDB     c_iddot                  ; ID.
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z6901,c_lit_     ; ?BRANCH --Z6901--v
        FDB     $0022                    ; 34
        FDB     c_out                    ; OUT
        FDB     c_at                     ; @
        FDB     c_sub                    ; -
        FDB     c_dup                    ; DUP
        FDB     c_sp                     ; SP
        FDB     c_bs0                    ; _BS
Z6901   FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; <TRAIT>
; ==================================================================
l_trai0 FDB     l_decds                  ; LFA -> DECDSPLY3

n_trai0 FCB     $87                      ; NFA -> 7
        FCB     $3C,$54,$52,$41,$49,$54  ; "<TRAIT"
        FCB     $BE                      ; ">"

c_trai0 FDB     p_trai0                  ; ASSEMBLER
p_trai0 LEAU    $01,U
        PULU    A
        SWI
        FCB     $25
        JMP     p_next

; ==================================================================
; PIXEL
; ==================================================================
l_pixel FDB     l_trai0                  ; LFA -> <TRAIT>

n_pixel FCB     $85                      ; NFA -> 5
        FCB     $50,$49,$58,$45,$CC      ; "PIXEL"

c_pixel FDB     p_pixel                  ; ASSEMBLER
p_pixel LEAX    ,U
        LEAU    $04,U
        SWI
        FCB     $2A
        JMP     p_next

; ==================================================================
; NPIX
; ==================================================================
l_npix  FDB     l_to_sq                  ; LFA -> >SQ

n_npix  FCB     $84                      ; NFA -> 4
        FCB     $4E,$50,$49,$D8          ; "NPIX"

c_npix  FDB     p_npix                   ; ASSEMBLER
p_npix  LEAX    ,U
        LEAU    $04,U
        SWI
        FCB     $2D
        JMP     p_next

; ==================================================================
; <LINE>
; ==================================================================
l_line_ FDB     l_pixel                  ; LFA -> PIXEL

n_line_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$4C,$49,$4E,$45,$BE  ; "<LINE>"

c_line_ FDB     p_line_                  ; ASSEMBLER
p_line_ LEAX    ,U
        LEAU    $08,U
        SWI
        FCB     $2B
        JMP     p_next

; ==================================================================
; <SEGM>
; ==================================================================
l_segm_ FDB     l_line_                  ; LFA -> <LINE>

n_segm_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$53,$45,$47,$4D,$BE  ; "<SEGM>"

c_segm_ FDB     p_segm_                  ; ASSEMBLER
p_segm_ LEAX    ,U
        LEAU    $04,U
        SWI
        FCB     $2C
        JMP     p_next

; ==================================================================
; TCONT
; ==================================================================
l_tcont FDB     l_segm_                  ; LFA -> <SEGM>

n_tcont FCB     $85                      ; NFA -> 5
        FCB     $54,$43,$4F,$4E,$D4      ; "TCONT"

c_tcont FDB     do_col                   ; : TCONT
p_tcont FDB     c_0                      ; 0
        FDB     c_trai0                  ; <TRAIT>
        FDB     c_semi_                  ; (;)

; ==================================================================
; TPTI1
; ==================================================================
l_tpti1 FDB     l_tcont                  ; LFA -> TCONT

n_tpti1 FCB     $85                      ; NFA -> 5
        FCB     $54,$50,$54,$49,$B1      ; "TPTI1"

c_tpti1 FDB     do_col                   ; : TPTI1
p_tpti1 FDB     c_1                      ; 1
        FDB     c_trai0                  ; <TRAIT>
        FDB     c_semi_                  ; (;)

; ==================================================================
; TPTI2
; ==================================================================
l_tpti2 FDB     l_tpti1                  ; LFA -> TPTI1

n_tpti2 FCB     $85                      ; NFA -> 5
        FCB     $54,$50,$54,$49,$B2      ; "TPTI2"

c_tpti2 FDB     do_col                   ; : TPTI2
p_tpti2 FDB     c_2                      ; 2
        FDB     c_trai0                  ; <TRAIT>
        FDB     c_semi_                  ; (;)

; ==================================================================
; TMIXTE
; ==================================================================
l_tmixt FDB     l_tpti2                  ; LFA -> TPTI2

n_tmixt FCB     $86                      ; NFA -> 6
        FCB     $54,$4D,$49,$58,$54,$C5  ; "TMIXTE"

c_tmixt FDB     do_col                   ; : TMIXTE
p_tmixt FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_trai0                  ; <TRAIT>
        FDB     c_semi_                  ; (;)

; ==================================================================
; <TEINT>
; ==================================================================
l_teint FDB     l_tmixt                  ; LFA -> TMIXTE

n_teint FCB     $87                      ; NFA -> 7
        FCB     $3C,$54,$45,$49,$4E,$54  ; "<TEINT"
        FCB     $BE                      ; ">"

c_teint FDB     p_teint                  ; ASSEMBLER
p_teint LEAX    ,U
        LEAU    $04,U
        SWI
        FCB     $31
        JMP     p_next

; ==================================================================
; TEINT
; ==================================================================
l_tein0 FDB     l_teint                  ; LFA -> <TEINT>

n_tein0 FCB     $85                      ; NFA -> 5
        FCB     $54,$45,$49,$4E,$D4      ; "TEINT"

c_tein0 FDB     do_col                   ; : TEINT
p_tein0 FDB     c_swap                   ; SWAP
        FDB     c_teint                  ; <TEINT>
        FDB     c_semi_                  ; (;)

; ==================================================================
; <PEINT>
; ==================================================================
l_peint FDB     l_tein0                  ; LFA -> TEINT

n_peint FCB     $87                      ; NFA -> 7
        FCB     $3C,$50,$45,$49,$4E,$54  ; "<PEINT"
        FCB     $BE                      ; ">"

c_peint FDB     p_peint                  ; ASSEMBLER
p_peint LEAX    ,U
        LEAU    $06,U
        LDB     $05,U
        STB     $04,U
        SWI
        FCB     $32
        JMP     p_next

; ==================================================================
; PEINT
; ==================================================================
l_pein0 FDB     l_peint                  ; LFA -> <PEINT>

n_pein0 FCB     $85                      ; NFA -> 5
        FCB     $50,$45,$49,$4E,$D4      ; "PEINT"

c_pein0 FDB     do_col                   ; : PEINT
p_pein0 FDB     c_swap                   ; SWAP
        FDB     c_peint                  ; <PEINT>
        FDB     c_semi_                  ; (;)

; ==================================================================
; SEGM
; ==================================================================
l_segm  FDB     l_qmn                    ; LFA -> ?N

n_segm  FCB     $84                      ; NFA -> 4
        FCB     $53,$45,$47,$CD          ; "SEGM"

c_segm  FDB     do_col                   ; : SEGM
p_segm  FDB     c_swap                   ; SWAP
        FDB     c_segm_                  ; <SEGM>
        FDB     c_semi_                  ; (;)

; ==================================================================
; LINE0
; ==================================================================
l_line0 FDB     l_pein0                  ; LFA -> PEINT

n_line0 FCB     $84                      ; NFA -> 4
        FCB     $4C,$49,$4E,$C5          ; "LINE"

c_line0 FDB     do_col                   ; : LINE0
p_line0 FDB     c_swap                   ; SWAP
        FDB     c_2swap                  ; 2SWAP
        FDB     c_swap                   ; SWAP
        FDB     c_line_                  ; <LINE>
        FDB     c_semi_                  ; (;)

; ==================================================================
; ARC
; ==================================================================
l_arc   FDB     l_acdat                  ; LFA -> ACDAT

n_arc   FCB     $83                      ; NFA -> 3
        FCB     $41,$52,$C3              ; "ARC"

c_arc   FDB     p_arc                    ; ASSEMBLER
p_arc   LEAX    ,U
        LEAU    $0A,U
        SWI
        FCB     $33
        JMP     p_next

; ==================================================================
; ARCTO
; ==================================================================
l_arcto FDB     l_arc                    ; LFA -> ARC

n_arcto FCB     $85                      ; NFA -> 5
        FCB     $41,$52,$43,$54,$CF      ; "ARCTO"

c_arcto FDB     p_arcto                  ; ASSEMBLER
p_arcto LEAX    ,U
        LEAU    $06,U
        SWI
        FCB     $34
        JMP     p_next

; ==================================================================
; CMD
; ==================================================================
l_cmd   FDB     l_segm                   ; LFA -> SEGM

n_cmd   FCB     $83                      ; NFA -> 3
        FCB     $43,$4D,$C4              ; "CMD"

c_cmd   FDB     do_const                 ; CONSTANT CMD
p_cmd   FDB     EF936X_REG_CMD           ; 61440

; ==================================================================
; STATUS
; ==================================================================
l_statu FDB     l_cmd                    ; LFA -> CMD

n_statu FCB     $86                      ; NFA -> 6
        FCB     $53,$54,$41,$54,$55,$D3  ; "STATUS"

c_statu FDB     do_const                 ; CONSTANT STATUS
p_statu FDB     EF936X_REG_CMD           ; 61440

; ==================================================================
; CTRL1
; ==================================================================
l_ctrl1 FDB     l_statu                  ; LFA -> STATUS

n_ctrl1 FCB     $85                      ; NFA -> 5
        FCB     $43,$54,$52,$4C,$B1      ; "CTRL1"

c_ctrl1 FDB     do_const                 ; CONSTANT CTRL1
p_ctrl1 FDB     EF936X_REG_CTRL1         ; 61441

; ==================================================================
; CTRL2
; ==================================================================
l_ctrl2 FDB     l_ctrl1                  ; LFA -> CTRL1

n_ctrl2 FCB     $85                      ; NFA -> 5
        FCB     $43,$54,$52,$4C,$B2      ; "CTRL2"

c_ctrl2 FDB     do_const                 ; CONSTANT CTRL2
p_ctrl2 FDB     EF936X_REG_CTRL2         ; 61442

; ==================================================================
; CSIZE
; ==================================================================
l_csize FDB     l_ctrl2                  ; LFA -> CTRL2

n_csize FCB     $85                      ; NFA -> 5
        FCB     $43,$53,$49,$5A,$C5      ; "CSIZE"

c_csize FDB     do_const                 ; CONSTANT CSIZE
p_csize FDB     EF936X_REG_CSIZE         ; 61443

; ==================================================================
; DELTAX
; ==================================================================
l_delta FDB     l_line0                  ; LFA -> LINE0

n_delta FCB     $86                      ; NFA -> 6
        FCB     $44,$45,$4C,$54,$41,$D8  ; "DELTAX"

c_delta FDB     do_const                 ; CONSTANT DELTAX
p_delta FDB     EF936X_REG_DELTAX        ; 61445

; ==================================================================
; DELTAY
; ==================================================================
l_delt0 FDB     l_delta                  ; LFA -> DELTAX

n_delt0 FCB     $86                      ; NFA -> 6
        FCB     $44,$45,$4C,$54,$41,$D9  ; "DELTAY"

c_delt0 FDB     do_const                 ; CONSTANT DELTAY
p_delt0 FDB     EF936X_REG_DELTAY        ; 61447

; ==================================================================
; XREG
; ==================================================================
l_xreg  FDB     l_delt0                  ; LFA -> DELTAY

n_xreg  FCB     $84                      ; NFA -> 4
        FCB     $58,$52,$45,$C7          ; "XREG"

c_xreg  FDB     do_const                 ; CONSTANT XREG
p_xreg  FDB     EF936X_REG_X_MSB         ; 61448

; ==================================================================
; YREG
; ==================================================================
l_yreg  FDB     l_arcto                  ; LFA -> ARCTO

n_yreg  FCB     $84                      ; NFA -> 4
        FCB     $59,$52,$45,$C7          ; "YREG"

c_yreg  FDB     do_const                 ; CONSTANT YREG
p_yreg  FDB     EF936X_REG_Y_MSB         ; 61450

; ==================================================================
; 2PI
; ==================================================================
l_2pi   FDB     l_npix                   ; LFA -> NPIX

n_2pi   FCB     $83                      ; NFA -> 3
        FCB     $32,$50,$C9              ; "2PI"

c_2pi   FDB     do_const                 ; CONSTANT 2PI
p_2pi   FDB     $0E10                    ; 3600

; ==================================================================
; PRET
; ==================================================================
l_pret  FDB     l_xreg                   ; LFA -> XREG

n_pret  FCB     $84                      ; NFA -> 4
        FCB     $50,$52,$45,$D4          ; "PRET"

c_pret  FDB     do_const                 ; CONSTANT PRET
p_pret  FDB     $0004                    ; 4

; ==================================================================
; PRET?
; ==================================================================
l_is_pr FDB     l_pret                   ; LFA -> PRET

n_is_pr FCB     $85                      ; NFA -> 5
        FCB     $50,$52,$45,$54,$BF      ; "PRET?"

c_is_pr FDB     p_is_pr                  ; ASSEMBLER
p_is_pr LDB     EF936X_REG_CMD
        BITB    #$04
        BEQ     p_is_pr
        JMP     p_next

; ==================================================================
; GO
; ==================================================================
l_go    FDB     l_csize                  ; LFA -> CSIZE

n_go    FCB     $82                      ; NFA -> 2
        FCB     $47,$CF                  ; "GO"

c_go    FDB     do_col                   ; : GO
p_go    FDB     c_is_pr                  ; PRET?
        FDB     c_cmd                    ; CMD
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; X,Y
; ==================================================================
l_xkomy FDB     l_is_pr                  ; LFA -> PRET?

n_xkomy FCB     $83                      ; NFA -> 3
        FCB     $58,$2C,$D9              ; "X,Y"

c_xkomy FDB     do_col                   ; : X,Y
p_xkomy FDB     c_yreg                   ; YREG
        FDB     c_exc                    ; !
        FDB     c_xreg                   ; XREG
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; CIRCLE
; ==================================================================
l_circl FDB     l_go                     ; LFA -> GO

n_circl FCB     $86                      ; NFA -> 6
        FCB     $43,$49,$52,$43,$4C,$C5  ; "CIRCLE"

c_circl FDB     do_col                   ; : CIRCLE
p_circl FDB     c_swap                   ; SWAP
        FDB     c_2dup                   ; 2DUP
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_rot                    ; ROT
        FDB     c_add                    ; +
        FDB     c_2pi                    ; 2PI
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_arc                    ; ARC
        FDB     c_semi_                  ; (;)

; ==================================================================
; SELONY
; ==================================================================
l_selon FDB     l_circl                  ; LFA -> CIRCLE

n_selon FCB     $86                      ; NFA -> 6
        FCB     $53,$45,$4C,$4F,$4E,$D9  ; "SELONY"

c_selon FDB     do_col                   ; : SELONY
p_selon FDB     c_is_pr                  ; PRET?
        FDB     c_delta                  ; DELTAX
        FDB     c_cexc                   ; C!
        FDB     c_xreg                   ; XREG
        FDB     c_at,c_lit_              ; @
        FDB     $0010                    ; 16
        FDB     c_rot,c_lit_             ; ROT
        FDB     $00FF                    ; 255
        FDB     c_and                    ; AND
        FDB     c_0                      ; 0
        FDB     c_do_,Z6B45              ; (DO) --Z6B45--v
Z6B31   FDB     c_dup                    ; DUP
        FDB     c_go                     ; GO
        FDB     c_1                      ; 1
        FDB     c_yreg                   ; YREG
        FDB     c_addex                  ; +!
        FDB     c_over                   ; OVER
        FDB     c_xreg                   ; XREG
        FDB     c_exc                    ; !
        FDB     c_loop_,Z6B31            ; (LOOP) --Z6B31--^
Z6B45   FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; SELONX
; ==================================================================
l_selo0 FDB     l_selon                  ; LFA -> SELONY

n_selo0 FCB     $86                      ; NFA -> 6
        FCB     $53,$45,$4C,$4F,$4E,$D8  ; "SELONX"

c_selo0 FDB     do_col                   ; : SELONX
p_selo0 FDB     c_swap                   ; SWAP
        FDB     c_is_pr                  ; PRET?
        FDB     c_delt0                  ; DELTAY
        FDB     c_cexc                   ; C!
        FDB     c_yreg                   ; YREG
        FDB     c_at,c_lit_              ; @
        FDB     $0012                    ; 18
        FDB     c_rot,c_lit_             ; ROT
        FDB     $00FF                    ; 255
        FDB     c_and                    ; AND
        FDB     c_0                      ; 0
        FDB     c_do_,Z6B86              ; (DO) --Z6B86--v
Z6B72   FDB     c_dup                    ; DUP
        FDB     c_go                     ; GO
        FDB     c_1                      ; 1
        FDB     c_xreg                   ; XREG
        FDB     c_addex                  ; +!
        FDB     c_over                   ; OVER
        FDB     c_yreg                   ; YREG
        FDB     c_exc                    ; !
        FDB     c_loop_,Z6B72            ; (LOOP) --Z6B72--^
Z6B86   FDB     c_2drop                  ; 2DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; PLAN
; ==================================================================
l_plan  FDB     l_xkomy                  ; LFA -> X,Y

n_plan  FCB     $84                      ; NFA -> 4
        FCB     $50,$4C,$41,$CE          ; "PLAN"

c_plan  FDB     do_col                   ; : PLAN
p_plan  FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z6BA3            ; ?BRANCH --Z6BA3--v
        FDB     c_2drop                  ; 2DROP
        FDB     c_2drop                  ; 2DROP
        FDB     c_branc,Z6BBD            ; BRANCH --Z6BBD--v
Z6BA3   FDB     c_2swap                  ; 2SWAP
        FDB     c_xkomy                  ; X,Y
        FDB     c_abs                    ; ABS
        FDB     c_swap                   ; SWAP
        FDB     c_abs                    ; ABS
        FDB     c_2dup                   ; 2DUP
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z6BBB            ; ?BRANCH --Z6BBB--v
        FDB     c_selon                  ; SELONY
        FDB     c_branc,Z6BBD            ; BRANCH --Z6BBD--v
Z6BBB   FDB     c_selo0                  ; SELONX
Z6BBD   FDB     c_semi_                  ; (;)

; ==================================================================
; CADRE
; ==================================================================
l_cadre FDB     l_selo0                  ; LFA -> SELONX

n_cadre FCB     $85                      ; NFA -> 5
        FCB     $43,$41,$44,$52,$C5      ; "CADRE"

c_cadre FDB     do_col                   ; : CADRE
p_cadre FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z6BD9            ; ?BRANCH --Z6BD9--v
        FDB     c_2drop                  ; 2DROP
        FDB     c_2drop                  ; 2DROP
        FDB     c_branc,Z6C1D            ; BRANCH --Z6C1D--v
Z6BD9   FDB     c_2dup                   ; 2DUP
        FDB     c_to_r                   ; >R
        FDB     c_to_r                   ; >R
        FDB     c_cross                  ; CROSS
        FDB     c_add                    ; +
        FDB     c_to_r                   ; >R
        FDB     c_add                    ; +
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_from_                  ; R>
        FDB     c_2swap                  ; 2SWAP
        FDB     c_xkomy                  ; X,Y
        FDB     c_abs                    ; ABS
        FDB     c_is_pr                  ; PRET?
        FDB     c_delt0                  ; DELTAY
        FDB     c_cexc                   ; C!
        FDB     c_delta                  ; DELTAX
        FDB     c_cexc,c_lit_            ; C!
        FDB     $0010,c_lit_             ; 16
        FDB     $0012,c_lit_             ; 18
        FDB     $0016,c_lit_             ; 22
        FDB     $0014,c_lit_             ; 20
        FDB     $0004                    ; 4
        FDB     c_0                      ; 0
        FDB     c_do_,Z6C1D              ; (DO) --Z6C1D--v
Z6C17   FDB     c_go                     ; GO
        FDB     c_loop_,Z6C17            ; (LOOP) --Z6C17--^
Z6C1D   FDB     c_semi_                  ; (;)

; ==================================================================
; MOVEREL
; ==================================================================
l_mover FDB     l_yreg                   ; LFA -> YREG

n_mover FCB     $87                      ; NFA -> 7
        FCB     $4D,$4F,$56,$45,$52,$45  ; "MOVERE"
        FCB     $CC                      ; "L"

c_mover FDB     p_mover                  ; ASSEMBLER
p_mover LEAX    ,U
        LEAU    $04,U
        LDD     EF936X_REG_X_MSB
        ADDD    ,X
        LDD     EF936X_REG_Y_MSB
        ADDD    $02,X
        STD     $02,X
        SWI
        FCB     $2D
        JMP     p_next

; ==================================================================
; PIX?
; ==================================================================
l_is_p0 FDB     l_plan                   ; LFA -> PLAN

n_is_p0 FCB     $84                      ; NFA -> 4
        FCB     $50,$49,$58,$BF          ; "PIX?"

c_is_p0 FDB     p_is_p0                  ; ASSEMBLER
p_is_p0 LEAU    -$04,U
        LEAX    ,U
        SWI
        FCB     $2E
        JMP     p_next

; ==================================================================
; COL?
; ==================================================================
l_is_co FDB     l_cadre                  ; LFA -> CADRE

n_is_co FCB     $84                      ; NFA -> 4
        FCB     $43,$4F,$4C,$BF          ; "COL?"

c_is_co FDB     p_is_co                  ; ASSEMBLER
p_is_co LEAX    ,U
        LEAU    $04,U
        SWI
        FCB     $2F
        CLRB
        PSHU    A
        PSHU    B
        JMP     p_next

; ==================================================================
; ATTRIBUTS
; ==================================================================
l_attri FDB     l_mover                  ; LFA -> MOVEREL

n_attri FCB     $89                      ; NFA -> 9
        FDB     $4154,$5452,$4942,$5554  ; "ATTRIBUT"
        FCB     $D3                      ; "S"

c_attri FDB     do_var                   ; VARIABLE ATTRIBUTS
p_attri FDB     $0067                    ; 103
        FCB     $00,$08,$01,$01,$00,$6D,$B9,$00,$08

; ==================================================================
; LARGE
; ==================================================================
l_large FDB     l_is_p0                  ; LFA -> PIX?

n_large FCB     $85                      ; NFA -> 5
        FCB     $4C,$41,$52,$47,$C5      ; "LARGE"

c_large FDB     do_col                   ; : LARGE
p_large FDB     c_attri,c_lit_           ; ATTRIBUTS
        FDB     $0004                    ; 4
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; HAUT
; ==================================================================
l_haut  FDB     l_large                  ; LFA -> LARGE

n_haut  FCB     $84                      ; NFA -> 4
        FCB     $48,$41,$55,$D4          ; "HAUT"

c_haut  FDB     do_col                   ; : HAUT
p_haut  FDB     c_attri,c_lit_           ; ATTRIBUTS
        FDB     $0005                    ; 5
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; H-DROIT
; ==================================================================
l_hsubd FDB     l_haut                   ; LFA -> HAUT

n_hsubd FCB     $87                      ; NFA -> 7
        FCB     $48,$2D,$44,$52,$4F,$49  ; "H-DROI"
        FCB     $D4                      ; "T"

c_hsubd FDB     do_col                   ; : H-DROIT
p_hsubd FDB     c_0                      ; 0
        FDB     c_attri,c_lit_           ; ATTRIBUTS
        FDB     $0006                    ; 6
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; H-ITAL
; ==================================================================
l_hsubi FDB     l_hsubd                  ; LFA -> H-DROIT

n_hsubi FCB     $86                      ; NFA -> 6
        FCB     $48,$2D,$49,$54,$41,$CC  ; "H-ITAL"

c_hsubi FDB     do_col                   ; : H-ITAL
p_hsubi FDB     c_1                      ; 1
        FDB     c_attri,c_lit_           ; ATTRIBUTS
        FDB     $0006                    ; 6
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; V-DROIT
; ==================================================================
l_vsubd FDB     l_2pi                    ; LFA -> 2PI

n_vsubd FCB     $87                      ; NFA -> 7
        FCB     $56,$2D,$44,$52,$4F,$49  ; "V-DROI"
        FCB     $D4                      ; "T"

c_vsubd FDB     do_col                   ; : V-DROIT
p_vsubd FDB     c_2                      ; 2
        FDB     c_attri,c_lit_           ; ATTRIBUTS
        FDB     $0006                    ; 6
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; V-ITAL
; ==================================================================
l_vsubi FDB     l_vsubd                  ; LFA -> V-DROIT

n_vsubi FCB     $86                      ; NFA -> 6
        FCB     $56,$2D,$49,$54,$41,$CC  ; "V-ITAL"

c_vsubi FDB     do_col                   ; : V-ITAL
p_vsubi FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_attri,c_lit_           ; ATTRIBUTS
        FDB     $0006                    ; 6
        FDB     c_add                    ; +
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; COORD
; ==================================================================
l_coord FDB     l_is_co                  ; LFA -> COL?

n_coord FCB     $85                      ; NFA -> 5
        FCB     $43,$4F,$4F,$52,$C4      ; "COORD"

c_coord FDB     do_col                   ; : COORD
p_coord FDB     c_attri                  ; ATTRIBUTS
        FDB     c_swap                   ; SWAP
        FDB     c_over                   ; OVER
        FDB     c_exc                    ; !
        FDB     c_2add                   ; 2+
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; <GTEXTE>
; ==================================================================
l_gtext FDB     l_hsubi                  ; LFA -> H-ITAL

n_gtext FCB     $88                      ; NFA -> 8
        FDB     $3C47,$5445,$5854,$45BE  ; "<GTEXTE>"

c_gtext FDB     p_gtext                  ; ASSEMBLER
p_gtext PULU    X
        SWI
        FCB     $35
        JMP     p_next

; ==================================================================
; GTEXTE
; ==================================================================
l_gtex0 FDB     l_coord                  ; LFA -> COORD

n_gtex0 FCB     $86                      ; NFA -> 6
        FCB     $47,$54,$45,$58,$54,$C5  ; "GTEXTE"

c_gtex0 FDB     do_col                   ; : GTEXTE
p_gtex0 FDB     c_count                  ; COUNT
        FDB     c_attri                  ; ATTRIBUTS
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_swap                   ; SWAP
        FDB     c_over,c_lit_            ; OVER
        FDB     $0009                    ; 9
        FDB     c_add                    ; +
        FDB     c_exc,c_lit_             ; !
        FDB     $0007                    ; 7
        FDB     c_add                    ; +
        FDB     c_exc                    ; !
        FDB     c_from_                  ; R>
        FDB     c_gtext                  ; <GTEXTE>
        FDB     c_semi_                  ; (;)

; ==================================================================
; SPLASH
; ==================================================================
l_splas FDB     l_gtex0                  ; LFA -> GTEXTE

n_splas FCB     $86                      ; NFA -> 6
        FCB     $53,$50,$4C,$41,$53,$C8  ; "SPLASH"

c_splas FDB     do_col                   ; : SPLASH
p_splas FDB     c_coord                  ; COORD
        FDB     c_col0                   ; COL
        FDB     c_gtex0                  ; GTEXTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; <MOTIF>
; ==================================================================
l_motif FDB     l_gtext                  ; LFA -> <GTEXTE>

n_motif FCB     $87                      ; NFA -> 7
        FCB     $3C,$4D,$4F,$54,$49,$46  ; "<MOTIF"
        FCB     $BE                      ; ">"

c_motif FDB     p_motif                  ; ASSEMBLER
p_motif PULU    X,D
        PSHU    D
        PSHU    X
        LDD     EF936X_REG_X_MSB
        LDX     EF936X_REG_Y_MSB
        PSHU    X,D
        LEAX    ,U
        LEAU    $08,U
        SWI
        FCB     $36
        JMP     p_next

; ==================================================================
; MOTIF
; ==================================================================
l_moti0 FDB     l_attri                  ; LFA -> ATTRIBUTS

n_moti0 FCB     $85                      ; NFA -> 5
        FCB     $4D,$4F,$54,$49,$C6      ; "MOTIF"

c_moti0 FDB     do_col                   ; : MOTIF
p_moti0 FDB     c_count                  ; COUNT
        FDB     c_motif                  ; <MOTIF>
        FDB     c_semi_                  ; (;)

; ==================================================================
; $DAT
; ==================================================================
l_dolda FDB     l_motif                  ; LFA -> <MOTIF>

n_dolda FCB     $84                      ; NFA -> 4
        FCB     $24,$44,$41,$D4          ; "$DAT"

c_dolda FDB     do_var                   ; VARIABLE $DAT
p_dolda FDB     $0832                    ; 2098
        FCB     $38,$2F,$30,$33,$2F,$38,$38

; ==================================================================
; BADAT
; ==================================================================
l_badat FDB     l_vsubi                  ; LFA -> V-ITAL

n_badat FCB     $85                      ; NFA -> 5
        FCB     $42,$41,$44,$41,$D4      ; "BADAT"

c_badat FDB     do_col                   ; : BADAT
p_badat FDB     c_1                      ; 1
        FDB     c_haut                   ; HAUT
        FDB     c_1                      ; 1
        FDB     c_large                  ; LARGE
        FDB     c_hsubd                  ; H-DROIT
        FDB     c_datet                  ; DATE'
        FDB     c_dolda                  ; $DAT
        FDB     c_1add,c_lit_            ; 1+
        FDB     $0004                    ; 4
        FDB     c_move,c_lit_            ; MOVE
        FDB     $0008,c_lit_             ; 8
        FDB     $0067                    ; 103
        FDB     c_coord                  ; COORD
        FDB     c_dolda,c_lit_           ; $DAT
        FDB     $000D                    ; 13
        FDB     c_col0                   ; COL
        FDB     c_gtex0                  ; GTEXTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; BORNE1
; ==================================================================
l_borne FDB     l_badat                  ; LFA -> BADAT

n_borne FCB     $86                      ; NFA -> 6
        FCB     $42,$4F,$52,$4E,$45,$B1  ; "BORNE1"

c_borne FDB     do_col                   ; : BORNE1
p_borne FDB     c_semi_                  ; (;)

; ==================================================================
; CRASH
; ==================================================================
l_crash FDB     l_splas                  ; LFA -> SPLASH

n_crash FCB     $85                      ; NFA -> 5
        FCB     $43,$52,$41,$53,$C8      ; "CRASH"

c_crash FDB     do_col                   ; : CRASH
p_crash FDB     c_true                   ; TRUE
        FDB     c_abort                  ; (ABORT") len=17
        FCB     $11
        FDB     $5665,$6374,$6575,$7220  ; "Vecteur "
        FDB     $696E,$6480,$6669,$6E69  ; "indéfini"
        FCB     $21                      ; "!"
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEFER
; ==================================================================
l_defer FDB     l_dolda                  ; LFA -> $DAT

n_defer FCB     $85                      ; NFA -> 5
        FCB     $44,$45,$46,$45,$D2      ; "DEFER"

c_defer FDB     do_col                   ; : DEFER
p_defer FDB     c_creat,c_lit_           ; CREATE
        FDB     $6E0C                    ; ' CRASH
        FDB     c_kom                    ; ,
        FDB     c_semic                  ; (;CODE)
        JMP     do_does
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_semi_                  ; (;)

; ==================================================================
; <IS>
; ==================================================================
l_is_   FDB     l_defer                  ; LFA -> DEFER

n_is_   FCB     $84                      ; NFA -> 4
        FCB     $3C,$49,$53,$BE          ; "<IS>"

c_is_   FDB     do_col                   ; : <IS>
p_is_   FDB     c_from_                  ; R>
        FDB     c_dup                    ; DUP
        FDB     c_2add                   ; 2+
        FDB     c_to_r                   ; >R
        FDB     c_at                     ; @
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; IS
; ==================================================================
l_is    FDB     l_moti0                  ; LFA -> MOTIF

n_is    FCB     $C2                      ; NFA -> 2 IMMEDIATE
        FCB     $49,$D3                  ; "IS"

c_is    FDB     do_col                   ; : IS
p_is    FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_qmbra,Z6E73            ; ?BRANCH --Z6E73--v
        FDB     c_compi                  ; COMPILE
        FDB     c_is_                    ; <IS>
        FDB     c_branc,Z6E79            ; BRANCH --Z6E79--v
Z6E73   FDB     c_tck                    ; '
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc                    ; !
Z6E79   FDB     c_semi_                  ; (;)

; ==================================================================
; RAPELTIT
; ==================================================================
l_rapel FDB     l_borne                  ; LFA -> BORNE1

n_rapel FCB     $88                      ; NFA -> 8
        FDB     $5241,$5045,$4C54,$49D4  ; "RAPELTIT"

c_rapel FDB     do_col                   ; : RAPELTIT
p_rapel FDB     c_cr                     ; CR
        FDB     c_14p                    ; 14P
        FDB     c_dotq1                  ; (."F) len=12
        FCB     $0C
        FDB     $414E,$4154,$4F4D,$4945  ; "ANATOMIE"
        FCB     $20,$44,$45,$20          ; " DE "
        FDB     c_3p                     ; 3P
        FDB     c_to_na                  ; >NAME
        FDB     c_iddot                  ; ID.
        FDB     c_11p                    ; 11P
        FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; <<<
; ==================================================================
l_from7 FDB     l_is_                    ; LFA -> <IS>

n_from7 FCB     $83                      ; NFA -> 3
        FCB     $3C,$3C,$BC              ; "<<<"

c_from7 FDB     do_col                   ; : <<<
p_from7 FDB     c_cr,c_lit_              ; CR
        FDB     $000B                    ; 11
        FDB     c_sp                     ; SP
        FDB     c_semi_                  ; (;)

; ==================================================================
; L-ANALY
; ==================================================================
l_lsuba FDB     l_from7                  ; LFA -> <<<

n_lsuba FCB     $87                      ; NFA -> 7
        FCB     $4C,$2D,$41,$4E,$41,$4C  ; "L-ANAL"
        FCB     $D9                      ; "Y"

c_lsuba FDB     do_col                   ; : L-ANALY
p_lsuba FDB     c_space                  ; SPACE
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_lgtna                  ; L>NAME
        FDB     c_14p                    ; 14P
        FDB     c_dotq1                  ; (."F) len=8
        FCB     $08
        FDB     $4C69,$8020,$8720,$3A20  ; "Lié à : "
        FDB     c_3p                     ; 3P
        FDB     c_iddot                  ; ID.
        FDB     c_space                  ; SPACE
        FDB     c_cdiv0                  ; C/L0
        FDB     c_tild                   ; ~
        FDB     c_udot                   ; U.
        FDB     c_bs,c_lit_              ; BS
        FDB     $0029                    ; 41
        FDB     c_tild                   ; ~
        FDB     c_11p                    ; 11P
        FDB     c_semi_                  ; (;)

; ==================================================================
; N-LETR
; ==================================================================
l_nsubl FDB     l_rapel                  ; LFA -> RAPELTIT

n_nsubl FCB     $86                      ; NFA -> 6
        FCB     $4E,$2D,$4C,$45,$54,$D2  ; "N-LETR"

c_nsubl FDB     do_col                   ; : N-LETR
p_nsubl FDB     c_11p                    ; 11P
        FDB     c_0                      ; 0
        FDB     c_do_,Z6F27              ; (DO) --Z6F27--v
Z6F05   FDB     c_cr                     ; CR
        FDB     c_dup,c_lit_             ; DUP
        FDB     $000B                    ; 11
        FDB     c_udotr                  ; U.R
        FDB     c_dup                    ; DUP
        FDB     c_space                  ; SPACE
        FDB     c_1p                     ; 1P
        FDB     c_cat,c_lit_             ; C@
        FDB     $007F                    ; 127
        FDB     c_and                    ; AND
        FDB     c_carti                  ; CAR~
        FDB     c_11p                    ; 11P
        FDB     c_1add                   ; 1+
        FDB     c_loop_,Z6F05            ; (LOOP) --Z6F05--^
Z6F27   FDB     c_drop                   ; DROP
        FDB     c_11p                    ; 11P
        FDB     c_semi_                  ; (;)

; ==================================================================
; N-ANALY
; ==================================================================
l_nsuba FDB     l_nsubl                  ; LFA -> N-LETR

n_nsuba FCB     $87                      ; NFA -> 7
        FCB     $4E,$2D,$41,$4E,$41,$4C  ; "N-ANAL"
        FCB     $D9                      ; "Y"

c_nsuba FDB     do_col                   ; : N-ANALY
p_nsuba FDB     c_1add                   ; 1+
        FDB     c_dup                    ; DUP
        FDB     c_1sub                   ; 1-
        FDB     c_space                  ; SPACE
        FDB     c_14p                    ; 14P
        FDB     c_cat,c_lit_             ; C@
        FDB     $007F                    ; 127
        FDB     c_and                    ; AND
        FDB     c_dup                    ; DUP
        FDB     c_width                  ; WIDTH
        FDB     c_at                     ; @
        FDB     c_gt                     ; >
        FDB     c_qmbra,Z6F85            ; ?BRANCH --Z6F85--v
        FDB     c_2p                     ; 2P
        FDB     c_dotq1                  ; (."F) len=13
        FCB     $0D
        FDB     $4D4F,$5420,$494D,$4D45  ; "MOT IMME"
        FCB     $44,$49,$41,$54,$20      ; "DIAT "
        FDB     c_11p                    ; 11P
        FDB     c_dotq1                  ; (."F) len=11
        FCB     $0B
        FDB     $2842,$6974,$2036,$203D  ; "(Bit 6 ="
        FCB     $20,$31,$29              ; " 1)"
        FDB     c_lit_
        FDB     $0040                    ; 64
        FDB     c_sub                    ; -
        FDB     c_11p                    ; 11P
        FDB     c_from7                  ; <<<
        FDB     c_space                  ; SPACE
Z6F85   FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $4E,$6F,$6D,$20,$64,$65  ; "Nom de"
        FCB     $20                      ; " "
        FDB     c_2dup                   ; 2DUP
        FDB     c_dup                    ; DUP
        FDB     c_dot                    ; .
        FDB     c_dotq1                  ; (."F) len=10
        FCB     $0A
        FDB     $6C65,$7474,$7265,$7320  ; "lettres "
        FCB     $3A,$20                  ; ": "
        FDB     c_3p                     ; 3P
        FDB     c_0                      ; 0
        FDB     c_do_,Z6FBC              ; (DO) --Z6FBC--v
Z6FAA   FDB     c_dup                    ; DUP
        FDB     c_cat,c_lit_             ; C@
        FDB     $007F                    ; 127
        FDB     c_and                    ; AND
        FDB     c_carti                  ; CAR~
        FDB     c_1add                   ; 1+
        FDB     c_loop_,Z6FAA            ; (LOOP) --Z6FAA--^
Z6FBC   FDB     c_drop                   ; DROP
        FDB     c_nsubl                  ; N-LETR
        FDB     c_semi_                  ; (;)

; ==================================================================
; RAPELBODY
; ==================================================================
l_rape0 FDB     l_nsuba                  ; LFA -> N-ANALY

n_rape0 FCB     $89                      ; NFA -> 9
        FDB     $5241,$5045,$4C42,$4F44  ; "RAPELBOD"
        FCB     $D9                      ; "Y"

c_rape0 FDB     do_col                   ; : RAPELBODY
p_rape0 FDB     c_14p                    ; 14P
        FDB     c_dotq1                  ; (."F) len=25
        FCB     $19
        FDB     $2053,$7569,$7665,$6E74  ; " Suivent"
        FDB     $206C,$6573,$2061,$6472  ; " les adr"
        FDB     $6573,$7365,$7320,$7175  ; "esses qu"
        FCB     $69                      ; "i"
        FDB     c_from7                  ; <<<
        FDB     c_dotq1                  ; (."F) len=25
        FCB     $19
        FDB     $2070,$6F69,$6E74,$656E  ; " pointen"
        FDB     $742C,$7061,$7220,$7061  ; "t,par pa"
        FDB     $6972,$6573,$2C20,$7375  ; "ires, su"
        FCB     $72                      ; "r"
        FDB     c_from7                  ; <<<
        FDB     c_dotq1                  ; (."F) len=21
        FCB     $15
        FDB     $206C,$6573,$206D,$6F74  ; " les mot"
        FDB     $7320,$8720,$6578,$8063  ; "s à exéc"
        FCB     $75,$74,$65,$72,$2E      ; "uter."
        FDB     c_11p                    ; 11P
        FDB     c_semi_                  ; (;)

; ==================================================================
; ANAT
; ==================================================================
l_anat  FDB     l_is                     ; LFA -> IS

n_anat  FCB     $84                      ; NFA -> 4
        FCB     $41,$4E,$41,$D4          ; "ANAT"

c_anat  FDB     do_col                   ; : ANAT
p_anat  FDB     c_ncu                    ; NCU
        FDB     c_cls                    ; CLS
        FDB     c_11p                    ; 11P
        FDB     c_bl                     ; BL
        FDB     c_word                   ; WORD
        FDB     c_find                   ; FIND
        FDB     c_qmbra,Z70C7            ; ?BRANCH --Z70C7--v
        FDB     c_dup                    ; DUP
        FDB     c_rapel                  ; RAPELTIT
        FDB     c_trait                  ; TRAIT
        FDB     c_dup                    ; DUP
        FDB     c_to_li                  ; >LINK
        FDB     c_dup                    ; DUP
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $4C,$69,$6E,$6B,$20,$3A  ; "Link :"
        FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_lsuba                  ; L-ANALY
        FDB     c_trait                  ; TRAIT
        FDB     c_dup                    ; DUP
        FDB     c_to_na                  ; >NAME
        FDB     c_dup                    ; DUP
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $4E,$61,$6D,$65,$20,$3A  ; "Name :"
        FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_nsuba                  ; N-ANALY
        FDB     c_trait                  ; TRAIT
        FDB     c_dup                    ; DUP
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $43,$6F,$64,$65,$20,$3A  ; "Code :"
        FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_14p                    ; 14P
        FDB     c_dotq1                  ; (."F) len=25
        FCB     $19
        FDB     $2028,$556E,$6520,$6365  ; " (Une ce"
        FDB     $6C6C,$756C,$652F,$3220  ; "llule/2 "
        FDB     $4164,$7265,$7373,$6573  ; "Adresses"
        FCB     $29                      ; ")"
        FDB     c_11p                    ; 11P
        FDB     c_trait                  ; TRAIT
        FDB     c_to_bo                  ; >BODY
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $42,$6F,$64,$79,$20,$3A  ; "Body :"
        FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_rape0                  ; RAPELBODY
        FDB     c_trait                  ; TRAIT
        FDB     c_branc,Z70EB            ; BRANCH --Z70EB--v
Z70C7   FDB     c_drop,c_lit_            ; DROP
        FDB     $0004                    ; 4
        FDB     c_al                     ; AL
        FDB     c_dotq1                  ; (."F) len=19
        FCB     $13
        FDB     $4365,$206D,$6F74,$2065  ; "Ce mot e"
        FDB     $7374,$2069,$6E63,$6F6E  ; "st incon"
        FCB     $6E,$75,$21              ; "nu!"
        FDB     c_cr                     ; CR
        FDB     c_qmqm                   ; ??
        FDB     c_cr                     ; CR
Z70EB   FDB     c_cr                     ; CR
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; FIN
; ==================================================================
l_fin   FDB     l_rape0                  ; LFA -> RAPELBODY

n_fin   FCB     $83                      ; NFA -> 3
        FCB     $46,$49,$CE              ; "FIN"

c_fin   FDB     do_var                   ; VARIABLE FIN
p_fin   FDB     $0000                    ; 0

; ==================================================================
; 3*
; ==================================================================
l_3ast  FDB     l_crash                  ; LFA -> CRASH

n_3ast  FCB     $82                      ; NFA -> 2
        FCB     $33,$AA                  ; "3*"

c_3ast  FDB     do_col                   ; : 3*
p_3ast  FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_ast                    ; *
        FDB     c_semi_                  ; (;)

; ==================================================================
; ?END
; ==================================================================
l_qmend FDB     l_3ast                   ; LFA -> 3*

n_qmend FCB     $84                      ; NFA -> 4
        FCB     $3F,$45,$4E,$C4          ; "?END"

c_qmend FDB     do_col                   ; : ?END
p_qmend FDB     c_qmbrk                  ; ?BRK
        FDB     c_qmbra,Z7121            ; ?BRANCH --Z7121--v
        FDB     c_sub1                   ; -1
        FDB     c_fin                    ; FIN
        FDB     c_exc                    ; !
Z7121   FDB     c_semi_                  ; (;)

; ==================================================================
; _OCT
; ==================================================================
l_oct   FDB     l_qmend                  ; LFA -> ?END

n_oct   FCB     $84                      ; NFA -> 4
        FCB     $5F,$4F,$43,$D4          ; "_OCT"

c_oct   FDB     do_col                   ; : _OCT
p_oct   FDB     c_over                   ; OVER
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z7144              ; (DO) --Z7144--v
Z7136   FDB     c_space                  ; SPACE
        FDB     c_i                      ; I
        FDB     c_cat                    ; C@
        FDB     c_2hdot                  ; 2H.
        FDB     c_qmend                  ; ?END
        FDB     c_loop_,Z7136            ; (LOOP) --Z7136--^
Z7144   FDB     c_semi_                  ; (;)

; ==================================================================
; _CAR
; ==================================================================
l_car   FDB     l_oct                    ; LFA -> _OCT

n_car   FCB     $84                      ; NFA -> 4
        FCB     $5F,$43,$41,$D2          ; "_CAR"

c_car   FDB     do_col                   ; : _CAR
p_car   FDB     c_over                   ; OVER
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z7165              ; (DO) --Z7165--v
Z7159   FDB     c_i                      ; I
        FDB     c_cat                    ; C@
        FDB     c_carti                  ; CAR~
        FDB     c_qmend                  ; ?END
        FDB     c_loop_,Z7159            ; (LOOP) --Z7159--^
Z7165   FDB     c_semi_                  ; (;)

; ==================================================================
; REFER
; ==================================================================
l_refer FDB     l_fin                    ; LFA -> FIN

n_refer FCB     $85                      ; NFA -> 5
        FCB     $52,$45,$46,$45,$D2      ; "REFER"

c_refer FDB     do_col                   ; : REFER
p_refer FDB     c_base                   ; BASE
        FDB     c_at                     ; @
        FDB     c_hex                    ; HEX
        FDB     c_cr                     ; CR
        FDB     c_vt,c_lit_              ; VT
        FDB     $0006                    ; 6
        FDB     c_0                      ; 0
        FDB     c_do_,Z718B              ; (DO) --Z718B--v
Z7185   FDB     c_ht                     ; HT
        FDB     c_loop_,Z7185            ; (LOOP) --Z7185--^
Z718B   FDB     c_3p,c_lit_              ; 3P
        FDB     $0008                    ; 8
        FDB     c_0                      ; 0
        FDB     c_do_,Z71A5              ; (DO) --Z71A5--v
Z7197   FDB     c_lit_
        FDB     $002B                    ; 43
        FDB     c_tild                   ; ~
        FDB     c_i                      ; I
        FDB     c_dot                    ; .
        FDB     c_loop_,Z7197            ; (LOOP) --Z7197--^
Z71A5   FDB     c_space,c_lit_           ; SPACE
        FDB     $0038,c_lit_             ; 56
        FDB     $0030                    ; ' EXIT
        FDB     c_do_,Z71BB              ; (DO) --Z71BB--v
Z71B3   FDB     c_i                      ; I
        FDB     c_tild                   ; ~
        FDB     c_loop_,Z71B3            ; (LOOP) --Z71B3--^
Z71BB   FDB     c_base                   ; BASE
        FDB     c_exc                    ; !
        FDB     c_14p                    ; 14P
        FDB     c_semi_                  ; (;)

; ==================================================================
; _LIGNE
; ==================================================================
l_ligne FDB     l_car                    ; LFA -> _CAR

n_ligne FCB     $86                      ; NFA -> 6
        FCB     $5F,$4C,$49,$47,$4E,$C5  ; "_LIGNE"

c_ligne FDB     do_col                   ; : _LIGNE
p_ligne FDB     c_cr                     ; CR
        FDB     c_3p                     ; 3P
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0005                    ; 5
        FDB     c_udotr,c_lit_           ; U.R
        FDB     $0008                    ; 8
        FDB     c_2dup                   ; 2DUP
        FDB     c_14p                    ; 14P
        FDB     c_oct                    ; _OCT
        FDB     c_2                      ; 2
        FDB     c_sp                     ; SP
        FDB     c_6p                     ; 6P
        FDB     c_car                    ; _CAR
        FDB     c_semi_                  ; (;)

; ==================================================================
; DMP
; ==================================================================
l_dmp   FDB     l_lsuba                  ; LFA -> L-ANALY

n_dmp   FCB     $83                      ; NFA -> 3
        FCB     $44,$4D,$D0              ; "DMP"

c_dmp   FDB     do_col                   ; : DMP
p_dmp   FDB     c_ncu                    ; NCU
        FDB     c_0                      ; 0
        FDB     c_fin                    ; FIN
        FDB     c_exc                    ; !
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z720C            ; ?BRANCH --Z720C--v
        FDB     c_dump                   ; DUMP
        FDB     c_branc,Z72B9            ; BRANCH --Z72B9--v
Z720C   FDB     c_cr,c_lit_              ; CR
        FDB     $0008                    ; 8
        FDB     c_divmo                  ; /MOD
        FDB     c_1                      ; 1
        FDB     c_max                    ; MAX
        FDB     c_swap                   ; SWAP
        FDB     c_to_r                   ; >R
        FDB     c_0                      ; 0
        FDB     c_do_,Z7260              ; (DO) --Z7260--v
Z7222   FDB     c_i,c_lit_               ; I
        FDB     $0005                    ; 5
        FDB     c_mod                    ; MOD
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z724A            ; ?BRANCH --Z724A--v
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_11p                    ; 11P
        FDB     c_dotq1                  ; (."F) len=1
        FCB     $01
        FCB     $4C                      ; "L"
        FDB     c_i,c_lit_               ; I
        FDB     $0005                    ; 5
        FDB     c_div                    ; /
        FDB     c_1add                   ; 1+
        FDB     c_5p                     ; 5P
        FDB     c_dot                    ; .
        FDB     c_refer                  ; REFER
Z724A   FDB     c_dup                    ; DUP
        FDB     c_ligne,c_lit_           ; _LIGNE
        FDB     $0008                    ; 8
        FDB     c_add                    ; +
        FDB     c_qmend                  ; ?END
        FDB     c_fin                    ; FIN
        FDB     c_at                     ; @
        FDB     c_qmlea                  ; (?LEAVE)
        FDB     c_loop_,Z7222            ; (LOOP) --Z7222--^
Z7260   FDB     c_from_                  ; R>
        FDB     c_dup                    ; DUP
        FDB     c_qmbra,Z729B            ; ?BRANCH --Z729B--v
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_11p                    ; 11P
        FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $45,$74,$20,$6C,$65,$73  ; "Et les"
        FCB     $20                      ; " "
        FDB     c_4p                     ; 4P
        FDB     c_dot                    ; .
        FDB     c_11p                    ; 11P
        FDB     c_dotq1                  ; (."F) len=20
        FCB     $14
        FDB     $3165,$7273,$206F,$6374  ; "1ers oct"
        FDB     $6574,$7320,$6369,$2D61  ; "ets ci-a"
        FCB     $70,$72,$84,$73          ; "près"
        FDB     c_ligne                  ; _LIGNE
        FDB     c_branc,Z729D            ; BRANCH --Z729D--v
Z729B   FDB     c_2drop                  ; 2DROP
Z729D   FDB     c_fin                    ; FIN
        FDB     c_at                     ; @
        FDB     c_qmbra,Z72B9            ; ?BRANCH --Z72B9--v
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=13
        FCB     $0D
        FDB     $5374,$6F70,$2064,$656D  ; "Stop dem"
        FCB     $61,$6E,$64,$80,$21      ; "andé!"
        FDB     c_cr                     ; CR
Z72B9   FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; ECDEB
; ==================================================================
l_ecdeb FDB     l_anat                   ; LFA -> ANAT

n_ecdeb FCB     $85                      ; NFA -> 5
        FCB     $45,$43,$44,$45,$C2      ; "ECDEB"

c_ecdeb FDB     do_var                   ; VARIABLE ECDEB
p_ecdeb FDB     $0000                    ; 0

; ==================================================================
; MEMCTR
; ==================================================================
l_memct FDB     l_ecdeb                  ; LFA -> ECDEB

n_memct FCB     $86                      ; NFA -> 6
        FCB     $4D,$45,$4D,$43,$54,$D2  ; "MEMCTR"

c_memct FDB     do_const                 ; CONSTANT MEMCTR
p_memct FDB     $E787                    ; 59271

; ==================================================================
; ECFIN
; ==================================================================
l_ecfin FDB     l_memct                  ; LFA -> MEMCTR

n_ecfin FCB     $85                      ; NFA -> 5
        FCB     $45,$43,$46,$49,$CE      ; "ECFIN"

c_ecfin FDB     do_var                   ; VARIABLE ECFIN
p_ecfin FDB     $0000                    ; 0

; ==================================================================
; CTRLHD
; ==================================================================
l_ctrlh FDB     l_ligne                  ; LFA -> _LIGNE

n_ctrlh FCB     $86                      ; NFA -> 6
        FCB     $43,$54,$52,$4C,$48,$C4  ; "CTRLHD"

c_ctrlh FDB     do_const                 ; CONSTANT CTRLHD
p_ctrlh FDB     $F010                    ; 61456

; ==================================================================
; MT
; ==================================================================
l_mt    FDB     l_ecfin                  ; LFA -> ECFIN

n_mt    FCB     $82                      ; NFA -> 2
        FCB     $4D,$D4                  ; "MT"

c_mt    FDB     do_col                   ; : MT
p_mt    FDB     c_us,c_lit_              ; US
        FDB     $001B                    ; 27
        FDB     c_ptild,c_lit_           ; P~
        FDB     $0055                    ; 85
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; >>T>>
; ==================================================================
l_gtgtt FDB     l_refer                  ; LFA -> REFER

n_gtgtt FCB     $85                      ; NFA -> 5
        FCB     $3E,$3E,$54,$3E,$BE      ; ">>T>>"

c_gtgtt FDB     do_col                   ; : >>T>>
p_gtgtt FDB     c_mt                     ; MT
        FDB     c_1                      ; 1
        FDB     c_ptild,c_lit_           ; P~
        FDB     $0000                    ; 0
        FDB     c_ptild                  ; P~
        FDB     c_fr                     ; FR
        FDB     c_semi_                  ; (;)

; ==================================================================
; <<T>>
; ==================================================================
l_ltltt FDB     l_dmp                    ; LFA -> DMP

n_ltltt FCB     $85                      ; NFA -> 5
        FCB     $3C,$3C,$54,$3E,$BE      ; "<<T>>"

c_ltltt FDB     do_col                   ; : <<T>>
p_ltltt FDB     c_mt                     ; MT
        FDB     c_0                      ; 0
        FDB     c_ptild,c_lit_           ; P~
        FDB     $0000                    ; 0
        FDB     c_ptild                  ; P~
        FDB     c_fr                     ; FR
        FDB     c_semi_                  ; (;)

; ==================================================================
; SHIFT
; ==================================================================
l_shift FDB     l_ctrlh                  ; LFA -> CTRLHD

n_shift FCB     $85                      ; NFA -> 5
        FCB     $53,$48,$49,$46,$D4      ; "SHIFT"

c_shift FDB     do_col                   ; : SHIFT
p_shift FDB     c_memct                  ; MEMCTR
        FDB     c_dup                    ; DUP
        FDB     c_to_r                   ; >R
        FDB     c_cat,c_lit_             ; C@
        FDB     $0040                    ; 64
        FDB     c_or                     ; OR
        FDB     c_dup                    ; DUP
        FDB     c_from_                  ; R>
        FDB     c_cexc                   ; C!
        FDB     c_ctrlh                  ; CTRLHD
        FDB     c_cexc                   ; C!
        FDB     c_semi_                  ; (;)

; ==================================================================
; _PFA
; ==================================================================
l_pfa   FDB     l_shift                  ; LFA -> SHIFT

n_pfa   FCB     $84                      ; NFA -> 4
        FCB     $5F,$50,$46,$C1          ; "_PFA"

c_pfa   FDB     do_var                   ; VARIABLE _PFA
p_pfa   FDB     $0000                    ; 0

; ==================================================================
; ?QUIT
; ==================================================================
l_qmqui FDB     l_pfa                    ; LFA -> _PFA

n_qmqui FCB     $85                      ; NFA -> 5
        FCB     $3F,$51,$55,$49,$D4      ; "?QUIT"

c_qmqui FDB     do_col                   ; : ?QUIT
p_qmqui FDB     c_qmkey                  ; ?KEY
        FDB     c_dup,c_lit_             ; DUP
        FDB     $001B                    ; 27
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7393            ; ?BRANCH --Z7393--v
        FDB     c_fort0                  ; FORTH
        FDB     c_termi                  ; TERMINAL
        FDB     c_drop                   ; DROP
        FDB     c_divdi                  ; //
        FDB     c_bsp                    ; BSP
        FDB     c_quit                   ; QUIT
        FDB     c_branc,Z73A1            ; BRANCH --Z73A1--v
Z7393   FDB     c_lit_
        FDB     $0020                    ; 32
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z73A1            ; ?BRANCH --Z73A1--v
        FDB     c_key                    ; KEY
        FDB     c_drop                   ; DROP
Z73A1   FDB     c_semi_                  ; (;)

; ==================================================================
; ADREFER
; ==================================================================
l_adref FDB     l_mt                     ; LFA -> MT

n_adref FCB     $87                      ; NFA -> 7
        FCB     $41,$44,$52,$45,$46,$45  ; "ADREFE"
        FCB     $D2                      ; "R"

c_adref FDB     do_col                   ; : ADREFER
p_adref FDB     c_ncu                    ; NCU
        FDB     c_11p                    ; 11P
        FDB     c_cr,c_lit_              ; CR
        FDB     $000B                    ; 11
        FDB     c_udotr,c_lit_           ; U.R
        FDB     $001D                    ; 29
        FDB     c_sp,c_lit_              ; SP
        FDB     $001D                    ; 29
        FDB     c_0                      ; 0
        FDB     c_do_,Z73D1              ; (DO) --Z73D1--v
Z73CB   FDB     c_bs                     ; BS
        FDB     c_loop_,Z73CB            ; (LOOP) --Z73CB--^
Z73D1   FDB     c_semi_                  ; (;)

; ==================================================================
; CONTENU
; ==================================================================
l_cont0 FDB     l_qmqui                  ; LFA -> ?QUIT

n_cont0 FCB     $87                      ; NFA -> 7
        FCB     $43,$4F,$4E,$54,$45,$4E  ; "CONTEN"
        FCB     $D5                      ; "U"

c_cont0 FDB     do_col                   ; : CONTENU
p_cont0 FDB     c_dup                    ; DUP
        FDB     c_3p,c_lit_              ; 3P
        FDB     $0006                    ; 6
        FDB     c_udotr                  ; U.R
        FDB     c_bl                     ; BL
        FDB     c_tild                   ; ~
        FDB     c_14p                    ; 14P
        FDB     c_to_na                  ; >NAME
        FDB     c_iddot                  ; ID.
        FDB     c_semi_                  ; (;)

; ==================================================================
; IDENTITE
; ==================================================================
l_ident FDB     l_adref                  ; LFA -> ADREFER

n_ident FCB     $88                      ; NFA -> 8
        FDB     $4944,$454E,$5449,$54C5  ; "IDENTITE"

c_ident FDB     do_col                   ; : IDENTITE
p_ident FDB     c_pfa                    ; _PFA
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_dup                    ; DUP
        FDB     c_adref                  ; ADREFER
        FDB     c_at                     ; @
        FDB     c_cont0                  ; CONTENU
        FDB     c_semi_                  ; (;)

; ==================================================================
; SAUTADR
; ==================================================================
l_sauta FDB     l_cont0                  ; LFA -> CONTENU

n_sauta FCB     $87                      ; NFA -> 7
        FCB     $53,$41,$55,$54,$41,$44  ; "SAUTAD"
        FCB     $D2                      ; "R"

c_sauta FDB     do_col                   ; : SAUTADR
p_sauta FDB     c_2add                   ; 2+
        FDB     c_dup                    ; DUP
        FDB     c_adref                  ; ADREFER
        FDB     c_at                     ; @
        FDB     c_3p,c_lit_              ; 3P
        FDB     $0006                    ; 6
        FDB     c_udotr                  ; U.R
        FDB     c_6p,c_lit_              ; 6P
        FDB     $0004                    ; 4
        FDB     c_pfa                    ; _PFA
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; <;>
; ==================================================================
l_semi1 FDB     l_ltltt                  ; LFA -> <<T>>

n_semi1 FCB     $83                      ; NFA -> 3
        FCB     $3C,$3B,$BE              ; "<;>"

c_semi1 FDB     do_col                   ; : <;>
p_semi_ FDB     c_ident                  ; IDENTITE
        FDB     c_0p                     ; 0P
        FDB     c_dotq1                  ; (."F) len=17
        FCB     $11
        FDB     $205F,$2046,$696E,$2064  ; " _ Fin d"
        FDB     $8066,$696E,$6974,$696F  ; "éfinitio"
        FCB     $6E                      ; "n"
        FDB     c_drop                   ; DROP
        FDB     c_trait                  ; TRAIT
        FDB     c_divdi                  ; //
        FDB     c_bsp                    ; BSP
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; <LIT>
; ==================================================================
l_lit_0 FDB     l_semi1                  ; LFA -> <;>

n_lit_0 FCB     $85                      ; NFA -> 5
        FCB     $3C,$4C,$49,$54,$BE      ; "<LIT>"

c_lit_0 FDB     do_col                   ; : <LIT>
p_lit_0 FDB     c_ident                  ; IDENTITE
        FDB     c_2add                   ; 2+
        FDB     c_dup                    ; DUP
        FDB     c_dup                    ; DUP
        FDB     c_2add                   ; 2+
        FDB     c_pfa                    ; _PFA
        FDB     c_exc                    ; !
        FDB     c_cr                     ; CR
        FDB     c_11p,c_lit_             ; 11P
        FDB     $000B                    ; 11
        FDB     c_udotr,c_lit_           ; U.R
        FDB     $0007                    ; 7
        FDB     c_sp                     ; SP
        FDB     c_14p                    ; 14P
        FDB     c_at                     ; @
        FDB     c_dot                    ; .
        FDB     c_pfa                    ; _PFA
        FDB     c_at                     ; @
        FDB     c_at,c_lit_              ; @
        FDB     $0056                    ; ' (LIT)
        FDB     c_ltgt                   ; <>
        FDB     c_qmbra,p_lit_0          ; ?BRANCH --p_lit_0--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; <DO>
; ==================================================================
l_do_0  FDB     l_lit_0                  ; LFA -> <LIT>

n_do_0  FCB     $84                      ; NFA -> 4
        FCB     $3C,$44,$4F,$BE          ; "<DO>"

c_do_0  FDB     do_col                   ; : <DO>
p_do_0  FDB     c_ident                  ; IDENTITE
        FDB     c_2add                   ; 2+
        FDB     c_dup                    ; DUP
        FDB     c_adref                  ; ADREFER
        FDB     c_at                     ; @
        FDB     c_3p,c_lit_              ; 3P
        FDB     $0006                    ; 6
        FDB     c_udotr                  ; U.R
        FDB     c_6p                     ; 6P
        FDB     c_dotq1                  ; (."F) len=21
        FCB     $15
        FDB     $205F,$2050,$6F75,$7220  ; " _ Pour "
        FDB     $6669,$6E20,$6465,$2062  ; "fin de b"
        FCB     $6F,$75,$63,$6C,$65      ; "oucle"
        FDB     c_lit_
        FDB     $0004                    ; 4
        FDB     c_pfa                    ; _PFA
        FDB     c_addex                  ; +!
        FDB     c_semi_                  ; (;)

; ==================================================================
; <LOOP>
; ==================================================================
l_loop0 FDB     l_do_0                   ; LFA -> <DO>

n_loop0 FCB     $86                      ; NFA -> 6
        FCB     $3C,$4C,$4F,$4F,$50,$BE  ; "<LOOP>"

c_loop0 FDB     do_col                   ; : <LOOP>
p_loop0 FDB     c_ident                  ; IDENTITE
        FDB     c_sauta                  ; SAUTADR
        FDB     c_dotq1                  ; (."F) len=20
        FCB     $14
        FDB     $205F,$2053,$6920,$4920  ; " _ Si I "
        FDB     $3C20,$7175,$6520,$6C69  ; "< que li"
        FCB     $6D,$69,$74,$65          ; "mite"
        FDB     c_semi_                  ; (;)

; ==================================================================
; <?BRA>
; ==================================================================
l_qmbr0 FDB     l_loop0                  ; LFA -> <LOOP>

n_qmbr0 FCB     $86                      ; NFA -> 6
        FCB     $3C,$3F,$42,$52,$41,$BE  ; "<?BRA>"

c_qmbr0 FDB     do_col                   ; : <?BRA>
p_qmbr0 FDB     c_ident                  ; IDENTITE
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0004                    ; 4
        FDB     c_add                    ; +
        FDB     c_cr                     ; CR
        FDB     c_3p,c_lit_              ; 3P
        FDB     $0011                    ; 17
        FDB     c_udotr                  ; U.R
        FDB     c_6p                     ; 6P
        FDB     c_dotq1                  ; (."F) len=9
        FCB     $09
        FDB     $205F,$2053,$7569,$7465  ; " _ Suite"
        FCB     $20                      ; " "
        FDB     c_dotq1                  ; (."F) len=9
        FCB     $09
        FDB     $7369,$2060,$5472,$7565  ; "si `True"
        FCB     $27                      ; "'"
        FDB     c_sauta                  ; SAUTADR
        FDB     c_dotq1                  ; (."F) len=19
        FCB     $13
        FDB     $205F,$2053,$7569,$7465  ; " _ Suite"
        FDB     $2073,$6920,$6046,$616C  ; " si `Fal"
        FCB     $73,$65,$27              ; "se'"
        FDB     c_semi_                  ; (;)

; ==================================================================
; <BRAN>
; ==================================================================
l_bran_ FDB     l_qmbr0                  ; LFA -> <?BRA>

n_bran_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$42,$52,$41,$4E,$BE  ; "<BRAN>"

c_bran_ FDB     do_col                   ; : <BRAN>
p_bran_ FDB     c_ident                  ; IDENTITE
        FDB     c_6p                     ; 6P
        FDB     c_dotq1                  ; (."F) len=11
        FCB     $0B
        FDB     $2053,$6175,$7420,$666F  ; " Saut fo"
        FCB     $72,$63,$80              ; "rcé"
        FDB     c_sauta                  ; SAUTADR
        FDB     c_dotq1                  ; (."F) len=18
        FCB     $12
        FDB     $205F,$2087,$2063,$6574  ; " _ à cet"
        FDB     $7465,$2061,$6472,$6573  ; "te adres"
        FCB     $73,$65                  ; "se"
        FDB     c_semi_                  ; (;)

; ==================================================================
; <.">
; ==================================================================
l_dotq3 FDB     l_bran_                  ; LFA -> <BRAN>

n_dotq3 FCB     $84                      ; NFA -> 4
        FCB     $3C,$2E,$22,$BE          ; '<.\">'

c_dotq3 FDB     do_col                   ; : <.">
p_dotq3 FDB     c_ident                  ; IDENTITE
        FDB     c_2add                   ; 2+
        FDB     c_cr,c_lit_              ; CR
        FDB     $0012                    ; 18
        FDB     c_sp,c_lit_              ; SP
        FDB     $0003                    ; 3
        FDB     c_pfa                    ; _PFA
        FDB     c_addex                  ; +!
        FDB     c_5p                     ; 5P
        FDB     c_cat                    ; C@
        FDB     c_0                      ; 0
        FDB     c_do_,Z75F1              ; (DO) --Z75F1--v
Z75BD   FDB     c_pfa                    ; _PFA
        FDB     c_at                     ; @
        FDB     c_cat                    ; C@
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z75D1            ; ?BRANCH --Z75D1--v
        FDB     c_frpti                  ; FRP~
        FDB     c_branc,Z75D3            ; BRANCH --Z75D3--v
Z75D1   FDB     c_tild                   ; ~
Z75D3   FDB     c_1                      ; 1
        FDB     c_pfa                    ; _PFA
        FDB     c_addex                  ; +!
        FDB     c_i,c_lit_               ; I
        FDB     $0014                    ; 20
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z75ED            ; ?BRANCH --Z75ED--v
        FDB     c_cr,c_lit_              ; CR
        FDB     $0013                    ; 19
        FDB     c_sp                     ; SP
Z75ED   FDB     c_loop_,Z75BD            ; (LOOP) --Z75BD--^
Z75F1   FDB     c_semi_                  ; (;)

; ==================================================================
; PRESENTE
; ==================================================================
l_prese FDB     l_dotq3                  ; LFA -> <.">

n_prese FCB     $88                      ; NFA -> 8
        FDB     $5052,$4553,$454E,$54C5  ; "PRESENTE"

c_prese FDB     do_col                   ; : PRESENTE
p_prese FDB     c_ncu                    ; NCU
        FDB     c_6p                     ; 6P
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=20
        FCB     $14
        FDB     $4C65,$206D,$6F74,$2087  ; "Le mot à"
        FDB     $2044,$4543,$4F4D,$5049  ; " DECOMPI"
        FCB     $4C,$45,$52,$20          ; "LER "
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $80,$74,$61,$6E,$74,$20  ; "étant "
        FDB     c_3p                     ; 3P
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=3
        FCB     $03
        FCB     $58,$58,$58              ; "XXX"
        FDB     c_6p                     ; 6P
        FDB     c_dotq1                  ; (."F) len=10
        FCB     $0A
        FDB     $2C20,$656E,$7472,$6572  ; ", entrer"
        FCB     $20,$60                  ; " `"
        FDB     c_3p                     ; 3P
        FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $44,$44,$44,$20,$58,$58  ; "DDD XX"
        FCB     $58                      ; "X"
        FDB     c_6p                     ; 6P
        FDB     c_dotq1                  ; (."F) len=2
        FCB     $02
        FCB     $27,$2E                  ; "'."
        FDB     c_1                      ; 1
        FDB     c_2                      ; 2
        FDB     c_at0                    ; AT
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; SECURITE
; ==================================================================
l_secur FDB     l_sauta                  ; LFA -> SAUTADR

n_secur FCB     $88                      ; NFA -> 8
        FDB     $5345,$4355,$5249,$54C5  ; "SECURITE"

c_secur FDB     do_col                   ; : SECURITE
p_secur FDB     c_dotq1                  ; (."F) len=17
        FCB     $11
        FDB     $5680,$7269,$6669,$6361  ; "Vérifica"
        FDB     $7469,$6F6E,$2070,$6172  ; "tion par"
        FCB     $20                      ; " "
        FDB     c_6p                     ; 6P
        FDB     c_dotq1                  ; (."F) len=4
        FCB     $04
        FCB     $44,$4D,$50,$20          ; "DMP "
        FDB     c_14p                    ; 14P
        FDB     c_dotq1                  ; (."F) len=14
        FCB     $0E
        FDB     $696E,$6469,$7370,$656E  ; "indispen"
        FCB     $73,$61,$62,$6C,$65,$21  ; "sable!"
        FDB     c_cr                     ; CR
        FDB     c_2                      ; 2
        FDB     c_sp                     ; SP
        FDB     c_dotq1                  ; (."F) len=27
        FCB     $1B
        FDB     $284C,$696D,$6974,$6520  ; "(Limite "
        FDB     $6475,$206D,$6F74,$2073  ; "du mot s"
        FDB     $7569,$7661,$6E74,$2070  ; "uivant p"
        FCB     $61,$72,$20              ; "ar "
        FDB     c_6p                     ; 6P
        FDB     c_dotq1                  ; (."F) len=8
        FCB     $08
        FDB     $3E57,$4F52,$4453,$2029  ; ">WORDS )"
        FDB     c_14p                    ; 14P
        FDB     c_semi_                  ; (;)

; ==================================================================
; CODES-OP
; ==================================================================
l_codes FDB     l_secur                  ; LFA -> SECURITE

n_codes FCB     $88                      ; NFA -> 8
        FDB     $434F,$4445,$532D,$4FD0  ; "CODES-OP"

c_codes FDB     do_col                   ; : CODES-OP
p_codes FDB     c_do_,Z7719              ; (DO) --Z7719--v
Z76E1   FDB     c_1add                   ; 1+
        FDB     c_dup                    ; DUP
        FDB     c_dup                    ; DUP
        FDB     c_dec0                   ; DEC0
        FDB     c_11p,c_lit_             ; 11P
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_space                  ; SPACE
        FDB     c_cat                    ; C@
        FDB     c_3p                     ; 3P
        FDB     c_2hdot                  ; 2H.
        FDB     c_i                      ; I
        FDB     c_1add                   ; 1+
        FDB     c_space                  ; SPACE
        FDB     c_2                      ; 2
        FDB     c_mod                    ; MOD
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z7711            ; ?BRANCH --Z7711--v
        FDB     c_cr,c_lit_              ; CR
        FDB     $0012                    ; 18
        FDB     c_sp                     ; SP
Z7711   FDB     c_qmkey                  ; ?KEY
        FDB     c_qmlea                  ; (?LEAVE)
        FDB     c_loop_,Z76E1            ; (LOOP) --Z76E1--^
Z7719   FDB     c_1p                     ; 1P
        FDB     c_cr,c_lit_              ; CR
        FDB     $0012                    ; 18
        FDB     c_sp                     ; SP
        FDB     c_semi_                  ; (;)

; ==================================================================
; ANACODE
; ==================================================================
l_anaco FDB     l_ident                  ; LFA -> IDENTITE

n_anaco FCB     $87                      ; NFA -> 7
        FCB     $41,$4E,$41,$43,$4F,$44  ; "ANACOD"
        FCB     $C5                      ; "E"

c_anaco FDB     do_col                   ; : ANACODE
p_anaco FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_dotq1                  ; (."F) len=8
        FCB     $08
        FDB     $436F,$6465,$732D,$4F50  ; "Codes-OP"
Z7740   FDB     c_over                   ; OVER
        FDB     c_at,c_lit_              ; @
        FDB     $0E12                    ; 3602
        FDB     c_ltgt                   ; <>
        FDB     c_to_r                   ; >R
        FDB     c_over                   ; OVER
        FDB     c_at,c_lit_              ; @
        FDB     $003C                    ; ' (;)
        FDB     c_ltgt                   ; <>
        FDB     c_from_                  ; R>
        FDB     c_and                    ; AND
        FDB     c_qmbra,Z776A            ; ?BRANCH --Z776A--v
        FDB     c_1add                   ; 1+
        FDB     c_to_r                   ; >R
        FDB     c_1add                   ; 1+
        FDB     c_from_                  ; R>
        FDB     c_branc,Z7740            ; BRANCH --Z7740--^
Z776A   FDB     c_nip                    ; NIP
        FDB     c_dup                    ; DUP
        FDB     c_1p,c_lit_              ; 1P
        FDB     $0003                    ; 3
        FDB     c_dotr,c_lit_            ; .R
        FDB     $0007                    ; 7
        FDB     c_sp                     ; SP
        FDB     c_6p                     ; 6P
        FDB     c_pfa                    ; _PFA
        FDB     c_at                     ; @
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_0                      ; 0
        FDB     c_codes                  ; CODES-OP
        FDB     c_dotq1                  ; (."F) len=18
        FCB     $12
        FDB     $4669,$6E20,$436F,$6465  ; "Fin Code"
        FDB     $2D4F,$5020,$2848,$6578  ; "-OP (Hex"
        FCB     $61,$29                  ; "a)"
        FDB     c_trait                  ; TRAIT
        FDB     c_2                      ; 2
        FDB     c_sp                     ; SP
        FDB     c_14p                    ; 14P
        FDB     c_secur                  ; SECURITE
        FDB     c_trait                  ; TRAIT
        FDB     c_drop                   ; DROP
        FDB     c_dec0                   ; DEC0
        FDB     c_divdi                  ; //
        FDB     c_bsp                    ; BSP
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; <;CODE>
; ==================================================================
l_semi2 FDB     l_prese                  ; LFA -> PRESENTE

n_semi2 FCB     $87                      ; NFA -> 7
        FCB     $3C,$3B,$43,$4F,$44,$45  ; "<;CODE"
        FCB     $BE                      ; ">"

c_semi2 FDB     do_col                   ; : <;CODE>
p_semi1 FDB     c_ident                  ; IDENTITE
        FDB     c_0                      ; 0
        FDB     c_anaco                  ; ANACODE
        FDB     c_semi_                  ; (;)

; ==================================================================
; <CODE>
; ==================================================================
l_code_ FDB     l_semi2                  ; LFA -> <;CODE>

n_code_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$43,$4F,$44,$45,$BE  ; "<CODE>"

c_code_ FDB     do_col                   ; : <CODE>
p_code_ FDB     c_pfa                    ; _PFA
        FDB     c_at                     ; @
        FDB     c_2sub                   ; 2-
        FDB     c_dup                    ; DUP
        FDB     c_adref                  ; ADREFER
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=18
        FCB     $12
        FDB     $2050,$7269,$6D69,$7469  ; " Primiti"
        FDB     $7665,$2065,$6E20,$636F  ; "ve en co"
        FCB     $64,$65                  ; "de"
        FDB     c_14p,c_lit_             ; 14P
        FDB     $FFFC                    ; -4
        FDB     c_pfa                    ; _PFA
        FDB     c_addex                  ; +!
        FDB     c_2                      ; 2
        FDB     c_anaco                  ; ANACODE
        FDB     c_semi_                  ; (;)

; ==================================================================
; EXCEPTED
; ==================================================================
l_excep FDB     l_anaco                  ; LFA -> ANACODE

n_excep FCB     $88                      ; NFA -> 8
        FDB     $4558,$4345,$5054,$45C4  ; "EXCEPTED"

c_excep FDB     do_col                   ; : EXCEPTED
p_excep FDB     c_lit_
        FDB     $003C                    ; ' (;)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7828            ; ?BRANCH --Z7828--v
        FDB     c_drop                   ; DROP
        FDB     c_semi1                  ; <;>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z7828   FDB     c_lit_
        FDB     $439B                    ; ' (")
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z783C            ; ?BRANCH --Z783C--v
        FDB     c_drop                   ; DROP
        FDB     c_dotq3                  ; <.">
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z783C   FDB     c_lit_
        FDB     $08F5                    ; ' (.")
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7850            ; ?BRANCH --Z7850--v
        FDB     c_drop                   ; DROP
        FDB     c_dotq3                  ; <.">
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z7850   FDB     c_lit_
        FDB     $4150                    ; ' (."F)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7864            ; ?BRANCH --Z7864--v
        FDB     c_drop                   ; DROP
        FDB     c_dotq3                  ; <.">
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z7864   FDB     c_lit_
        FDB     $094D                    ; ' (ABORT")
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7878            ; ?BRANCH --Z7878--v
        FDB     c_drop                   ; DROP
        FDB     c_dotq3                  ; <.">
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z7878   FDB     c_lit_
        FDB     $0056                    ; ' (LIT)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z788C            ; ?BRANCH --Z788C--v
        FDB     c_drop                   ; DROP
        FDB     c_lit_0                  ; <LIT>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z788C   FDB     c_lit_
        FDB     $0632                    ; ' (DO)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z78A0            ; ?BRANCH --Z78A0--v
        FDB     c_drop                   ; DROP
        FDB     c_do_0                   ; <DO>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z78A0   FDB     c_lit_
        FDB     $064C                    ; ' (?DO)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z78B4            ; ?BRANCH --Z78B4--v
        FDB     c_drop                   ; DROP
        FDB     c_do_0                   ; <DO>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z78B4   FDB     c_lit_
        FDB     $0664                    ; ' (LOOP)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z78C8            ; ?BRANCH --Z78C8--v
        FDB     c_drop                   ; DROP
        FDB     c_loop0                  ; <LOOP>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z78C8   FDB     c_lit_
        FDB     $0682                    ; ' (+LOOP)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z78DC            ; ?BRANCH --Z78DC--v
        FDB     c_drop                   ; DROP
        FDB     c_loop0                  ; <LOOP>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z78DC   FDB     c_lit_
        FDB     $0610                    ; ' ?BRANCH
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z78F0            ; ?BRANCH --Z78F0--v
        FDB     c_drop                   ; DROP
        FDB     c_qmbr0                  ; <?BRA>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z78F0   FDB     c_lit_
        FDB     $0629                    ; ' BRANCH
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7904            ; ?BRANCH --Z7904--v
        FDB     c_drop                   ; DROP
        FDB     c_bran_                  ; <BRAN>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z7904   FDB     c_lit_
        FDB     $116A                    ; ' (;CODE)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7918            ; ?BRANCH --Z7918--v
        FDB     c_drop                   ; DROP
        FDB     c_semi2                  ; <;CODE>
        FDB     c_branc,Z7924            ; BRANCH --Z7924--v
Z7918   FDB     c_ident                  ; IDENTITE
        FDB     c_drop                   ; DROP
        FDB     c_2                      ; 2
        FDB     c_pfa                    ; _PFA
        FDB     c_addex                  ; +!
        FDB     c_drop                   ; DROP
Z7924   FDB     c_semi_                  ; (;)

; ==================================================================
; KOADANS
; ==================================================================
l_koada FDB     l_codes                  ; LFA -> CODES-OP

n_koada FCB     $87                      ; NFA -> 7
        FCB     $4B,$4F,$41,$44,$41,$4E  ; "KOADAN"
        FCB     $D3                      ; "S"

c_koada FDB     do_col                   ; : KOADANS
p_koada FDB     c_adref                  ; ADREFER
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=10
        FCB     $0A
        FDB     $2043,$6F6E,$7465,$6E75  ; " Contenu"
        FCB     $3A,$20                  ; ": "
        FDB     c_at                     ; @
        FDB     c_3p                     ; 3P
        FDB     c_udot                   ; U.
        FDB     c_trait                  ; TRAIT
        FDB     c_divdi                  ; //
        FDB     c_bsp                    ; BSP
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; B-ANALY
; ==================================================================
l_bsuba FDB     l_gtgtt                  ; LFA -> >>T>>

n_bsuba FCB     $87                      ; NFA -> 7
        FCB     $42,$2D,$41,$4E,$41,$4C  ; "B-ANAL"
        FCB     $D9                      ; "Y"

c_bsuba FDB     do_col                   ; : B-ANALY
p_bsuba FDB     c_pfa                    ; _PFA
        FDB     c_at                     ; @
        FDB     c_at                     ; @
        FDB     c_excep                  ; EXCEPTED
        FDB     c_qmqui                  ; ?QUIT
        FDB     c_branc,p_bsuba          ; BRANCH --p_bsuba--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; DEF(CODE)
; ==================================================================
l_deflp FDB     l_code_                  ; LFA -> <CODE>

n_deflp FCB     $89                      ; NFA -> 9
        FDB     $4445,$4628,$434F,$4445  ; "DEF(CODE"
        FCB     $A9                      ; ")"

c_deflp FDB     do_const                 ; CONSTANT DEF(CODE)
p_deflp FDB     do_col                   ; 4597

; ==================================================================
; CON(CODE)
; ==================================================================
l_conlp FDB     l_koada                  ; LFA -> KOADANS

n_conlp FCB     $89                      ; NFA -> 9
        FDB     $434F,$4E28,$434F,$4445  ; "CON(CODE"
        FCB     $A9                      ; ")"

c_conlp FDB     do_const                 ; CONSTANT CON(CODE)
p_conlp FDB     do_const                 ; 4534

; ==================================================================
; VAR(CODE)
; ==================================================================
l_varlp FDB     l_bsuba                  ; LFA -> B-ANALY

n_varlp FCB     $89                      ; NFA -> 9
        FDB     $5641,$5228,$434F,$4445  ; "VAR(CODE"
        FCB     $A9                      ; ")"

c_varlp FDB     do_const                 ; CONSTANT VAR(CODE)
p_varlp FDB     do_var                   ; 6193

; ==================================================================
; USE(CODE)
; ==================================================================
l_uselp FDB     l_excep                  ; LFA -> EXCEPTED

n_uselp FCB     $89                      ; NFA -> 9
        FDB     $5553,$4528,$434F,$4445  ; "USE(CODE"
        FCB     $A9                      ; ")"

c_uselp FDB     do_const                 ; CONSTANT USE(CODE)
p_uselp FDB     getUVAR                  ; 6845

; ==================================================================
; VOC(CODE)
; ==================================================================
l_voclp FDB     l_varlp                  ; LFA -> VAR(CODE)

n_voclp FCB     $89                      ; NFA -> 9
        FDB     $564F,$4328,$434F,$4445  ; "VOC(CODE"
        FCB     $A9                      ; ")"

c_voclp FDB     do_const                 ; CONSTANT VOC(CODE)
p_voclp FDB     do_voc                   ; 5854

; ==================================================================
; <VARS>
; ==================================================================
l_vars_ FDB     l_deflp                  ; LFA -> DEF(CODE)

n_vars_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$56,$41,$52,$53,$BE  ; "<VARS>"

c_vars_ FDB     do_col                   ; : <VARS>
p_vars_ FDB     c_cr,c_lit_              ; CR
        FDB     $000C                    ; 12
        FDB     c_sp                     ; SP
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=18
        FCB     $12
        FDB     $5661,$7269,$6162,$6C65  ; "Variable"
        FDB     $206F,$7264,$696E,$6169  ; " ordinai"
        FCB     $72,$65                  ; "re"
        FDB     c_2add                   ; 2+
        FDB     c_dup                    ; DUP
        FDB     c_koada                  ; KOADANS
        FDB     c_semi_                  ; (;)

; ==================================================================
; <USER>
; ==================================================================
l_user_ FDB     l_vars_                  ; LFA -> <VARS>

n_user_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$55,$53,$45,$52,$BE  ; "<USER>"

c_user_ FDB     do_col                   ; : <USER>
p_user_ FDB     c_cr,c_lit_              ; CR
        FDB     $000C                    ; 12
        FDB     c_sp                     ; SP
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=15
        FCB     $0F
        FDB     $5661,$7269,$6162,$6C65  ; "Variable"
        FCB     $20,$3C,$55,$73,$65,$72  ; " <User"
        FCB     $3E                      ; ">"
        FDB     c_2add                   ; 2+
        FDB     c_at                     ; @
        FDB     c_up                     ; UP
        FDB     c_at                     ; @
        FDB     c_add                    ; +
        FDB     c_dup                    ; DUP
        FDB     c_koada                  ; KOADANS
        FDB     c_semi_                  ; (;)

; ==================================================================
; <CONS>
; ==================================================================
l_cons_ FDB     l_user_                  ; LFA -> <USER>

n_cons_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$43,$4F,$4E,$53,$BE  ; "<CONS>"

c_cons_ FDB     do_col                   ; : <CONS>
p_cons_ FDB     c_cr,c_lit_              ; CR
        FDB     $000C                    ; 12
        FDB     c_sp                     ; SP
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=9
        FCB     $09
        FDB     $436F,$6E73,$7461,$6E74  ; "Constant"
        FCB     $65                      ; "e"
        FDB     c_2add                   ; 2+
        FDB     c_dup                    ; DUP
        FDB     c_koada                  ; KOADANS
        FDB     c_semi_                  ; (;)

; ==================================================================
; <VOCS>
; ==================================================================
l_vocs_ FDB     l_cons_                  ; LFA -> <CONS>

n_vocs_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$56,$4F,$43,$53,$BE  ; "<VOCS>"

c_vocs_ FDB     do_col                   ; : <VOCS>
p_vocs_ FDB     c_cr,c_lit_              ; CR
        FDB     $000C                    ; 12
        FDB     c_sp                     ; SP
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=18
        FCB     $12
        FDB     $4368,$616E,$6774,$2056  ; "Changt V"
        FDB     $6F63,$6162,$756C,$6169  ; "ocabulai"
        FCB     $72,$65                  ; "re"
        FDB     c_drop                   ; DROP
        FDB     c_trait                  ; TRAIT
        FDB     c_divdi                  ; //
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; <DEFI>
; ==================================================================
l_defi_ FDB     l_vocs_                  ; LFA -> <VOCS>

n_defi_ FCB     $86                      ; NFA -> 6
        FCB     $3C,$44,$45,$46,$49,$BE  ; "<DEFI>"

c_defi_ FDB     do_col                   ; : <DEFI>
p_defi_ FDB     c_cr,c_lit_              ; CR
        FDB     $000C                    ; 12
        FDB     c_sp                     ; SP
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=11
        FCB     $0B
        FDB     $4445,$4649,$4E49,$5449  ; "DEFINITI"
        FCB     $4F,$4E,$20              ; "ON "
        FDB     c_lit_
        FDB     $0022                    ; 34
        FDB     c_tild,c_lit_            ; ~
        FDB     $003A                    ; 58
        FDB     c_tild,c_lit_            ; ~
        FDB     $0022                    ; 34
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; QUELCFA
; ==================================================================
l_quelc FDB     l_uselp                  ; LFA -> USE(CODE)

n_quelc FCB     $87                      ; NFA -> 7
        FCB     $51,$55,$45,$4C,$43,$46  ; "QUELCF"
        FCB     $C1                      ; "A"

c_quelc FDB     do_col                   ; : QUELCFA
p_quelc FDB     c_at                     ; @
        FDB     c_2dup                   ; 2DUP
        FDB     c_2sub                   ; 2-
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7AE2            ; ?BRANCH --Z7AE2--v
        FDB     c_2add                   ; 2+
        FDB     c_pfa                    ; _PFA
        FDB     c_exc                    ; !
        FDB     c_drop                   ; DROP
        FDB     c_code_                  ; <CODE>
        FDB     c_branc,Z7B3E            ; BRANCH --Z7B3E--v
Z7AE2   FDB     c_deflp                  ; DEF(CODE)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7AF4            ; ?BRANCH --Z7AF4--v
        FDB     c_drop                   ; DROP
        FDB     c_defi_                  ; <DEFI>
        FDB     c_branc,Z7B3E            ; BRANCH --Z7B3E--v
Z7AF4   FDB     c_varlp                  ; VAR(CODE)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7B06            ; ?BRANCH --Z7B06--v
        FDB     c_drop                   ; DROP
        FDB     c_vars_                  ; <VARS>
        FDB     c_branc,Z7B3E            ; BRANCH --Z7B3E--v
Z7B06   FDB     c_conlp                  ; CON(CODE)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7B18            ; ?BRANCH --Z7B18--v
        FDB     c_drop                   ; DROP
        FDB     c_cons_                  ; <CONS>
        FDB     c_branc,Z7B3E            ; BRANCH --Z7B3E--v
Z7B18   FDB     c_uselp                  ; USE(CODE)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7B2A            ; ?BRANCH --Z7B2A--v
        FDB     c_drop                   ; DROP
        FDB     c_user_                  ; <USER>
        FDB     c_branc,Z7B3E            ; BRANCH --Z7B3E--v
Z7B2A   FDB     c_voclp                  ; VOC(CODE)
        FDB     c_over                   ; OVER
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7B3C            ; ?BRANCH --Z7B3C--v
        FDB     c_drop                   ; DROP
        FDB     c_vocs_                  ; <VOCS>
        FDB     c_branc,Z7B3E            ; BRANCH --Z7B3E--v
Z7B3C   FDB     c_drop                   ; DROP
Z7B3E   FDB     c_to_bo                  ; >BODY
        FDB     c_11p                    ; 11P
        FDB     c_trait                  ; TRAIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; DDD
; ==================================================================
l_ddd   FDB     l_defi_                  ; LFA -> <DEFI>

n_ddd   FCB     $83                      ; NFA -> 3
        FCB     $44,$44,$C4              ; "DDD"

c_ddd   FDB     do_col                   ; : DDD
p_ddd   FDB     c_cls                    ; CLS
        FDB     c_dotq1                  ; (."F) len=17
        FCB     $11
        FDB     $536F,$7274,$6965,$2073  ; "Sortie s"
        FDB     $7572,$204D,$542D,$3830  ; "ur MT-80"
        FCB     $3F                      ; "?"
        FDB     c_ydivn                  ; Y/N
        FDB     c_qmbra,Z7B6E            ; ?BRANCH --Z7B6E--v
        FDB     c_print                  ; PRINTER
        FDB     c_bsp                    ; BSP
Z7B6E   FDB     c_cls                    ; CLS
        FDB     c_ncu                    ; NCU
        FDB     c_0                      ; 0
        FDB     c_pfa                    ; _PFA
        FDB     c_exc                    ; !
        FDB     c_11p                    ; 11P
        FDB     c_bl                     ; BL
        FDB     c_word                   ; WORD
        FDB     c_find                   ; FIND
        FDB     c_qmbra,Z7C0E            ; ?BRANCH --Z7C0E--v
        FDB     c_dup                    ; DUP
        FDB     c_rapel                  ; RAPELTIT
        FDB     c_trait                  ; TRAIT
        FDB     c_dup                    ; DUP
        FDB     c_to_li                  ; >LINK
        FDB     c_dup                    ; DUP
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $4C,$69,$6E,$6B,$20,$3A  ; "Link :"
        FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_lsuba                  ; L-ANALY
        FDB     c_trait                  ; TRAIT
        FDB     c_dup                    ; DUP
        FDB     c_to_na                  ; >NAME
        FDB     c_dup                    ; DUP
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $4E,$61,$6D,$65,$20,$3A  ; "Name :"
        FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_nsuba                  ; N-ANALY
        FDB     c_trait                  ; TRAIT
        FDB     c_dup                    ; DUP
        FDB     c_dup                    ; DUP
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $43,$6F,$64,$65,$20,$3A  ; "Code :"
        FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_14p                    ; 14P
        FDB     c_dotq1                  ; (."F) len=25
        FCB     $19
        FDB     $2028,$556E,$6520,$6365  ; " (Une ce"
        FDB     $6C6C,$756C,$652F,$3220  ; "llule/2 "
        FDB     $4164,$7265,$7373,$6573  ; "Adresses"
        FCB     $29                      ; ")"
        FDB     c_quelc                  ; QUELCFA
        FDB     c_dup                    ; DUP
        FDB     c_pfa                    ; _PFA
        FDB     c_exc                    ; !
        FDB     c_dotq1                  ; (."F) len=6
        FCB     $06
        FCB     $42,$6F,$64,$79,$20,$3A  ; "Body :"
        FDB     c_lit_
        FDB     $0005                    ; 5
        FDB     c_udotr                  ; U.R
        FDB     c_rape0                  ; RAPELBODY
        FDB     c_trait                  ; TRAIT
        FDB     c_bsuba                  ; B-ANALY
        FDB     c_branc,Z7C32            ; BRANCH --Z7C32--v
Z7C0E   FDB     c_drop,c_lit_            ; DROP
        FDB     $0004                    ; 4
        FDB     c_al                     ; AL
        FDB     c_dotq1                  ; (."F) len=19
        FCB     $13
        FDB     $4365,$206D,$6F74,$2065  ; "Ce mot e"
        FDB     $7374,$2069,$6E63,$6F6E  ; "st incon"
        FCB     $6E,$75,$21              ; "nu!"
        FDB     c_cr                     ; CR
        FDB     c_divdi                  ; //
        FDB     c_cr                     ; CR
Z7C32   FDB     c_bsp                    ; BSP
        FDB     c_cr                     ; CR
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; .TB
; ==================================================================
l_dottb FDB     l_voclp                  ; LFA -> VOC(CODE)

n_dottb FCB     $83                      ; NFA -> 3
        FCB     $2E,$54,$C2              ; ".TB"

c_dottb FDB     do_col                   ; : .TB
p_dottb FDB     c_bin                    ; BIN
        FDB     c_2                      ; 2
        FDB     c_sp                     ; SP
        FDB     c_6p                     ; 6P
        FDB     c_tib                    ; TIB
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_                     ; #
        FDB     c_n_gt                   ; #>
        FDB     c_type                   ; TYPE
        FDB     c_dec0                   ; DEC0
        FDB     c_cr                     ; CR
        FDB     c_semi_                  ; (;)

; ==================================================================
; DHB
; ==================================================================
l_dhb   FDB     l_ddd                    ; LFA -> DDD

n_dhb   FCB     $83                      ; NFA -> 3
        FCB     $44,$48,$C2              ; "DHB"

c_dhb   FDB     do_col                   ; : DHB
p_dhb   FDB     c_cr                     ; CR
        FDB     c_over                   ; OVER
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_ncu                    ; NCU
        FDB     c_do_,Z7CA6              ; (DO) --Z7CA6--v
Z7C80   FDB     c_3p                     ; 3P
        FDB     c_i                      ; I
        FDB     c_udot                   ; U.
        FDB     c_space                  ; SPACE
        FDB     c_14p                    ; 14P
        FDB     c_i                      ; I
        FDB     c_cat                    ; C@
        FDB     c_dup                    ; DUP
        FDB     c_2hdot                  ; 2H.
        FDB     c_space                  ; SPACE
        FDB     c_11p                    ; 11P
        FDB     c_dup,c_lit_             ; DUP
        FDB     $0004                    ; 4
        FDB     c_dotr                   ; .R
        FDB     c_dottb                  ; .TB
        FDB     c_qmqui                  ; ?QUIT
        FDB     c_loop_,Z7C80            ; (LOOP) --Z7C80--^
Z7CA6   FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; E-T
; ==================================================================
l_esubt FDB     l_quelc                  ; LFA -> QUELCFA

n_esubt FCB     $83                      ; NFA -> 3
        FCB     $45,$2D,$D4              ; "E-T"

c_esubt FDB     do_col                   ; : E-T
p_esubt FDB     c_ncu                    ; NCU
        FDB     c_cls                    ; CLS
        FDB     c_4p                     ; 4P
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=14
        FCB     $0E
        FDB     $4445,$434F,$4D50,$494C  ; "DECOMPIL"
        FCB     $41,$54,$45,$55,$52,$20  ; "ATEUR "
        FDB     c_trait                  ; TRAIT
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=25
        FCB     $19
        FDB     $4D54,$2D38,$3020,$656E  ; "MT-80 en"
        FDB     $206F,$7264,$7265,$2064  ; " ordre d"
        FDB     $6520,$6D61,$7263,$6865  ; "e marche"
        FCB     $3F                      ; "?"
        FDB     c_ydivn                  ; Y/N
        FDB     c_qmbra,Z7CFF            ; ?BRANCH --Z7CFF--v
        FDB     c_doldo                  ; $$
        FDB     c_prese                  ; PRESENTE
Z7CFF   FDB     c_semi_                  ; (;)

; ==================================================================
; MEMO
; ==================================================================
l_memo  FDB     l_esubt                  ; LFA -> E-T

n_memo  FCB     $84                      ; NFA -> 4
        FCB     $4D,$45,$4D,$CF          ; "MEMO"

c_memo  FDB     do_var                   ; VARIABLE MEMO
p_memo  FDB     $0541                    ; 1345
        FCB     $4C,$4C,$4F,$54,$20,$01,$DE,$0E,$76,$43,$67,$3B,$67
        FCB     $0D,$EF,$46,$4E,$00,$3C,$7C,$5E,$85,$54,$4F,$43,$48
        FCB     $B1,$11,$F5

; ==================================================================
; FLG
; ==================================================================
l_flg   FDB     l_dottb                  ; LFA -> .TB

n_flg   FCB     $83                      ; NFA -> 3
        FCB     $46,$4C,$C7              ; "FLG"

c_flg   FDB     do_var                   ; VARIABLE FLG
p_flg   FDB     $0000                    ; 0

; ==================================================================
; >MIN
; ==================================================================
l_to_mi FDB     l_flg                    ; LFA -> FLG

n_to_mi FCB     $84                      ; NFA -> 4
        FCB     $3E,$4D,$49,$CE          ; ">MIN"

c_to_mi FDB     do_col                   ; : >MIN
p_to_mi FDB     c_dup,c_lit_             ; DUP
        FDB     $0041,c_lit_             ; 65
        FDB     $005A                    ; 90
        FDB     c_eq_                    ; <=>
        FDB     c_qmbra,Z7D52,c_lit_     ; ?BRANCH --Z7D52--v
        FDB     $0020                    ; 32
        FDB     c_add                    ; +
Z7D52   FDB     c_semi_                  ; (;)

; ==================================================================
; IDEM
; ==================================================================
l_idem  FDB     l_memo                   ; LFA -> MEMO

n_idem  FCB     $84                      ; NFA -> 4
        FCB     $49,$44,$45,$CD          ; "IDEM"

c_idem  FDB     do_col                   ; : IDEM
p_idem  FDB     c_memo                   ; MEMO
        FDB     c_cat                    ; C@
        FDB     c_0                      ; 0
        FDB     c_do_,Z7D8D              ; (DO) --Z7D8D--v
Z7D67   FDB     c_dup                    ; DUP
        FDB     c_i                      ; I
        FDB     c_add                    ; +
        FDB     c_cat                    ; C@
        FDB     c_to_mi                  ; >MIN
        FDB     c_memo                   ; MEMO
        FDB     c_i                      ; I
        FDB     c_1add                   ; 1+
        FDB     c_add                    ; +
        FDB     c_cat                    ; C@
        FDB     c_to_mi                  ; >MIN
        FDB     c_ltgt                   ; <>
        FDB     c_qmbra,Z7D89            ; ?BRANCH --Z7D89--v
        FDB     c_drop                   ; DROP
        FDB     c_0                      ; 0
        FDB     c_leave                  ; (LEAVE)
Z7D89   FDB     c_loop_,Z7D67            ; (LOOP) --Z7D67--^
Z7D8D   FDB     c_0                      ; 0
        FDB     c_ltgt                   ; <>
        FDB     c_semi_                  ; (;)

; ==================================================================
; "~
; ==================================================================
l_qttil FDB     l_to_mi                  ; LFA -> >MIN

n_qttil FCB     $82                      ; NFA -> 2
        FCB     $22,$FE                  ; '\"~'

c_qttil FDB     do_col                   ; : "~
p_qttil FDB     c_14p,c_lit_             ; 14P
        FDB     $0022                    ; 34
        FDB     c_tild                   ; ~
        FDB     c_semi_                  ; (;)

; ==================================================================
; KOA
; ==================================================================
l_koa   FDB     l_conlp                  ; LFA -> CON(CODE)

n_koa   FCB     $83                      ; NFA -> 3
        FCB     $4B,$4F,$C1              ; "KOA"

c_koa   FDB     do_col                   ; : KOA
p_koa   FDB     c_memo                   ; MEMO
        FDB     c_1add                   ; 1+
        FDB     c_dup                    ; DUP
        FDB     c_1sub                   ; 1-
        FDB     c_cat                    ; C@
        FDB     c_qttil                  ; "~
        FDB     c_3p                     ; 3P
        FDB     c_type                   ; TYPE
        FDB     c_qttil                  ; "~
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z7DCA            ; ?BRANCH --Z7DCA--v
        FDB     c_cr                     ; CR
Z7DCA   FDB     c_semi_                  ; (;)

; ==================================================================
; INMEMO
; ==================================================================
l_inmem FDB     l_idem                   ; LFA -> IDEM

n_inmem FCB     $86                      ; NFA -> 6
        FCB     $49,$4E,$4D,$45,$4D,$CF  ; "INMEMO"

c_inmem FDB     do_col                   ; : INMEMO
p_inmem FDB     c_space                  ; SPACE
        FDB     c_memo                   ; MEMO
        FDB     c_1add                   ; 1+
        FDB     c_width                  ; WIDTH
        FDB     c_expec                  ; EXPECT
        FDB     c_span                   ; SPAN
        FDB     c_at                     ; @
        FDB     c_memo                   ; MEMO
        FDB     c_cexc                   ; C!
        FDB     c_ncu                    ; NCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; DIKOA
; ==================================================================
l_dikoa FDB     l_dhb                    ; LFA -> DHB

n_dikoa FCB     $85                      ; NFA -> 5
        FCB     $44,$49,$4B,$4F,$C1      ; "DIKOA"

c_dikoa FDB     do_col                   ; : DIKOA
p_dikoa FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_al                     ; AL
        FDB     c_dotq1                  ; (."F) len=13
        FCB     $0D
        FDB     $5265,$6368,$6572,$6368  ; "Recherch"
        FCB     $65,$20,$64,$65,$20      ; "e de "
        FDB     c_koa                    ; KOA
        FDB     c_semi_                  ; (;)

; ==================================================================
; FEREF
; ==================================================================
l_feref FDB     l_qttil                  ; LFA -> "~

n_feref FCB     $85                      ; NFA -> 5
        FCB     $46,$45,$52,$45,$C6      ; "FEREF"

c_feref FDB     do_col                   ; : FEREF
p_feref FDB     c_cr                     ; CR
        FDB     c_koa                    ; KOA
        FDB     c_2                      ; 2
        FDB     c_sp                     ; SP
        FDB     c_dotq1                  ; (."F) len=9
        FCB     $09
        FDB     $3D3E,$2045,$6372,$616E  ; "=> Ecran"
        FCB     $20                      ; " "
        FDB     c_ecdeb                  ; ECDEB
        FDB     c_at                     ; @
        FDB     c_dup                    ; DUP
        FDB     c_1p,c_lit_              ; 1P
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_14p,c_lit_             ; 14P
        FDB     $002C                    ; 44
        FDB     c_tild                   ; ~
        FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $20,$6C,$69,$67,$6E,$65  ; " ligne"
        FCB     $20                      ; " "
        FDB     c_1p                     ; 1P
        FDB     c_deb                    ; DEB
        FDB     c_sub                    ; -
        FDB     c_cdiv0                  ; C/L0
        FDB     c_div                    ; /
        FDB     c_1add                   ; 1+
        FDB     c_2                      ; 2
        FDB     c_dotr                   ; .R
        FDB     c_14p,c_lit_             ; 14P
        FDB     $002E                    ; 46
        FDB     c_tild                   ; ~
        FDB     c_0                      ; 0
        FDB     c_flg                    ; FLG
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; REVUE
; ==================================================================
l_revue FDB     l_feref                  ; LFA -> FEREF

n_revue FCB     $85                      ; NFA -> 5
        FCB     $52,$45,$56,$55,$C5      ; "REVUE"

c_revue FDB     do_col                   ; : REVUE
p_revue FDB     c_ncu                    ; NCU
        FDB     c_1                      ; 1
        FDB     c_flg                    ; FLG
        FDB     c_exc                    ; !
        FDB     c_memo                   ; MEMO
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_nbcar                  ; NBCAR
        FDB     c_over                   ; OVER
        FDB     c_add                    ; +
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z7EB5              ; (DO) --Z7EB5--v
Z7E93   FDB     c_i                      ; I
        FDB     c_cat                    ; C@
        FDB     c_to_mi                  ; >MIN
        FDB     c_over                   ; OVER
        FDB     c_cat                    ; C@
        FDB     c_to_mi                  ; >MIN
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7EB1            ; ?BRANCH --Z7EB1--v
        FDB     c_i                      ; I
        FDB     c_idem                   ; IDEM
        FDB     c_qmbra,Z7EB1            ; ?BRANCH --Z7EB1--v
        FDB     c_i                      ; I
        FDB     c_feref                  ; FEREF
Z7EB1   FDB     c_loop_,Z7E93            ; (LOOP) --Z7E93--^
Z7EB5   FDB     c_drop                   ; DROP
        FDB     c_11p                    ; 11P
        FDB     c_flg                    ; FLG
        FDB     c_at                     ; @
        FDB     c_qmbra,Z7EE4            ; ?BRANCH --Z7EE4--v
        FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_termi                  ; TERMINAL
        FDB     c_ncu                    ; NCU
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=14
        FCB     $0E
        FDB     $5061,$7320,$7375,$7220  ; "Pas sur "
        FCB     $45,$63,$72,$61,$6E,$20  ; "Ecran "
        FDB     c_ecdeb                  ; ECDEB
        FDB     c_qm                     ; ?
        FDB     c_prnt                   ; PRNT
        FDB     c_exc                    ; !
Z7EE4   FDB     c_semi_                  ; (;)

; ==================================================================
; DANS
; ==================================================================
l_dans  FDB     l_dikoa                  ; LFA -> DIKOA

n_dans  FCB     $84                      ; NFA -> 4
        FCB     $44,$41,$4E,$D3          ; "DANS"

c_dans  FDB     do_col                   ; : DANS
p_dans  FDB     c_cls,c_lit_             ; CLS
        FDB     $0004                    ; 4
        FDB     c_al                     ; AL
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $4D6F,$7420,$8720,$6368  ; "Mot à ch"
        FDB     $6572,$6368,$6572,$3F20  ; "ercher? "
        FDB     c_inmem                  ; INMEMO
        FDB     c_cr                     ; CR
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $4563,$7261,$6E20,$6480  ; "Ecran dé"
        FDB     $7061,$7274,$3F20,$2020  ; "part?   "
        FDB     c_dotin                  ; .IN
        FDB     c_ecdeb                  ; ECDEB
        FDB     c_exc                    ; !
        FDB     c_cr                     ; CR
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $4563,$7261,$6E20,$6C69  ; "Ecran li"
        FDB     $6D69,$7465,$3F20,$2020  ; "mite?   "
        FDB     c_dotin                  ; .IN
        FDB     c_1add                   ; 1+
        FDB     c_ecfin                  ; ECFIN
        FDB     c_exc                    ; !
        FDB     c_2                      ; 2
        FDB     c_al                     ; AL
        FDB     c_1p                     ; 1P
        FDB     c_dotq1                  ; (."F) len=21
        FCB     $15
        FDB     $5265,$746F,$7572,$2070  ; "Retour p"
        FDB     $6F75,$7220,$616E,$6E75  ; "our annu"
        FCB     $6C,$65,$72,$20,$20      ; "ler  "
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=21
        FCB     $15
        FDB     $556E,$6520,$746F,$7563  ; "Une touc"
        FDB     $6865,$2070,$6F75,$7220  ; "he pour "
        FCB     $73,$75,$69,$74,$65      ; "suite"
        FDB     c_14p                    ; 14P
        FDB     c_key,c_lit_             ; KEY
        FDB     $000D                    ; 13
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z7F94            ; ?BRANCH --Z7F94--v
        FDB     c_dans                   ; DANS
        FDB     c_branc,Z7FBC            ; BRANCH --Z7FBC--v
Z7F94   FDB     c_cls                    ; CLS
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=15
        FCB     $0F
        FDB     $5355,$5220,$494D,$5052  ; "SUR IMPR"
        FCB     $49,$4D,$41,$4E,$54,$45  ; "IMANTE"
        FCB     $3F                      ; "?"
        FDB     c_ydivn                  ; Y/N
        FDB     c_qmbra,Z7FB8            ; ?BRANCH --Z7FB8--v
        FDB     c_print                  ; PRINTER
        FDB     c_fr                     ; FR
        FDB     c_branc,Z7FBA            ; BRANCH --Z7FBA--v
Z7FB8   FDB     c_termi                  ; TERMINAL
Z7FBA   FDB     c_dikoa                  ; DIKOA
Z7FBC   FDB     c_ecfin                  ; ECFIN
        FDB     c_at                     ; @
        FDB     c_ecdeb                  ; ECDEB
        FDB     c_at                     ; @
        FDB     c_do_,Z7FD8              ; (DO) --Z7FD8--v
Z7FC8   FDB     c_i                      ; I
        FDB     c_deb                    ; DEB
        FDB     c_revue                  ; REVUE
        FDB     c_1                      ; 1
        FDB     c_ecdeb                  ; ECDEB
        FDB     c_addex                  ; +!
        FDB     c_loop_,Z7FC8            ; (LOOP) --Z7FC8--^
Z7FD8   FDB     c_prnt                   ; PRNT
        FDB     c_at                     ; @
        FDB     c_qmbra,Z7FE8            ; ?BRANCH --Z7FE8--v
        FDB     c_us                     ; US
        FDB     c_bs                     ; BS
        FDB     c_branc,Z7FEC            ; BRANCH --Z7FEC--v
Z7FE8   FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
Z7FEC   FDB     c_semi_                  ; (;)

; ==================================================================
; BANNIERE-PRESENTATION
; ==================================================================
l_banni FDB     l_revue                  ; LFA -> REVUE

n_banni FCB     $95                      ; NFA -> 21
        FDB     $4241,$4E4E,$4945,$5245  ; "BANNIERE"
        FDB     $2D50,$5245,$5345,$4E54  ; "-PRESENT"
        FCB     $41,$54,$49,$4F,$CE      ; "ATION"

c_banni FDB     do_col                   ; : BANNIERE-PRESENTATION
p_banni FDB     c_14p                    ; 14P
        FDB     c_ncu                    ; NCU
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $3D2D,$3D2D,$3D2D,$3D2D  ; "=-=-=-=-"
        FDB     $3D2D,$3D2D,$3D2D,$3D2D  ; "=-=-=-=-"
        FDB     $3D2D,$3D2D,$3D2D,$3D2D  ; "=-=-=-=-"
        FCB     $3D,$2D,$3D,$2D,$3D,$2D  ; "=-=-=-"
        FCB     $3D                      ; "="
        FDB     c_3p                     ; 3P
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $204C,$6520,$466F,$7274  ; " Le Fort"
        FDB     $6820,$7375,$7220,$6C65  ; "h sur le"
        FDB     $7175,$656C,$2076,$6F75  ; "quel vou"
        FCB     $73,$20,$61,$6C,$6C,$65  ; "s alle"
        FCB     $7A                      ; "z"
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $7472,$6176,$6169,$6C6C  ; "travaill"
        FDB     $6572,$2020,$6573,$7420  ; "er  est "
        FDB     $2070,$6172,$616D,$6574  ; " paramet"
        FCB     $72,$80,$20,$70,$6F,$75  ; "ré pou"
        FCB     $72                      ; "r"
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $666F,$6E63,$7469,$6F6E  ; "fonction"
        FDB     $6E65,$7220,$6176,$6563  ; "ner avec"
        FDB     $2064,$6575,$7820,$6472  ; " deux dr"
        FCB     $69,$76,$65,$73,$20,$6F  ; "ives o"
        FCB     $85                      ; "ù"
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $6C65,$7320,$6469,$736B  ; "les disk"
        FDB     $7320,$7365,$726F,$6E74  ; "s seront"
        FDB     $2066,$6F72,$6D61,$7480  ; " formaté"
        FCB     $73,$20,$20,$46,$4C,$45  ; "s  FLE"
        FCB     $58                      ; "X"
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $656E,$2044,$6F75,$626C  ; "en Doubl"
        FDB     $6520,$6661,$6365,$2C20  ; "e face, "
        FDB     $446F,$7562,$6C65,$2064  ; "Double d"
        FCB     $65,$6E,$73,$69,$74,$80  ; "ensité"
        FCB     $2E                      ; "."
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=22
        FCB     $16
        FDB     $2056,$6F75,$7320,$6175  ; " Vous au"
        FDB     $7265,$7A2C,$6175,$2064  ; "rez,au d"
        FCB     $80,$70,$61,$72,$74,$3A  ; "épart:"
        FDB     c_14p                    ; 14P
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $2020,$203D,$4469,$736B  ; "   =Disk"
        FDB     $2053,$7973,$7484,$6D65  ; " Système"
        FDB     $2065,$6E20,$4472,$6976  ; " en Driv"
        FCB     $65,$20,$30,$2C,$20,$20  ; "e 0,  "
        FCB     $20                      ; " "
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=28
        FCB     $1C
        FDB     $2020,$203D,$4469,$736B  ; "   =Disk"
        FDB     $2054,$7261,$7661,$696C  ; " Travail"
        FDB     $2065,$6E20,$4472,$6976  ; " en Driv"
        FCB     $65,$20,$31,$2C          ; "e 1,"
        FDB     c_3p                     ; 3P
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $6176,$6563,$2033,$3220  ; "avec 32 "
        FDB     $7365,$6374,$6575,$7273  ; "secteurs"
        FDB     $2070,$6172,$2070,$6973  ; " par pis"
        FCB     $74,$65,$2E,$20,$20,$20  ; "te.   "
        FCB     $20                      ; " "
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $2050,$6F75,$7220,$7061  ; " Pour pa"
        FDB     $7373,$6572,$2065,$6E20  ; "sser en "
        FDB     $3136,$2073,$6563,$7465  ; "16 secte"
        FCB     $75,$72,$73,$20,$70,$61  ; "urs pa"
        FCB     $72                      ; "r"
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $7069,$7374,$6520,$6661  ; "piste fa"
        FDB     $6972,$6520,$3C20,$5346  ; "ire < SF"
        FDB     $4444,$203E,$2E20,$4F75  ; "DD >. Ou"
        FCB     $20,$20,$66,$61,$69,$72  ; "  fair"
        FCB     $65                      ; "e"
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $3C20,$5346,$5344,$203E  ; "< SFSD >"
        FDB     $2070,$6F75,$7220,$2070  ; " pour  p"
        FDB     $6173,$7365,$7220,$656E  ; "asser en"
        FCB     $20,$53,$69,$6D,$70,$6C  ; " Simpl"
        FCB     $65                      ; "e"
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $6465,$6E73,$6974,$8020  ; "densité "
        FDB     $3130,$2073,$6563,$7465  ; "10 secte"
        FDB     $7572,$7320,$3335,$2070  ; "urs 35 p"
        FCB     $69,$73,$74,$65,$73,$2E  ; "istes."
        FCB     $20                      ; " "
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $204D,$6169,$7320,$6461  ; " Mais da"
        FDB     $6E73,$2063,$6520,$6361  ; "ns ce ca"
        FDB     $7320,$766F,$7573,$206E  ; "s vous n"
        FCB     $27,$20,$61,$75,$72,$65  ; "' aure"
        FCB     $7A                      ; "z"
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $7175,$2775,$6E65,$2066  ; "qu'une f"
        FDB     $6163,$652F,$6469,$736B  ; "ace/disk"
        FDB     $2061,$6363,$6573,$7369  ; " accessi"
        FCB     $62,$6C,$65,$2E,$20,$20  ; "ble.  "
        FCB     $20                      ; " "
        FDB     c_14p                    ; 14P
        FDB     c_semi_                  ; (;)

; ==================================================================
; MESSINIT
; ==================================================================
l_messi FDB     l_inmem                  ; LFA -> INMEMO

n_messi FCB     $88                      ; NFA -> 8
        FDB     $4D45,$5353,$494E,$49D4  ; "MESSINIT"

c_messi FDB     do_col                   ; : MESSINIT
p_messi FDB     c_cls                    ; CLS
        FDB     c_2p                     ; 2P
        FDB     c_date                   ; DATE
        FDB     c_ncu                    ; NCU
        FDB     c_cr                     ; CR
        FDB     c_7p                     ; 7P
        FDB     c_dr1                    ; DR1
        FDB     c_shift                  ; SHIFT
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=27
        FCB     $1B
        FDB     $2046,$4F52,$5448,$2D38  ; " FORTH-8"
        FDB     $3320,$2F20,$466C,$6578  ; "3 / Flex"
        FDB     $392E,$322F,$3835,$2E56  ; "9.2/85.V"
        FCB     $65,$72,$2E              ; "er."
        FDB     c_versi                  ; VERSION
        FDB     c_at                     ; @
        FDB     c_0                      ; 0
        FDB     c_from2                  ; <#
        FDB     c_n_                     ; #
        FDB     c_n_,c_lit_              ; #
        FDB     $002E                    ; 46
        FDB     c_hold                   ; HOLD
        FDB     c_n_s                    ; #S
        FDB     c_n_gt                   ; #>
        FDB     c_type                   ; TYPE
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=25
        FCB     $19
        FDB     $2020,$2020,$2020,$2057  ; "       W"
        FDB     $696C,$736F,$6E20,$4D2E  ; "ilson M."
        FDB     $2046,$4544,$4552,$4943  ; " FEDERIC"
        FCB     $49                      ; "I"
        FDB     c_banni                  ; BANNIERE-PRESENTATION
        FDB     c_6p                     ; 6P
        FDB     c_cr                     ; CR
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=14
        FCB     $0E
        FDB     $5441,$5045,$5220,$3A20  ; "TAPER : "
        FCB     $20,$20,$20,$20,$20,$20  ; "      "
        FDB     c_14p                    ; 14P
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $2D3E,$2075,$6E65,$2074  ; "-> une t"
        FDB     $6F75,$6368,$6520,$2070  ; "ouche  p"
        FDB     $6F75,$7220,$7265,$6461  ; "our reda"
        FCB     $74,$65,$72,$2C,$20,$20  ; "ter,  "
        FCB     $20                      ; " "
        FDB     c_doldo                  ; $$
        FDB     c_dotq1                  ; (."F) len=31
        FCB     $1F
        FDB     $2D3E,$206F,$7520,$6072  ; "-> ou `r"
        FDB     $6574,$7572,$6E27,$2070  ; "eturn' p"
        FDB     $6F75,$7220,$636F,$6E74  ; "our cont"
        FCB     $69,$6E,$75,$65,$72,$2E  ; "inuer."
        FCB     $20                      ; " "
        FDB     c_14p                    ; 14P
        FDB     c_key,c_lit_             ; KEY
        FDB     $000D                    ; 13
        FDB     c_ltgt                   ; <>
        FDB     c_qmbra,Z8330            ; ?BRANCH --Z8330--v
        FDB     c_dodat                  ; DODAT
Z8330   FDB     c_pile                   ; PILE
        FDB     c_ycu                    ; YCU
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; TECRAN
; ==================================================================
l_tecra FDB     l_dans                   ; LFA -> DANS

n_tecra FCB     $86                      ; NFA -> 6
        FCB     $54,$45,$43,$52,$41,$CE  ; "TECRAN"

c_tecra FDB     do_col                   ; : TECRAN
p_tecra FDB     c_scpa,c_lit_            ; SCPA
        FDB     $0009                    ; 9
        FDB     c_pap                    ; PAP
        FDB     c_clc                    ; CLC
        FDB     c_ncu                    ; NCU
        FDB     c_11p                    ; 11P
        FDB     c_deb                    ; DEB
        FDB     c_nbcar                  ; NBCAR
        FDB     c_chain                  ; CHAINE
        FDB     c_semi_                  ; (;)

; ==================================================================
; LIS
; ==================================================================
l_lis   FDB     l_tecra                  ; LFA -> TECRAN

n_lis   FCB     $83                      ; NFA -> 3
        FCB     $4C,$49,$D3              ; "LIS"

c_lis   FDB     do_col                   ; : LIS
p_lis   FDB     c_ncu,c_lit_             ; NCU
        FDB     $0003                    ; 3
        FDB     c_pap                    ; PAP
        FDB     c_home                   ; HOME
        FDB     c_clc,c_lit_             ; CLC
        FDB     $0001,c_lit_             ; 1
        FDB     $0002                    ; 2
        FDB     c_cdiv0,c_lit_           ; C/L0
        FDB     $0019,c_lit_             ; 25
        FDB     $0009,c_lit_             ; 9
        FDB     $000B                    ; 11
        FDB     c_win                    ; WIN
        FDB     c_clc                    ; CLC
        FDB     c_1add                   ; 1+
        FDB     c_swap                   ; SWAP
        FDB     c_do_,Z83B7              ; (DO) --Z83B7--v
Z838F   FDB     c_i                      ; I
        FDB     c_1                      ; 1
        FDB     c_1                      ; 1
        FDB     c_at0,c_lit_             ; AT
        FDB     $0003                    ; 3
        FDB     c_pap                    ; PAP
        FDB     c_1p,c_lit_              ; 1P
        FDB     $0003                    ; 3
        FDB     c_dotr                   ; .R
        FDB     c_1                      ; 1
        FDB     c_2                      ; 2
        FDB     c_at0                    ; AT
        FDB     c_i                      ; I
        FDB     c_tecra                  ; TECRAN
        FDB     c_qmkey                  ; ?KEY
        FDB     c_qmlea                  ; (?LEAVE)
        FDB     c_loop_,Z838F            ; (LOOP) --Z838F--^
Z83B7   FDB     c_scro                   ; SCRO
        FDB     c_1,c_lit_               ; 1
        FDB     $0018                    ; 24
        FDB     c_at0                    ; AT
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; $CHH
; ==================================================================
l_dolch FDB     l_lis                    ; LFA -> LIS

n_dolch FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$C8          ; "$CHH"

c_dolch FDB     do_var                   ; VARIABLE $CHH
p_dolch FDB     $2820                    ; 10272
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; ==================================================================
; $CHM
; ==================================================================
l_dolc0 FDB     l_dolch                  ; LFA -> $CHH

n_dolc0 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$CD          ; "$CHM"

c_dolc0 FDB     do_var                   ; VARIABLE $CHM
p_dolc0 FDB     $2820                    ; 10272
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; ==================================================================
; $CHB
; ==================================================================
l_dolc1 FDB     l_dolc0                  ; LFA -> $CHM

n_dolc1 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$C2          ; "$CHB"

c_dolc1 FDB     do_var                   ; VARIABLE $CHB
p_dolc1 FDB     $2820                    ; 10272
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        FCB     $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; ==================================================================
; $CH1
; ==================================================================
l_dolc2 FDB     l_dolc1                  ; LFA -> $CHB

n_dolc2 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$B1          ; "$CH1"

c_dolc2 FDB     do_var                   ; VARIABLE $CH1
p_dolc2 FDB     $025E                    ; 606
        FCB     $43

; ==================================================================
; $CH5
; ==================================================================
l_dolc3 FDB     l_dolc2                  ; LFA -> $CH1

n_dolc3 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$B5          ; "$CH5"

c_dolc3 FDB     do_var                   ; VARIABLE $CH5
p_dolc3 FDB     $025E                    ; 606
        FCB     $59

; ==================================================================
; $CH3
; ==================================================================
l_dolc4 FDB     l_dolc3                  ; LFA -> $CH5

n_dolc4 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$B3          ; "$CH3"

c_dolc4 FDB     do_var                   ; VARIABLE $CH3
p_dolc4 FDB     $025E                    ; 606
        FCB     $42

; ==================================================================
; $CH7
; ==================================================================
l_dolc5 FDB     l_dolc4                  ; LFA -> $CH3

n_dolc5 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$B7          ; "$CH7"

c_dolc5 FDB     do_var                   ; VARIABLE $CH7
p_dolc5 FDB     $025E                    ; 606
        FCB     $44

; ==================================================================
; $CH2
; ==================================================================
l_dolc6 FDB     l_dolc5                  ; LFA -> $CH7

n_dolc6 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$B2          ; "$CH2"

c_dolc6 FDB     do_var                   ; VARIABLE $CH2
p_dolc6 FDB     $0F53                    ; 3923
        FCB     $61,$75,$74,$20,$87,$20,$67,$61,$75,$63,$68,$65,$20
        FCB     $7C

; ==================================================================
; $CH4
; ==================================================================
l_dolc7 FDB     l_dolc6                  ; LFA -> $CH2

n_dolc7 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$B4          ; "$CH4"

c_dolc7 FDB     do_var                   ; VARIABLE $CH4
p_dolc7 FDB     $0D53                    ; 3411
        FCB     $61,$75,$74,$20,$87,$20,$64,$72,$6F,$69,$74,$65

; ==================================================================
; $CH6
; ==================================================================
l_dolc8 FDB     l_dolc7                  ; LFA -> $CH4

n_dolc8 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$B6          ; "$CH6"

c_dolc8 FDB     do_var                   ; VARIABLE $CH6
p_dolc8 FDB     $0F45                    ; 3909
        FCB     $63,$72,$61,$6E,$3E,$4D,$80,$6D,$6F,$69,$72,$65,$20
        FCB     $7C

; ==================================================================
; $CH8
; ==================================================================
l_dolc9 FDB     l_dolc8                  ; LFA -> $CH6

n_dolc9 FCB     $84                      ; NFA -> 4
        FCB     $24,$43,$48,$B8          ; "$CH8"

c_dolc9 FDB     do_var                   ; VARIABLE $CH8
p_dolc9 FDB     $0D4D                    ; 3405
        FCB     $80,$6D,$6F,$69,$72,$65,$3E,$45,$63,$72,$61,$6E

; ==================================================================
; EFCAD
; ==================================================================
l_efcad FDB     l_messi                  ; LFA -> MESSINIT

n_efcad FCB     $85                      ; NFA -> 5
        FCB     $45,$46,$43,$41,$C4      ; "EFCAD"

c_efcad FDB     do_col                   ; : EFCAD
p_efcad FDB     c_lit_
        FDB     $000E                    ; 14
        FDB     c_col0,c_lit_            ; COL
        FDB     $0001,c_lit_             ; 1
        FDB     $0001,c_lit_             ; 1
        FDB     $00FC,c_lit_             ; 252
        FDB     $0012                    ; 18
        FDB     c_plan                   ; PLAN
        FDB     c_semi_                  ; (;)

; ==================================================================
; &&
; ==================================================================
l_ampam FDB     l_banni                  ; LFA -> BANNIERE-PRESENTATION

n_ampam FCB     $82                      ; NFA -> 2
        FCB     $26,$A6                  ; "&&"

c_ampam FDB     do_col                   ; : &&
p_ampam FDB     c_cls,c_lit_             ; CLS
        FDB     $0005                    ; 5
        FDB     c_al                     ; AL
        FDB     c_dotq1                  ; (."F) len=10
        FCB     $0A
        FDB     $4469,$736B,$203C,$436F  ; "Disk <Co"
        FCB     $75,$72                  ; "ur"
        FDB     c_dotq1                  ; (."F) len=17
        FCB     $11
        FDB     $7269,$6572,$3E20,$656E  ; "rier> en"
        FDB     $2044,$7269,$7665,$2031  ; " Drive 1"
        FCB     $3F                      ; "?"
        FDB     c_ydivn                  ; Y/N
        FDB     c_qmbra,Z8559            ; ?BRANCH --Z8559--v
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=3
        FCB     $03
        FCB     $4F,$6B,$21              ; "Ok!"
        FDB     c_lit_
        FDB     $0006                    ; 6
        FDB     c_load                   ; LOAD
        FDB     c_branc,Z858D            ; BRANCH --Z858D--v
Z8559   FDB     c_lit_
        FDB     $0003                    ; 3
        FDB     c_al                     ; AL
        FDB     c_dotq1                  ; (."F) len=14
        FCB     $0E
        FDB     $4C65,$206D,$6574,$7472  ; "Le mettr"
        FCB     $65,$20,$73,$76,$70,$21  ; "e svp!"
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=24
        FCB     $18
        FDB     $6F75,$2063,$6861,$6E67  ; "ou chang"
        FDB     $6572,$2064,$6520,$6D6F  ; "er de mo"
        FDB     $7421,$2028,$3E54,$5429  ; "t! (>TT)"
Z858D   FDB     c_semi_                  ; (;)

; ==================================================================
; HLP1
; ==================================================================
l_hlp1  FDB     l_dolc9                  ; LFA -> $CH8

n_hlp1  FCB     $84                      ; NFA -> 4
        FCB     $48,$4C,$50,$B1          ; "HLP1"

c_hlp1  FDB     do_col                   ; : HLP1
p_hlp1  FDB     c_efcad                  ; EFCAD
        FDB     c_1                      ; 1
        FDB     c_haut                   ; HAUT
        FDB     c_1                      ; 1
        FDB     c_large                  ; LARGE
        FDB     c_1                      ; 1
        FDB     c_hsubd                  ; H-DROIT
        FDB     c_dolc0,c_lit_           ; $CHM
        FDB     $0006,c_lit_             ; 6
        FDB     $0008,c_lit_             ; 8
        FDB     $0008                    ; 8
        FDB     c_splas                  ; SPLASH
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; HLP2
; ==================================================================
l_hlp2  FDB     l_hlp1                   ; LFA -> HLP1

n_hlp2  FCB     $84                      ; NFA -> 4
        FCB     $48,$4C,$50,$B2          ; "HLP2"

c_hlp2  FDB     do_col                   ; : HLP2
p_hlp2  FDB     c_efcad                  ; EFCAD
        FDB     c_1                      ; 1
        FDB     c_haut                   ; HAUT
        FDB     c_1                      ; 1
        FDB     c_large                  ; LARGE
        FDB     c_1                      ; 1
        FDB     c_hsubd                  ; H-DROIT
        FDB     c_dolch,c_lit_           ; $CHH
        FDB     $0006,c_lit_             ; 6
        FDB     $000B,c_lit_             ; 11
        FDB     $0008                    ; 8
        FDB     c_dolc1,c_lit_           ; $CHB
        FDB     $0006,c_lit_             ; 6
        FDB     $0003,c_lit_             ; 3
        FDB     $0008                    ; 8
        FDB     c_2                      ; 2
        FDB     c_0                      ; 0
        FDB     c_do_,Z85FB              ; (DO) --Z85FB--v
Z85F5   FDB     c_splas                  ; SPLASH
        FDB     c_loop_,Z85F5            ; (LOOP) --Z85F5--^
Z85FB   FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; HLP3
; ==================================================================
l_hlp3  FDB     l_hlp2                   ; LFA -> HLP2

n_hlp3  FCB     $84                      ; NFA -> 4
        FCB     $48,$4C,$50,$B3          ; "HLP3"

c_hlp3  FDB     do_col                   ; : HLP3
p_hlp3  FDB     c_efcad                  ; EFCAD
        FDB     c_1                      ; 1
        FDB     c_haut                   ; HAUT
        FDB     c_1                      ; 1
        FDB     c_large                  ; LARGE
        FDB     c_1                      ; 1
        FDB     c_hsubd                  ; H-DROIT
        FDB     c_dolc9,c_lit_           ; $CH8
        FDB     $0009,c_lit_             ; 9
        FDB     $0002,c_lit_             ; 2
        FDB     $0097                    ; 151
        FDB     c_dolc5,c_lit_           ; $CH7
        FDB     $0002,c_lit_             ; 2
        FDB     $0003,c_lit_             ; 3
        FDB     $0086                    ; 134
        FDB     c_dolc8,c_lit_           ; $CH6
        FDB     $0009,c_lit_             ; 9
        FDB     $0002,c_lit_             ; 2
        FDB     $0028                    ; 40
        FDB     c_dolc3,c_lit_           ; $CH5
        FDB     $0002,c_lit_             ; 2
        FDB     $0003,c_lit_             ; 3
        FDB     $0018                    ; 24
        FDB     c_dolc7,c_lit_           ; $CH4
        FDB     $0009,c_lit_             ; 9
        FDB     $000C,c_lit_             ; 12
        FDB     $0097                    ; 151
        FDB     c_dolc4,c_lit_           ; $CH3
        FDB     $0002,c_lit_             ; 2
        FDB     $000C,c_lit_             ; 12
        FDB     $0086                    ; 134
        FDB     c_dolc6,c_lit_           ; $CH2
        FDB     $0009,c_lit_             ; 9
        FDB     $000C,c_lit_             ; 12
        FDB     $0028                    ; 40
        FDB     c_dolc2,c_lit_           ; $CH1
        FDB     $0002,c_lit_             ; 2
        FDB     $000C,c_lit_             ; 12
        FDB     $0018                    ; 24
        FDB     c_dolda,c_lit_           ; $DAT
        FDB     $000E,c_lit_             ; 14
        FDB     $0008,c_lit_             ; 8
        FDB     $0067,c_lit_             ; 103
        FDB     $0009                    ; 9
        FDB     c_0                      ; 0
        FDB     c_do_,Z86A4              ; (DO) --Z86A4--v
Z869E   FDB     c_splas                  ; SPLASH
        FDB     c_loop_,Z869E            ; (LOOP) --Z869E--^
Z86A4   FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; RTHLP
; ==================================================================
l_rthlp FDB     l_ampam                  ; LFA -> &&

n_rthlp FCB     $85                      ; NFA -> 5
        FCB     $52,$54,$48,$4C,$D0      ; "RTHLP"

c_rthlp FDB     do_col                   ; : RTHLP
p_rthlp FDB     c_1add                   ; 1+
        FDB     c_cr                     ; CR
        FDB     c_ycu                    ; YCU
        FDB     c_cdiv0                  ; C/L0
        FDB     c_expec                  ; EXPECT
        FDB     c_14p                    ; 14P
        FDB     c_semi_                  ; (;)

; ==================================================================
; TOCH1
; ==================================================================
l_toch1 FDB     l_hlp3                   ; LFA -> HLP3

n_toch1 FCB     $85                      ; NFA -> 5
        FCB     $54,$4F,$43,$48,$B1      ; "TOCH1"

c_toch1 FDB     do_col                   ; : TOCH1
p_toch1 FDB     c_cr                     ; CR
        FDB     c_ncu                    ; NCU
        FDB     c_dotq1                  ; (."F) len=26
        FCB     $1A
        FDB     $456E,$7472,$6572,$2061  ; "Entrer a"
        FDB     $7520,$6D61,$7869,$6D75  ; "u maximu"
        FDB     $6D20,$3120,$6C69,$676E  ; "m 1 lign"
        FCB     $65,$20                  ; "e "
        FDB     c_3p                     ; 3P
        FDB     c_dolc0                  ; $CHM
        FDB     c_rthlp                  ; RTHLP
        FDB     c_hlp1                   ; HLP1
        FDB     c_semi_                  ; (;)

; ==================================================================
; TOCH2
; ==================================================================
l_toch2 FDB     l_toch1                  ; LFA -> TOCH1

n_toch2 FCB     $85                      ; NFA -> 5
        FCB     $54,$4F,$43,$48,$B2      ; "TOCH2"

c_toch2 FDB     do_col                   ; : TOCH2
p_toch2 FDB     c_cr                     ; CR
        FDB     c_ncu                    ; NCU
        FDB     c_dotq1                  ; (."F) len=26
        FCB     $1A
        FDB     $456E,$7472,$6572,$2061  ; "Entrer a"
        FDB     $7520,$6D61,$7869,$6D75  ; "u maximu"
        FDB     $6D20,$3120,$6C69,$676E  ; "m 1 lign"
        FCB     $65,$20                  ; "e "
        FDB     c_3p                     ; 3P
        FDB     c_dolch                  ; $CHH
        FDB     c_rthlp                  ; RTHLP
        FDB     c_cr                     ; CR
        FDB     c_ncu                    ; NCU
        FDB     c_dotq1                  ; (."F) len=10
        FCB     $0A
        FDB     $456E,$7472,$6572,$2061  ; "Entrer a"
        FCB     $75,$20                  ; "u "
        FDB     c_dotq1                  ; (."F) len=16
        FCB     $10
        FDB     $6D61,$7869,$6D75,$6D20  ; "maximum "
        FDB     $3120,$6C69,$676E,$6520  ; "1 ligne "
        FDB     c_3p                     ; 3P
        FDB     c_dolc1                  ; $CHB
        FDB     c_rthlp                  ; RTHLP
        FDB     c_semi_                  ; (;)

; ==================================================================
; V-LIG
; ==================================================================
l_vsubl FDB     l_rthlp                  ; LFA -> RTHLP

n_vsubl FCB     $85                      ; NFA -> 5
        FCB     $56,$2D,$4C,$49,$C7      ; "V-LIG"

c_vsubl FDB     do_var                   ; VARIABLE V-LIG
p_vsubl FDB     $0000                    ; 0

; ==================================================================
; SORPILE
; ==================================================================
l_sorpi FDB     l_koa                    ; LFA -> KOA

n_sorpi FCB     $87                      ; NFA -> 7
        FCB     $53,$4F,$52,$50,$49,$4C  ; "SORPIL"
        FCB     $C5                      ; "E"

c_sorpi FDB     do_col                   ; : SORPILE
p_sorpi FDB     c_lit_
        FDB     $0002,c_lit_             ; 2
        FDB     $0006,c_lit_             ; 6
        FDB     $0027,c_lit_             ; ' PAUSE
        FDB     $0007,c_lit_             ; 7
        FDB     $0005,c_lit_             ; 5
        FDB     $000E                    ; 14
        FDB     c_win                    ; WIN
        FDB     c_scpa                   ; SCPA
        FDB     c_clc                    ; CLC
        FDB     c_divdi                  ; //
        FDB     c_9p,c_lit_              ; 9P
        FDB     $0001,c_lit_             ; 1
        FDB     $000A,c_lit_             ; 10
        FDB     $0028,c_lit_             ; 40
        FDB     $0019,c_lit_             ; 25
        FDB     $0009,c_lit_             ; 9
        FDB     $000E                    ; 14
        FDB     c_win                    ; WIN
        FDB     c_ncu                    ; NCU
        FDB     c_1                      ; 1
        FDB     c_vsubl                  ; V-LIG
        FDB     c_addex                  ; +!
        FDB     c_vsubl                  ; V-LIG
        FDB     c_at                     ; @
        FDB     c_1add                   ; 1+
        FDB     c_al                     ; AL
        FDB     c_cel                    ; CEL
        FDB     c_1                      ; 1
        FDB     c_subal                  ; -AL
        FDB     c_14p                    ; 14P
        FDB     c_ycu                    ; YCU
        FDB     c_semi_                  ; (;)

; ==================================================================
; RESTITUE
; ==================================================================
l_resti FDB     l_vsubl                  ; LFA -> V-LIG

n_resti FCB     $88                      ; NFA -> 8
        FDB     $5245,$5354,$4954,$55C5  ; "RESTITUE"

c_resti FDB     do_col                   ; : RESTITUE
p_resti FDB     c_lit_
        FDB     $1BCA,c_lit_             ; ' TERMINAL
        FDB     $10F9                    ; ' QUIT
        FDB     c_to_bo                  ; >BODY
        FDB     c_2add                   ; 2+
        FDB     c_exc,c_lit_             ; !
        FDB     $0A6C,c_lit_             ; ' [
        FDB     $10F9                    ; ' QUIT
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc,c_lit_             ; !
        FDB     $0F3F,c_lit_             ; ' BL
        FDB     $0FFA                    ; ' INTERPRET
        FDB     c_to_bo                  ; >BODY
        FDB     c_exc,c_lit_             ; !
        FDB     $0CE2,c_lit_             ; ' WORD
        FDB     $0FFA                    ; ' INTERPRET
        FDB     c_to_bo                  ; >BODY
        FDB     c_2add                   ; 2+
        FDB     c_exc                    ; !
        FDB     c_semi_                  ; (;)

; ==================================================================
; RESTORE
; ==================================================================
l_resto FDB     l_resti                  ; LFA -> RESTITUE

n_resto FCB     $87                      ; NFA -> 7
        FCB     $52,$45,$53,$54,$4F,$52  ; "RESTOR"
        FCB     $C5                      ; "E"

c_resto FDB     do_col                   ; : RESTORE
p_resto FDB     c_resti                  ; RESTITUE
        FDB     c_cls                    ; CLS
        FDB     c_dotq1                  ; (."F) len=3
        FCB     $03
        FCB     $20,$6F,$6B              ; " ok"
        FDB     c_home                   ; HOME
        FDB     c_scro                   ; SCRO
        FDB     c_ycu                    ; YCU
        FDB     c_quit                   ; QUIT
        FDB     c_semi_                  ; (;)

; ==================================================================
; EXEC
; ==================================================================
l_exec  FDB     l_efcad                  ; LFA -> EFCAD

n_exec  FCB     $84                      ; NFA -> 4
        FCB     $45,$58,$45,$C3          ; "EXEC"

c_exec  FDB     do_col                   ; : EXEC
p_exec  FDB     c_dup,c_lit_             ; DUP
        FDB     $5E58                    ; ' ERE
        FDB     c_eq                     ; =
        FDB     c_over,c_lit_            ; OVER
        FDB     $5E9B                    ; ' EWR
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_over,c_lit_            ; OVER
        FDB     $5E24                    ; ' RE
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_over,c_lit_            ; OVER
        FDB     $5E67                    ; ' WW
        FDB     c_eq                     ; =
        FDB     c_or                     ; OR
        FDB     c_qmbra,Z8860            ; ?BRANCH --Z8860--v
        FDB     c_resti                  ; RESTITUE
        FDB     c_execu                  ; EXECUTE
        FDB     c_branc,Z8864            ; BRANCH --Z8864--v
Z8860   FDB     c_execu                  ; EXECUTE
        FDB     c_sorpi                  ; SORPILE
Z8864   FDB     c_semi_                  ; (;)

; ==================================================================
; LEGENDE
; ==================================================================
l_legen FDB     l_toch2                  ; LFA -> TOCH2

n_legen FCB     $87                      ; NFA -> 7
        FCB     $4C,$45,$47,$45,$4E,$44  ; "LEGEND"
        FCB     $C5                      ; "E"

c_legen FDB     do_col                   ; : LEGENDE
p_legen FDB     c_lit_
        FDB     $000A,c_lit_             ; 10
        FDB     $00D2,c_lit_             ; 210
        FDB     $00EB,c_lit_             ; ' >R
        FDB     $0029,c_lit_             ; 41
        FDB     $0005                    ; 5
        FDB     c_col0                   ; COL
        FDB     c_cadre                  ; CADRE
        FDB     c_2                      ; 2
        FDB     c_1,c_lit_               ; 1
        FDB     $0027,c_lit_             ; ' PAUSE
        FDB     $0004,c_lit_             ; 4
        FDB     $0003,c_lit_             ; 3
        FDB     $0005                    ; 5
        FDB     c_win                    ; WIN
        FDB     c_clc                    ; CLC
        FDB     c_scpa                   ; SCPA
        FDB     c_ncu,c_lit_             ; NCU
        FDB     $0003                    ; 3
        FDB     c_sp                     ; SP
        FDB     c_dotq1                  ; (."F) len=32
        FCB     $20
        FDB     $3D3D,$3D3D,$3D3D,$3D3D  ; "========"
        FDB     $3D3D,$204D,$4F44,$453A  ; "== MODE:"
        FDB     $2053,$5445,$5020,$3D3D  ; " STEP =="
        FDB     $3D3D,$3D3D,$3D3D,$3D3D  ; "========"
        FDB     c_cr                     ; CR
        FDB     c_2                      ; 2
        FDB     c_sp                     ; SP
        FDB     c_dotq1                  ; (."F) len=34
        FCB     $22
        FDB     $4576,$6F6C,$7574,$696F  ; "Evolutio"
        FDB     $6E20,$6465,$206C,$6120  ; "n de la "
        FDB     $7069,$6C65,$2087,$206C  ; "pile à l"
        FDB     $2765,$7880,$6375,$7469  ; "'exécuti"
        FCB     $6F,$6E                  ; "on"
        FDB     c_cr                     ; CR
        FDB     c_dotq1                  ; (."F) len=30
        FCB     $1E
        FDB     $2052,$6170,$7065,$6C3A  ; " Rappel:"
        FDB     $204F,$6E20,$736F,$7274  ; " On sort"
        FDB     $2064,$6520,$5354,$4550  ; " de STEP"
        FCB     $20,$70,$61,$72,$3A,$20  ; " par: "
        FDB     c_dotq1                  ; (."F) len=7
        FCB     $07
        FCB     $52,$45,$53,$54,$4F,$52  ; "RESTOR"
        FCB     $45                      ; "E"
        FDB     c_lit_
        FDB     $0009                    ; 9
        FDB     c_pap                    ; PAP
        FDB     c_9p                     ; 9P
        FDB     c_home                   ; HOME
        FDB     c_semi_                  ; (;)

; ==================================================================
; -OK
; ==================================================================
l_subok FDB     l_exec                   ; LFA -> EXEC

n_subok FCB     $83                      ; NFA -> 3
        FCB     $2D,$4F,$CB              ; "-OK"

c_subok FDB     do_col                   ; : -OK
p_subok FDB     c_lsq                    ; [
        FDB     c_termi                  ; TERMINAL
Z893F   FDB     c_rpexc                  ; RP!
        FDB     c_s0                     ; S0
        FDB     c_at                     ; @
        FDB     c_2add                   ; 2+
        FDB     c_to_ti                  ; >TIB
        FDB     c_exc                    ; !
        FDB     c_tckpr                  ; 'PROMPT
        FDB     c_at                     ; @
        FDB     c_execu                  ; EXECUTE
        FDB     c_query                  ; QUERY
        FDB     c_inter                  ; INTERPRET
        FDB     c_termi                  ; TERMINAL
        FDB     c_branc,Z893F            ; BRANCH --Z893F--^
        FDB     c_semi_                  ; (;)

; ==================================================================
; `STEP
; ==================================================================
l_bqste FDB     l_legen                  ; LFA -> LEGENDE

n_bqste FCB     $85                      ; NFA -> 5
        FCB     $60,$53,$54,$45,$D0      ; "`STEP"

c_bqste FDB     do_col                   ; : `STEP
p_bqste FDB     c_bl                     ; BL
        FDB     c_word                   ; WORD
        FDB     c_dup                    ; DUP
        FDB     c_cat                    ; C@
        FDB     c_qmbra,Z89CF            ; ?BRANCH --Z89CF--v
        FDB     c_ncu                    ; NCU
        FDB     c_find                   ; FIND
        FDB     c_qmdup                  ; ?DUP
        FDB     c_qmbra,Z8993            ; ?BRANCH --Z8993--v
        FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_eq                     ; =
        FDB     c_qmbra,Z898D            ; ?BRANCH --Z898D--v
        FDB     c_kom                    ; ,
        FDB     c_branc,Z898F            ; BRANCH --Z898F--v
Z898D   FDB     c_exec                   ; EXEC
Z898F   FDB     c_branc,Z89C9            ; BRANCH --Z89C9--v
Z8993   FDB     c_numbe                  ; NUMBER
        FDB     c_dpl                    ; DPL
        FDB     c_at                     ; @
        FDB     c_1add                   ; 1+
        FDB     c_state                  ; STATE
        FDB     c_at                     ; @
        FDB     c_qmbra,Z89B7            ; ?BRANCH --Z89B7--v
        FDB     c_qmbra,Z89AF            ; ?BRANCH --Z89AF--v
        FDB     c_swap                   ; SWAP
        FDB     c_liter                  ; LITERAL
        FDB     c_branc,Z89B1            ; BRANCH --Z89B1--v
Z89AF   FDB     c_drop                   ; DROP
Z89B1   FDB     c_liter                  ; LITERAL
        FDB     c_branc,Z89C9            ; BRANCH --Z89C9--v
Z89B7   FDB     c_0eq                    ; 0=
        FDB     c_qmbra,Z89C7            ; ?BRANCH --Z89C7--v
        FDB     c_space                  ; SPACE
        FDB     c_drop,c_lit_            ; DROP
        FDB     $005F                    ; 95
        FDB     c_tild                   ; ~
Z89C7   FDB     c_sorpi                  ; SORPILE
Z89C9   FDB     c_qmsta                  ; ?STACK
        FDB     c_branc,p_bqste          ; BRANCH --p_bqste--^
Z89CF   FDB     c_drop                   ; DROP
        FDB     c_semi_                  ; (;)

; ==================================================================
; STEP
; ==================================================================
l_step  FDB     l_sorpi                  ; LFA -> SORPILE

n_step  FCB     $84                      ; NFA -> 4
        FCB     $53,$54,$45,$D0          ; "STEP"

c_step  FDB     do_col                   ; : STEP
p_step  FDB     c_cls,c_lit_             ; CLS
        FDB     $8965,c_lit_             ; ' `STEP
        FDB     $0FFA                    ; ' INTERPRET
        FDB     c_chang                  ; CHANGEBODY
        FDB     c_ncu,c_lit_             ; NCU
        FDB     $8939,c_lit_             ; ' -OK
        FDB     $10F9                    ; ' QUIT
        FDB     c_chang                  ; CHANGEBODY
        FDB     c_0                      ; 0
        FDB     c_vsubl                  ; V-LIG
        FDB     c_exc                    ; !
        FDB     c_legen                  ; LEGENDE
        FDB     c_pile                   ; PILE
        FDB     c_sorpi                  ; SORPILE
        FDB     c_ycu                    ; YCU
        FDB     c_subok                  ; -OK
        FDB     c_semi_                  ; (;)

; ==================================================================
; >TL
; ==================================================================
l_to_tl FDB     l_resto                  ; LFA -> RESTORE

n_to_tl FCB     $83                      ; NFA -> 3
        FCB     $3E,$54,$CC              ; ">TL"

c_to_tl FDB     do_col                   ; : >TL
p_to_tl FDB     c_ampam                  ; &&
        FDB     c_semi_                  ; (;)

; ==================================================================
; >EDPT
; ==================================================================
l_to_ed FDB     l_to_tl                  ; LFA -> >TL

n_to_ed FCB     $85                      ; NFA -> 5
        FCB     $3E,$45,$44,$50,$D4      ; ">EDPT"

c_to_ed FDB     do_col                   ; : >EDPT
p_to_ed FDB     c_lit_
        FDB     $0047                    ; 71
        FDB     c_load                   ; LOAD
        FDB     c_semi_                  ; (;)

; ==================================================================
; >MOVETO
; ==================================================================
l_to_mo FDB     l_to_ed                  ; LFA -> >EDPT

n_to_mo FCB     $87                      ; NFA -> 7
        FCB     $3E,$4D,$4F,$56,$45,$54  ; ">MOVET"
        FCB     $CF                      ; "O"

c_to_mo FDB     do_col                   ; : >MOVETO
p_to_mo FDB     c_lit_
        FDB     $005B                    ; 91
        FDB     c_load                   ; LOAD
        FDB     c_semi_                  ; (;)

; ==================================================================
; >TT
; ==================================================================
l_to_tt FDB     l_to_mo                  ; LFA -> >MOVETO

n_to_tt FCB     $83                      ; NFA -> 3
        FCB     $3E,$54,$D4              ; ">TT"

c_to_tt FDB     do_col                   ; : >TT
p_to_tt FDB     c_lit_
        FDB     $006B                    ; 107
        FDB     c_load                   ; LOAD
        FDB     c_semi_                  ; (;)

; ==================================================================
; >TRI
; ==================================================================
l_to_tr FDB     l_to_tt                  ; LFA -> >TT

n_to_tr FCB     $84                      ; NFA -> 4
        FCB     $3E,$54,$52,$C9          ; ">TRI"

c_to_tr FDB     do_col                   ; : >TRI
p_to_tr FDB     c_lit_
        FDB     $007C                    ; ' !
        FDB     c_load                   ; LOAD
        FDB     c_semi_                  ; (;)

; ==================================================================
; >TEMP
; ==================================================================
l_to_te FDB     l_to_tr                  ; LFA -> >TRI

n_to_te FCB     $85                      ; NFA -> 5
        FCB     $3E,$54,$45,$4D,$D0      ; ">TEMP"

c_to_te FDB     do_col                   ; : >TEMP
p_to_te FDB     c_lit_
        FDB     $008A                    ; 138
        FDB     c_load                   ; LOAD
        FDB     c_semi_                  ; (;)

; ==================================================================
; >WORDS
; ==================================================================
l_to_wo FDB     l_to_te                  ; LFA -> >TEMP

n_to_wo FCB     $86                      ; NFA -> 6
        FCB     $3E,$57,$4F,$52,$44,$D3  ; ">WORDS"

c_to_wo FDB     do_col                   ; : >WORDS
p_to_wo FDB     c_lit_
        FDB     $0092                    ; 146
        FDB     c_load                   ; LOAD
        FDB     c_semi_                  ; (;)

; ==================================================================
; >DDD
; ==================================================================
l_to_dd FDB     l_to_wo                  ; LFA -> >WORDS

n_to_dd FCB     $84                      ; NFA -> 4
        FCB     $3E,$44,$44,$C4          ; ">DDD"

c_to_dd FDB     do_col                   ; : >DDD
p_to_dd FDB     c_esubt                  ; E-T
        FDB     c_semi_                  ; (;)

; ==================================================================
; TASK
; ==================================================================
l_task  FDB     l_bqste                  ; LFA -> `STEP

n_task  FCB     $84                      ; NFA -> 4
        FCB     $54,$41,$53,$CB          ; "TASK"

c_task  FDB     do_col                   ; : TASK
        FDB     c_semi_                  ; (;)

        END
