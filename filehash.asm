;;;;;;;;;;;;;;;   TODOS
;;;;;;;;;;;;;;    1 - Parse CLI arguments
;;;;;;;;;;;;;;    2 - Validate CLI arguments
;;;;;;;;;;;;;;    3 - Open file to read
;;;;;;;;;;;;;;    4 - Generate file verification code
;;;;;;;;;;;;;;    5 - Check file verification code
;;;;;;;;;;;;;;    6 - 


.model small
.stack 1024
.data
    CR		equ		0dh
    LF		equ		0ah
	BreakLine			db	CR, LF, 0
	MsgErroOpenFile		db	"Erro na abertura do arquivo.", CR, LF, 0
    MsgErrorReadFile    db  "Error while reading file.", CR, LF, 0
	MsgEqualHashes      db  "The hashes are equal.", CR, LF, 0
	MsgNotEqualHashes   db  "The hashes aren`t equal.", CR, LF, 0
    MENSAGEM DB 'Hello World! ',0DH,0Ah
    TAMANHO EQU $-MENSAGEM
    CONTADOR DB ?
    args DB 30 DUP(0)  ; define buffer for first cli argument
    len_args DB 0;4
	
	error_read_flag		db		0

	filename 		    db		30 dup (0)
    file_handle         dw      00h
	hash_parameter 	    db		30 dup (0)
	hash_parameter_flag db		0		;; We need a flag to indicate the option
	hash_equals_flag	db		0		;; Flag to confirm the compare result operation
	hash_number_cli	    dw      4 dup (0)
	hash_index		    dw		0
    file_content_len    dw      0
    converted_hash      db      16 dup(0)
    reserved_zero       dw      0
    glob                db      'U'
    hash                db      8 dup (0)
    hash_pointer        dw      0
    file_buffer         DB      512 dup (0)
	
	

.code
    .startup
		;;Reset flag at beginning of the code
		MOV hash_parameter_flag, 0
		MOV hash_equals_flag, 0
        CALL load_cli_args
        CALL parse_args
		; Execute procedure to read the file
		;if (fopen(FileNameSrc)) {
		;	printf("Erro na abertura do arquivo.\r\n")
		;	exit(1)
		;}
		;FileHandleSrc = BX
		lea		dx,filename
		call	fopen
		cmp		error_read_flag, 0
		jne		end_main
        call    calculate_hash_32
        call    hex_to_ascii
        LEA     BX,converted_hash
        CALL    printf_s
		;; Used to generate a CR LF
		LEA     BX,BreakLine
        CALL    printf_s
		;; Check if the user request the -v option
		CMP 	hash_parameter_flag, 0
		JE		end_main
		CALL	compare_hashes
		CMP		hash_equals_flag, 0
		JE		hashes_not_equal
		hashes_are_equal:
			LEA     BX,MsgEqualHashes
			CALL    printf_s
			JMP		end_main
		hashes_not_equal:
			LEA     BX,MsgNotEqualHashes
			CALL    printf_s			
        JMP     end_main 
            

        end_main:
			call    fclose
            NOP
    .exit 0 ; Retorna ao DOS

    calculate_hash_32 PROC NEAR
            MOV SI, 0
            MOV CX,0
        sum_loop:
            CALL read_byte
            CMP AX,0
            je end_sum_loop
            add cl, [file_buffer]
            mov [hash+7],cl
            adc [hash+6],0
            adc [hash+5],0
            adc [hash+4],0
            adc [hash+3],0
            adc [hash+2],0
            adc [hash+1],0
            adc [hash],0
            inc si
            JMP sum_loop
        end_sum_loop:
            RET
    calculate_hash_32 ENDP

    hex_to_ascii PROC NEAR
        MOV SI,8
        MOV DI,0
        move_needle:
            DEC SI
            MOV AL,[hash+SI]
            CMP AL,0
            JA move_needle
        INC SI
        hex_loop:
            MOV AX,0
            MOV BL,[hash+SI]
            CMP BL,0
            JE  end_hex_loop
            MOV AL,BL
            AND AL,0Fh
            MOV AH,BL
            AND AH,0F0h
            ROR AH,1
            ROR AH,1
            ROR AH,1
            ROR AH,1
            CMP AL,09h
            JG  offset_letter_l
            JMP offset_num_l
        convert_high_byte:
            CMP AH,09h
            JG  offset_letter_h

        offset_num_h:
            ADD AH,'0'
            JMP save_bytes
        offset_letter_h:
            SUB AH,10
            ADD AH,'A'
            JMP save_bytes
        offset_num_l:
            ADD AL,'0'
            JMP convert_high_byte
        offset_letter_l:
            SUB AL,10
            ADD AL,'A'
            JMP convert_high_byte
            

        save_bytes:
            MOV [converted_hash+DI+1],AL
            MOV [converted_hash+DI],AH
            INC SI
            ADD DI,2
            JMP hex_loop
        end_hex_loop:
            RET
    hex_to_ascii ENDP
	
	compare_hashes PROC NEAR
	;; Load both addresses
		MOV SI, 0
		;;LEA AX, converted_hash
		;;LEA BX, hash_parameter
	;	While (*s1 !='\0' && *s2 !='\0')
	while_hash:
		LEA		BX, converted_hash
        MOV		DL,[BX+SI]
        cmp		dl,0
        je		ch_adj
		LEA		BX, hash_parameter
		MOV		AL,[BX+SI]
		CMP		AL, 0
		je      ch_1

    ;; if(*s1 == *s2){
	;; 		continue
	;;	}
	;;	else
	;;		return
		CMP AL, DL
		JNE end_compare_hashes
    ;		++s1, ++s2;
        INC		SI
            
    ;	}
        JMP		while_hash
		
	;; In case of the first parameter found \0
	;; We need to adjust the second parameter to be on same page
	ch_adj:
		LEA		BX, hash_parameter
		MOV		AL,[BX+SI]
            
    ch_1:
		CMP AL, DL
		JNE  end_compare_hashes
		INC hash_equals_flag
	end_compare_hashes:
        RET
	compare_hashes ENDP


    ; calculate_hash PROC NEAR
    ;         LEA BX, file_buffer
    ;         MOV CX,[file_content_len]
    ;     calc_loop:
    ;         MOV AX, [BX]
    ;         ADD hash, AX
    ;         ADC hash+2,0
    ;         ADC hash+4,0
    ;         ADC hash+6,0
    ;         ADD BX,2
    ;         DEC CX
    ;         LOOP calc_loop
    ;         RET
    ; calculate_hash ENDP

    read_byte PROC NEAR
        PUSH CX
        read_file_content:
            LEA DX,file_buffer
            MOV BX,[file_handle]
            MOV AH,3fh
            MOV CX, 1
            INT 21h
            JC  error_read
            JMP end_read_file

        error_read:
            mov ax, 4C01h
            INT 21h
            LEA BX,MsgErrorReadFile
            CALL printf_s
            HLT

        end_read_file:
            POP CX
            RET

    read_byte ENDP

    ;
    ;--------------------------------------------------------------------
    ;Function: Write a string to stdout
    ;
    ;void printf_s(char *s -> BX, int w -> CX) {
    ;	While (*s!='\0' && w != 0) {
    ;		putchar(*s)
    ; 		++s;
    ;	}
    ;}
    ;--------------------------------------------------------------------
    printf_s	PROC NEAR

    ;	While (*s!='\0') {
        MOV		DL,[BX]
        cmp		dl,0
        je		ps_1

    ;		putchar(*s)
        push	bx
        mov		ah,2
        int		21H
        pop		bx

    ;		++s;
        inc		bx
            
    ;	}
        jmp		printf_s
            
    ps_1:
        ret
        
    printf_s	ENDP

    load_cli_args PROC NEAR
        push ds        ; Save segment registers
        push es

        mov ax,ds      ; swap DS and ES 
        mov bx,es
        mov ds,bx
        mov es,ax

        mov si,80h     ; GET cli args size
        MOV len_args,80h
        mov ch,0
        mov cl,[si]

        mov si,81h     ; init cli pointer
        lea di,args     ; init destination pointer
        rep movsb

        pop es         ; recover original segment registers data
        pop ds
        RET
    load_cli_args ENDP

    parse_args PROC NEAR
            LEA SI, args
        read_arg_chars:
            LODSB
            CMP AL,0h
            JE  end_parse_args
            CMP AL, ' '
            JE  read_arg_chars
            CMP AL, '-'
            JE  read_arg_chars
            CMP AL, 'a'
            JE  get_file_name
            CMP AL, 'g'
            JE  generate_hash
            CMP AL, 'v'
            JE  verify_hash
        end_parse_args:
            RET

        get_file_name:
            INC SI
            MOV BX, SI
            CALL save_filename
            JMP read_arg_chars

        generate_hash:
			; We need to fopen the file
            JMP read_arg_chars

        verify_hash:
            ;; We need to create a routine to store the hash number informed by user
			INC SI
            MOV BX, SI
            CALL save_hash_input
			INC hash_parameter_flag  ;;Turn on the flag to communicate the main routine
            JMP read_arg_chars

    parse_args ENDP
	
	save_filename PROC NEAR
		lea bx, filename
		read_filename:
            LODSB
            CMP AL, ' '
            JE  end_save_filename
			; Store a character and increase the pointer to store the next char
			MOV [bx], AL
			INC bx
            JMP read_filename
        end_save_filename:
            RET
	save_filename ENDP
	
	save_hash_input PROC NEAR
		lea bx, hash_parameter
		read_hash_cli:
            LODSB
            CMP AL, ' '
            JE  end_save_hash
			CMP AL, 0
            JE  end_save_hash
			; Store a character and increase the pointer to store the next char
			MOV [bx], AL
			INC bx
            JMP read_hash_cli
        end_save_hash:
            RET
	save_hash_input ENDP
	
	store_hash_int PROC NEAR
		mov di, hash_index
		lea bx, [hash_number_cli+di]
		; Store the data into the current position and adjust the index
		mov bx, ax
		inc di
		mov hash_index, di
		; If the index value is more than 3, we need to reset the variable
		cmp hash_index, 3
		jne end_store_hash
		mov hash_index, 0
		end_store_hash:
            RET
	store_hash_int ENDP
	
	
;--------------------------------------------------------------------
;Função:Converte um ASCII-DECIMAL para HEXA
;Entra: (S) -> DS:BX -> Ponteiro para o string de origem
;Sai:	(A) -> AX -> Valor "Hex" resultante
;Algoritmo:
;	A = 0;
;	while (*S!='\0') {
;		A = 10 * A + (*S - '0')
;		++S;
;	}
;	return
;--------------------------------------------------------------------
	atoi	proc near

		; A = 0;
		mov		ax,0
		
	atoi_2:
		; while (*S!='\0') {
		cmp		byte ptr[bx], 0
		jz		atoi_1

		; 	A = 10 * A
		mov		cx,10
		mul		cx

		; 	A = A + *S
		mov		ch,0
		mov		cl,[bx]
		add		ax,cx

		; 	A = A - '0'
		sub		ax,'0'

		; 	++S
		inc		bx
		
		;}
		jmp		atoi_2

	atoi_1:
		; return
		ret
		
	atoi	endp
	
;--------------------------------------------------------------------
;Função	Abre o arquivo cujo nome está no string apontado por DX
;		boolean fopen(char *FileName -> DX)
;Entra: DX -> ponteiro para o string com o nome do arquivo
;Sai:   BX -> handle do arquivo
;       CF -> 0, se OK
;--------------------------------------------------------------------
fopen	proc	near
        mov		al,0
        mov		ah,3dh
        int		21h
        mov		bx,ax
        mov		file_handle,bx
        jnc     end_fopen
        lea		bx, MsgErroOpenFile
        call	printf_s
		inc		error_read_flag
    end_fopen:
        ret
fopen	endp


fclose  proc  near
    mov ah, 3Eh
    mov bx, [file_handle]
    int 21h
    mov ax, 4C00h
    int 21h
    RET
fclose ENDP
END
