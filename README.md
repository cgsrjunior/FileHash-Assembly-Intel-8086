# FileHash-Assembly-Intel-8086
This repository contains an implementation made into intel 8086 assembly using TASM mounting tool for generate hash given the file input through command line

If you need to debug the program, i'll suggest to make the basic procedure of compilation

```Assembly
MASM FILEHASH.ASM
LINK FILEHASH.OBJ
```

After finish the compilation, create a .bat with this commands and run so you can load the program into CodeViewer and check step-by-step execution where a input file is given

```Assembly
RESCAN
MASM FILEHASH.ASM
LINK FILEHASH.OBJ
ML -Zi FILEHASH.ASM
CV FILEHASH.EXE -a caso1.txt -v A
```
