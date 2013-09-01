      *> ***************************************************************
      *> ** Author: Gary L. Cutler                                    **
      *> **         CutlerGL@gmail.com                                **
      *> **                                                           **
      *> ** This copybook defines the control block required as an    **
      *> ** argument to the STREAMIO subroutine (STREAMIO.cbl).  The  **
      *> ** comments in that program describe the function of the     **
      *> ** STREAMIO package.                                         **
      *> ***************************************************************
       01  Streamio-CB.
           05 SCB-Handle                        PIC X(4) COMP-X.
           05 SCB-Mode                          PIC X(1).
              88 Streamio-MODE-Input            VALUE 'I', 'i'.
              88 Streamio-MODE-Output           VALUE 'O', 'o'.
              88 Streamio-MODE-Both             VALUE 'B', 'b'.
           05 SCB-Function                      PIC X(2).
              88 Streamio-FUNC-CLOSE            VALUE 'C ', 'c '.
              88 Streamio-FUNC-DELETE           VALUE 'D ', 'd '.
              88 Streamio-FUNC-OPEN             VALUE 'O ', 'o '.
              88 Streamio-FUNC-READ             VALUE 'R ', 'r '.
              88 Streamio-FUNC-READ-Delimited   VALUE 'RD', 'rd',
                                                      'rD', 'Rd'.
              88 Streamio-FUNC-WRITE            VALUE 'W ', 'w '.
              88 Streamio-FUNC-WRITE-Delimited  VALUE 'WD', 'wd',
                                                      'wD', 'Wd'.
           05 SCB-Delimiter-Mode                PIC X(1).
              88 Streamio-DELIM-Unix            VALUE 'U', 'u'.
              88 Streamio-DELIM-Windows         VALUE 'W', 'w'.
           05 SCB-Offset                        PIC X(8) COMP-X.
           05 SCB-Error-Routine                 USAGE PROGRAM-POINTER.
           05 SCB-Error-Routine-Num REDEFINES SCB-Error-Routine
                                                USAGE BINARY-LONG.
           05 SCB-Return-Code                   USAGE BINARY-LONG.
           05 SCB-Filename                      PIC X(256).
